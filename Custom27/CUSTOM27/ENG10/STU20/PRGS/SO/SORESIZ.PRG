*:************************************************************************************
*: Program file       : SORESIZE.PRG
*: Program desc.      : Sales Order Resizing
*: System             : Aria Advantage Series.
*: Module             : Sales Order (SO)
*: Date               : 10/03/2002
*: Developer          : Abdou Elgendy [Abd]
*: Tracking Job Number: C200398,4
*:************************************************************************************
*: Calls :
*:       : Functions       : lfOrdBrow  , lfOpenfls   , lfCrtTmpFil, lfVPoLnHdr
*:       :                 : lfvOrder   , lpSavScr    , lpShow     , lfRestVar
*:       :                 : lfUpdTmp   , lfvQty      , lfDispBrow , lfwBrows
*:       :                 : lfvBrows   , lfBrwUnTrp  , lfBrowTrap , lfTraps
*:       :                 : lfClrStyls , lfUpVarbls  , lfvChkAdd  , lfGetDisPr
*:       :                 : lfEditAlo   , lfGetPOLn  , lfCrUnSess , lfUpdVars   
*:       :                 : lfvEquCost.
*:       : --------------------------------------------------------------------------
*:       : Procedures      : lpDefnFlds , lpDetEsc
*:************************************************************************************
*: Passed Parameters  : None
*:************************************************************************************
*: Example            : DO SORESIZE.PRG
*:************************************************************************************
*: Note               : This programs calls SOUPDATE.PRG to save a new SO.
*:************************************************************************************
*: Modifications ..
*: C200398,4 ABD 10/16/2002 Add the browse while the program in the test.
*: B606773,1 ABD 12/29/2002 Fix problem in the resize screen in case update many
*: B606773,1 ABD            orders at the same session.
**************************************************************************************
*:
EXTERNAL ARRAY laData , laKeyField

DECLARE laKeyField[2,4]
STORE .F. TO llBrowse   , llMulCurr , llIgnorAll , llUpdTmpAl , llAddNewRe , llMultiWare
STORE ''  TO lcScFields , lcActName , lcStyMaj , lcStyDesc , lcModal , lcStySek ,;
             lcColour   , lcSz1     , lcSz2    , lcSz3     , lcSz4   , lcSz5    ,;
             lcSz6      , lcSz7     , lcSz8    , lcFit     , lcAlocated , lcTempAloc,;
             lcPOHdr    , lcPOLine  , lcPMethod, lcDMethod , lcPUnMeth  , lcDUnMeth ,;
             lcTmpCtPk
             
STORE '' To lcWinch0,lcWinCh1,lcWinCh2,lcWinCh3,lcBrowStr,lcSvdPo,lcDropLoc,;
            lcIType1,lcIType2,lcIType3,lcIType4,lcIType5
STORE 0   TO lnTotQty , lnLnNo , lnQty1 , lnQty2 , lnQty3 , lnQty4 , lnQty5 ,;
             lnQty6   , lnQty7 , lnQty8

STORE 'O' TO lcOrdType
laDefProc[9]  = .F.     && Save procedure(lpSavScr)
IF !gfSetup()
  RETURN
ENDIF

STORE 1   TO lnCurrUnt1,lnCurrUnt2,lnShpLoc
lcAType     = 'P'
lcPOType    = 'P/O'
lcAProgm    = 'POSTY'
lcUnCmPgId  = PADR('STYLE_PO'+lcAType,10)
llMulCurr   = gfGetMemVar('llMulCurr',gcAct_Comp)
llWareHous  = gfGetMemVar('M_WareHouse')='Y'
llGenOrNum  = gfGetMemVar('M_GenStOrN') ='Y'
lcCrtCSH    = gfGetMemVar('M_CRTCSTSH')
llDyelot    = gfGetMemVar('M_Dyelot')   ='Y'

lcIType1    = gfGetMemVar('M_cIType1')
lcIType2    = gfGetMemVar('M_cIType2')
lcIType3    = gfGetMemVar('M_cIType3')
lcIType4    = gfGetMemVar('M_cIType4')
lcIType5    = gfGetMemVar('M_cIType5')
llSOInstld  = (OCCURS('SO',gcComp_mdl)<>0)
llApIstall  = (OCCURS('AP',gcComp_mdl)<>0)

lcBro_Titl = 'Order_Lines'
IF !WEXIST(gcBaseWind)
  lcWinCh1   = gfTempName()
  STORE .T. TO llFromBrow
  STORE SPACE(0) TO lcEscTrap , lcCtrETrap , lcCtrHTrap , lcCtrNTrap
  lnBrR = 1
  lcWinch0   = gfTempName()
  lcWinCh2   = gfTempName()
  lcWinCh3   = gfTempName()
  lcWinCh4   = gfTempName()
  lcOrdHdr   = gfTempName()
  lcOrdLine  = gfTempName()
  lcAlocated = gfTempName()
  lcTempAloc = gfTempName()
  lcPOHdr    = gfTempName()
  lcPOLine   = gfTempName()
  
ENDIF

lcMjrTtl  = gfItemMask('HM')
lcMjrPct  = gfItemMask('PM')
lnstylewid=LEN(lcMjrPct)
lnLineNo = 0
DECLARE laDisRltFld[1,2]
laDisRltFld[1,1] = 'DISCPCNT'
laDisRltFld[1,2] = 'lnDisc_Pcnt'

lcKeyBmp = gcBmpHome + "ExtKey.BMP"
lcCanBmp = gcBmpHome + "Can.BMP"
lcSavBmp = gcBmpHome + "Sav.BMP"

*-- Open the files.
=lfOpenfls()

*-- variables of SOUPDATE.PRG [Start]

DECLARE laVariables[6] , laOrdStatus[2] , laSetups[8,2]
STORE 2   TO lnOrdStatus
laOrdStatus[1] = 'Bid      '
laOrdStatus[2] = 'Open     '

STORE .F. TO llContinue , llBomVarnt  , llCDPerL  , laSetups    , llMFDsPrc, llUpdAlo   ,;
             llPODsPrc  , llFromEDI
STORE ''  TO lcFlToUpd  , lcSession   , lcFiles   , laVariables , lcGlYear , lcGlPeriod ,;
             lcExRsin   , lcUntSin    , lcODefDiv , lcScrMode   , lcCurrOrd, lcODefSes  ,;
             lcODefWare , lcOrdCanLn
STORE {}  TO ldDefOrdDate
STORE 2   TO lnactfolder
lcFlToUpd  = gfTempName()
lcOrdCanLn = gfTempName()

IF 'PO' $ gcCmpModules     && if module PO is installed
  llPODsPrc = gfGetMemVar('M_PoDspPrc')  && setting in Po Module
ENDIF

IF 'MF' $ gcCmpModules	   && if module MF is installed
  llMFDsPrc = gfGetMemVar('M_MFDspPrc')  
ENDIF

laVariables[1] = 'ldDefOrdDate'
laVariables[2] = 'lcODefSes'  
laVariables[3] = 'lcODefDiv'
laVariables[4] = 'lcODefWare'
laVariables[5] = 'lcScrMode'
laVariables[6] = 'lcCurrOrd'

=gfOpenFile(gcDataDir+'ORDCANLN',gcDataDir+'ORDCANLN','SH')
=AFIELDS(laFileStru)
=gfCrtTmp(lcOrdCanLn,@laFileStru,[CORDTYPE+ORDER+STR(LINENO,6)],lcOrdCanLn)
=gfCloseFile('ORDCANLN')

laSetups[1,1]  = 'M_PACK'        && System has been steup to use packs
laSetups[2,1]  = 'M_STY_COM'     && Edit sales reps commissions at style level
laSetups[3,1]  = 'M_OR_NOTE'     && Edit order lines notepad
laSetups[4,1]  = 'M_LINK_GL'     && System has been linked to GL
laSetups[5,1]  = 'M_WareHouse'   && System has been steup to use multiple warehouses
laSetups[6,1]  = 'M_GenOrNum'    && Generate order number manually
laSetups[7,1]  = 'M_HOLD_ORD'    && Default new order status to hold
laSetups[8,1]  = 'M_CANAFTER'    && Number of days need to calculate order completion date
=gfGetMemVar(@laSetups,gcAct_Comp)
*-- variables of SOUPDATE.PRG [End]

*-- Create Temp Files.
=lfCrtTmpFil()

lcScFields = 'ORDER,ACCOUNT,STORE,CUSTPO,STATUS,MULTI,MULTIPO,ENTERED,START,'+;
             'COMPLETE,cTermCode,SHIPVIA,SPCINST,SEASON,cDivision,DISC,DEPT,'+;
             'NOTE1,NOTE2,BUYER,PHONE,CINSUR,BULK,CREORDER,PRIORITY,CFACCODE,'+;
             'REP1,COMM1,REP2,COMM2,CWARECODE,LINK_CODE,CCURRCODE,NEXRATE,BOOK,BOOKAMT,'+;
             'SHIP,SHIPAMT,CANCEL,CANCELAMT,OPEN,OPENAMT,CFROMORDER,'+;
             'CANCELLED,DECL_DATE,DECL_CODE,CCANCRESON,APPROVAL,APPRAMT,'+;
             'NCURRUNIT,Alt_ShpTo,CORDERCAT,GL_SALES,INT_VEND,EVENT_COD,'+;
             'BILLNO,MERC_TYPE,BLANK_ORD,DISTRB_NO,CCLASS,LFROMWEB'
  
SELECT OrdHdr
SCATTER FIELDS &lcScFields TO laData BLANK

laKeyField[1,1] = 'lcOrdType'
laKeyField[1,2] = .F.
laKeyField[1,3] = 'ORDHDR'
laKeyField[1,4] = 1
laKeyField[2,1] = 'laData[1]'
laKeyField[2,2] = .T.
laKeyField[2,3] = 'ORDHDR'
laKeyField[2,4] = 2


PUSH KEY
ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBro_Titl)
ON KEY LABEL TAB
ON KEY LABEL BACKTAB

lcHldEsc = ON('KEY','ESC')
ON KEY LABEL ESC DO lpDetEsc
lcEscTrap = ON('KEY','ESC')

DO (gcScrDir+gcWinAppl+"\SORESIZ.SPX")
ON KEY LABEL ALT+A 
POP KEY

ON KEY LABEL ESC     &lcHldEsc

IF glQuitting
  IF USED(lcOrdHdr)
    USE IN (lcOrdHdr)
  ENDIF
  ERASE (gcDef_Path+lcOrdHdr+".DBF")
  ERASE (gcDef_Path+lcOrdHdr+".CDX")
  
  IF USED(lcOrdLine)
    USE IN (lcOrdLine)
  ENDIF
  ERASE (gcDef_Path+lcOrdLine+".DBF")
  ERASE (gcDef_Path+lcOrdLine+".CDX")
  ERASE (gcDef_Path+lcOrdLine+".FPT")

  IF USED(lcAlocated)
    USE IN (lcAlocated)
  ENDIF
  ERASE (gcWorkDir+lcAlocated+".DBF")
  ERASE (gcWorkDir+lcAlocated+".CDX")   
  
  IF USED(lcTempAloc)
    USE IN (lcTempAloc)
  ENDIF
  ERASE (gcWorkDir+lcTempAloc+".DBF")
  ERASE (gcWorkDir+lcTempAloc+".CDX")   

  IF USED(lcPOHdr)
    USE IN (lcPOHdr)
  ENDIF
  ERASE (gcWorkDir+lcPOHdr+".DBF")
  ERASE (gcWorkDir+lcPOHdr+".CDX")   

  IF USED(lcPOLine)
    USE IN (lcPOLine)
  ENDIF
  ERASE (gcWorkDir+lcPOLine+".DBF")
  ERASE (gcWorkDir+lcPOLine+".CDX")   

  
ENDIF

*- End OF Code.
*:*************************************************************
*: Name      : lfOrdBrow
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Validate Sales order number.
*:*************************************************************
*: Calls     : AriaBrow , lpDefnFlds
*:*************************************************************
*: Parameters: lcOrder
*:*************************************************************
*: Returns   :  .f.
*:*************************************************************
*: Example   :  =lfOrdBrow()
*:*************************************************************
*:
FUNCTION lfOrdBrow
PARAMETERS lcOrder

*-- Adding the Browse in 2 lines because the compiler can't compile a line which is too long
*-- Note : No more fields can be added to the BROWSE or the browse will display an error message
*-- "a line is too long to be compiled"
DO lpDefnFlds               && Define lcBrFields
DO CASE
  CASE !EMPTY(lcOrder) .AND. SEEK(lcOrdType+lcOrder,'OrdHdr')
    RETURN
  OTHERWISE
    SELECT ORDHDR
    SET ORDER TO TAG ORDHDR IN OrdHdr
    PRIVATE lcOldFilt
    lcOldFilt = FILTER()
    SET FILTER TO
    lcOrder = IIF(ARIABROW('lcOrdType',"Orders",gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','Order','laBrowArr'),;
                  OrdHdr.Order,SPACE(6))
    SET FILTER TO &lcOldFilt
ENDCASE

*-- End OF lfOrdBrow
*:**************************************************************************
*: Name      : lpDefnFlds
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Define the lcBrFields variable.
*:**************************************************************************
*: Reference : None.
*:**************************************************************************
*: Parameters: None.
*:**************************************************************************
*: Returns   :  .f.
*:**************************************************************************
*: Example   : lpDefnFlds
*:**************************************************************************
*
PROCEDURE lpDefnFlds
lcBrFields = [Order:H="Order#",Status:1:H="Status",Account:H="Acct",]+;
             [Store=IIF(MULTI='Y','*Multi*',STORE):H="Store",]+;
             [Customer.stname:30:H="Name",]+;
             [Dept:H="Department",MultiPo=IIF(MultiPo,'Yes','No'):H="Multi PO",]+;
             [CustPo=IIF(MultiPO,'*Multi_PO*',CustPO):H="Cust. P.O#",]+;
             [Bulk:H="Bulk",Multi:H="Multi",cFacCode:H="Factor Code",]+;
             [Rep1:H="Sales Rep1",Comm1:H="Commission 1",Rep2:H="Sales Rep2",]+;
             [Comm2:H="Commission 2",Open:H="Open.Qty.",OpenAmt:H="Open.Amt.",]+;
             [Ship:H="Ship.Qty.",ShipAmt:H="Ship.Amt.",]+;
             [Book:H="Book.Qty.",BookAmt:H="Book.Amt.",]+;
             [Cancel:H="Cancel.Qty.",CancelAmt:H="Cancel.Amt."]
lcBrFields = lcBrFields + [,Entered:H="Entered",Start:H="Start",Complete:H="Complete",]+;
             [Cancelled:H="Cancelled",Disc:H="Disc.",]+;
             [cWareCode:H="Warehouse",cAdd_User:H="User",dAdd_Date:H="Date",]+;
             [lcSesDesc=gfCodDes(Season,'SEASON'):H="Season",]+;
             [lcDivDesc=gfCodDes(cDivision,'CDIVISION'):H="Division",]+;
             [lcShipVia=gfCodDes(ShipVia,'SHIPVIA'):H="ShipVia",]+;
             [Note1:6:H="Notes",Note2:6:H="Notes 2"]
IF llMulCurr
  lcBrFields = lcBrFields + [,cCurrCode:H="Currency",nExRate:H="Ex. Rate",nCurrUnit:H="Unit"]
ENDIF             

*-- End OF lpDefnFlds
*:*************************************************************
*: Name      : lfOpenfls
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Open Files.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lfOpenfls()
*:*************************************************************

FUNCTION lfOpenfls

=gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')

=gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
=gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')

IF !USED('UnCmSess')
  =gfOpenFile(gcDataDir+'UnCmSess',gcDataDir+'TRANS','SH')
ENDIF
IF !USED('STYDYE')
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES',gcDataDir+'Ccode_no','SH')
ENDIF

*-- End OF lfOpenfls.
*:*************************************************************
*: Name       : lfCrtTmpFil
*: Developer  : Abdou Elgendy
*: Date       : 11/07/2002
*: Purpose    : Create temp. file.
*:*************************************************************
*: Calls      : =gfCrtTmp()
*:*************************************************************
*: Passed Parameters  :  None
*:*************************************************************
*: Returns            : None
*:*************************************************************
*: Example            :  =lfCrtTmpFil()
*:*************************************************************
*:
FUNCTION lfCrtTmpFil

SELECT ORDHDR
lnFileStru = AFIELDS(laFileStru)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

CREATE TABLE (lcOrdHdr) FROM ARRAY laFileStru
INDEX ON cordtype+order TAG (lcOrdHdr)

SELECT ORDLINE
lnFileStru = AFIELDS(laFileStru)
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0

laFileStru[lnFileStru+2,1] = 'nTotAmt'
laFileStru[lnFileStru+2,2] = 'N'
laFileStru[lnFileStru+2,3] = 10
laFileStru[lnFileStru+2,4] = 2

CREATE TABLE (lcOrdLine) FROM ARRAY laFileStru
INDEX ON cordtype+order+STR(lineno,6) TAG 'ORDLINE'
INDEX ON Style TAG 'Style' ADDITIVE


SET RELATION TO STYLE INTO STYLE

SELECT CutPick
lnFileStru = AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]
laFileStru[lnFileStru+1,1] = 'nSteps'
laFileStru[lnFileStru+1,2] = 'N'
laFileStru[lnFileStru+1,3] = 2
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'cUpdSizes'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 8
laFileStru[lnFileStru+2,4] = 0

CREATE TABLE (lcTempAloc) FROM ARRAY laFileStru
INDEX ON TRANCD+ORDER+CORDLINE TAG 'CUTORD'
INDEX ON TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE TAG 'CUTPKORD'

  
CREATE TABLE (lcAlocated) FROM ARRAY laFileStru
INDEX ON TRANCD+ORDER+CORDLINE TAG 'CUTORD'
INDEX ON TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE TAG 'CUTPKORD'

*-Create PO Header file. 
SELECT POSHDR
=AFIELDS(laStrufile)
lnFileStru = ALEN(laStrufile,1)
DIMENSION laStrufile[lnFileStru+1,4]
laStrufile[lnFileStru+1,1] = 'nSteps'
laStrufile[lnFileStru+1,2] = 'N'
laStrufile[lnFileStru+1,3] = 2
laStrufile[lnFileStru+1,4] = 0
=gfCrtTmp(lcPOHdr,@laStrufile,'cStyType+PO',lcPOHdr)
SELECT (lcPOHdr)

*-Create line file. 
SELECT POSLN
=AFIELDS(laStrufile)
lnFileStru = ALEN(laStrufile,1)
DIMENSION laStrufile[lnFileStru+4,4]

laStrufile[lnFileStru+1,1] = 'nSteps'
laStrufile[lnFileStru+1,2] = 'N'
laStrufile[lnFileStru+1,3] = 2
laStrufile[lnFileStru+1,4] = 0

laStrufile[lnFileStru+2,1] = 'cAccName'
laStrufile[lnFileStru+2,2] = 'C'
laStrufile[lnFileStru+2,3] = 30
laStrufile[lnFileStru+2,4] = 0
laStrufile[lnFileStru+3,1] = 'Start'
laStrufile[lnFileStru+3,2] = 'D'
laStrufile[lnFileStru+3,3] = 8
laStrufile[lnFileStru+3,4] = 0
laStrufile[lnFileStru+4,1] = 'Complete'
laStrufile[lnFileStru+4,2] = 'D'
laStrufile[lnFileStru+4,3] = 8
laStrufile[lnFileStru+4,4] = 0

=gfCrtTmp(lcPOline,@laStrufile,'Style+STR(LineNo,6)',lcPOline)

SELECT (lcPOline)
RETURN

*-- End Of lfCrtTmpFil
*:*************************************************************
*: Name      : lfvOrder
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Validate Sales Order number.
*:*************************************************************
*: Calls     : lfOrdBrow, 
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  .f.
*:*************************************************************
*: Example   :  =lfvOrder()
*:*************************************************************
*:
FUNCTION lfvOrder
PRIVATE lcOrder

IF MDOWN() 
  RETURN
ENDIF
IF (!EMPTY(laData[1]) .AND. SEEK(lcOrdType+laData[1],'ORDHDR'))
  =lfUpdTmp()
  laScrMode = .F.
  laScrMode[2] = .T.  
  SHOW GET ibOrder DISABLE
  SHOW GETS
ENDIF


IF llBrowse .OR. (!EMPTY(laData[1]) .AND. !SEEK(lcOrdType+laData[1],'ORDHDR'))
  lcOrder = laData[1]
  =lfOrdBrow(@lcOrder)

  IF EMPTY(lcOrder)
    laData[1] = lcOrder 
    SHOW GET laData[1] ENABLE
    _CUROBJ = OBJNUM(laData[1])
    RETURN  
  ELSE
    laData[1] = lcOrder
    llBrowse = .F.
    laScrMode = .F.
    laScrMode[2] = .T.
    SHOW GET ibOrder DISABLE
    SHOW GETS
  ENDIF
ENDIF

IF INLIST(laData[5],'C','X')
  SHOW GET pbEdt DISABLE
ENDIF

SHOW GET Dumy    ENABLE
=lfDispBrow()

*-- End OF lfvOrder
*:*************************************************************
*: Name      : lpSavScr
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Validate Save Button.
*:*************************************************************
*: Calls     : 1) DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')
*:           : 2) DO gfSavePo IN (gcapphome+'POUPDATE.PRG')
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lpSavScr()
*:*************************************************************

FUNCTION lpSavScr
PRIVATE lcSavedPo
lcSavedPo = lcSvdPo
SELECT (lcOrdLine)
SET FILTER TO TOtQty # 0
PRIVATE  lcOldOrder
lcOldOrder = ORDER()

SET ORDER TO TAG 'ORDLINE'
*-- Update &lcOrdLine..Qty1 ... &lcOrdLine..Qty8
FOR lnInde = 1 TO Scale.Cnt
  lcInd = STR(lnInde,1)
  REPLACE Qty&lcInd WITH lnQty&lcInd
  SHOW GET lnQty&lcInd
ENDFOR

lnTotQty = lnQty1 + lnQty2 + lnQty3 + lnQty4 + lnQty5 + lnQty6 + lnQty7 + lnQty8
SHOW GET lnTotQty

REPLACE TotQty WITH lnTotQty


DO lfSavScr IN (gcAppHome + 'SO\SOUPDATE.FXP')

*-- the v Close the following Files , So I will open it again here.
IF !USED('POSHDR')
  =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
ENDIF
IF !USED('POSLN') 
  =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLN','SH')
ENDIF
IF !USED('CUTPICK')
  =gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')
ENDIF

*-- Save the New Records.
IF llAddNewRe .AND. !EMPTY(lcSvdPo)
  
  SELECT (lcTempAloc)
  LOCATE
  SCAN
    SCATTER TO laCutPick
    INSERT INTO CUTPICK FROM ARRAY laCutPick
  ENDSCAN
  
  *--Update the Po line file.
  lcOldFilds = lcScFields
  lcScFields ='Po        ,Vendor    ,Status    ,cDivision ,Entered   ,'+;
              'Complete  ,Available ,Origin    ,cFob      ,cTermCode ,'+;
              'ShipVia   ,QuotaCat  ,cLcNo     ,LCExpire  ,Contact   ,'+;
              'Phone     ,Insurance ,ShpName   ,cWareCode ,Link_Code ,'+;
              'cOutAddr1 ,cOutAddr2 ,cOutAddr3 ,cOutAddr4 ,cOutAddr5 ,'+;
              'cPriceCur ,cDutyCur  ,nPriceRat ,nDutyRat  ,PoTotal   ,'+;
              'nICost1   ,nICost2   ,nICost3   ,nICost4   ,nICost5   ,'+;
              'nStyOrder ,Receive   ,Damage    ,Cancel    ,Open      ,'+;
              'nAct_Cost1,nAct_Cost2,nAct_Cost3,nAct_Cost4,nAct_Cost5,'+;
              'nLan_Cost1,nLan_Cost2,nLan_Cost3,nLan_Cost4,nLan_Cost5,'+;
              'nFCost1   ,nFCost2   ,nFCost3   ,nFCost4   ,nFCost5   ,'+;
              'nFLanCost1,nFLanCost2,nFLanCost3,nFLanCost4,nFLanCost5,'+;
              'nFActCost1,nFActCost2,nFActCost3,nFActCost4,nFActCost5,'+;
              'cMultiLot ,cPurCode  ,Buyer,Account,Store  ,cPONo,'     +;
              'Color     ,Cost'
  llContine = .T.
  
  SELECT (lcPoHdr)
  SCATTER FIELDS &lcScFields TO laData
  lcDropLoc = ladata[19]
  DO gfSavePo IN (gcapphome+'POUPDATE.PRG')
  lcScFields = lcOldFilds
ENDIF

SELECT (lcOrdLine)
SET FILTER TO 
SET ORDER TO &lcOldOrder
LOCATE

*- Update the ordhdr status with .F.
=SEEK(cOrdType+Order,'OrdHdr')
SELECT OrdHdr
REPLACE LLOK_STAT WITH .F.,;
        CLOK_USER WITH '',;
        DLOK_DATE WITH {},;
        CLOK_TIME WITH ''

SCATTER FIELDS &lcScFields TO laData



*-- Update the PO Line File And Header file with last line.
IF !EMPTY(lcSavedPo)
  SELECT POSLN
  =SEEK('P'+lcSavedPo)
  lnLastNo = 0
  SCAN REST WHILE  cstytype+po+style+STR(lineno,6)+trancd = 'P'+lcSavedPo
    SELECT POSLN
    lnLastNo = lnLastNo + 1
    REPLACE LineNo WITH lnLastNo
  ENDSCAN

  SELECT POSHDR
  =SEEK('P'+lcSavedPo)
  REPLACE lastLine WITH lnLastNo
ENDIF

*- Sort the Cut Pick File.
SELECT CUTPICK
lcOrdName = Order()
SET ORDER TO CUTPICK
SELECT ORDLINE
=SEEK('O'+ladata[1])
SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + ladata[1]
  IF SEEK(Ordline.Style,'STYLE')
    IF SEEK(IIF(Style.Make,'1','2')+lcSavedPo+Ordline.Style,'CutPick')
      SELECT CUTPICK
      REPLACE cordline   WITH STR(Ordline.lineno,6),;
              ctktlineno WITH STR(Ordline.lineno,6)
        
    ENDIF
  ENDIF
  SELECT ORDLINE
ENDSCAN
SELECT CUTPICK
SET ORDER TO &lcOrdName
SELECT (lcOrdLine)

*--End OF lpSavScr
*:*************************************************************
*: Name      : lpShow
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Show function.
*:*************************************************************
*: Calls     : =lfRestVar("A") , =lfUpdTmp() , =lfGetOrd()
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lpShow()
*:*************************************************************
*:
FUNCTION lpShow

SHOW GET pbDlt DISABLE
IF INLIST(laData[5],'C','X')
  SHOW GET pbEdt DISABLE
ENDIF
DO CASE
  CASE laScrMode[1]      && Select Mode. 
  
    *B606773,1 ABD - Fix problem in the resize screen in case update many
    *B606773,1 ABD - orders at the same session. [Begin]
    IF USED(lcTempAloc)
      SELECT(lcTempAloc)
      DELE ALL
    ENDIF

    IF USED(lcAlocated)
      SELECT(lcAlocated)
      DELE ALL
    ENDIF
    *B606773,1 ABD - [End]
  
    lcSvdPo = ''
    SELECT(lcPOHdr)
    DELE ALL
    SELECT(lcPOLine)
    DELE ALL

    SELECT (lcOrdLine)
    Zap
    SELECT (lcOrdHdr)
    ZAP
    =lfRestVar("A")
  CASE laScrMode[2]      && View Mode.

    *B606773,1 ABD - Fix problem in the resize screen in case update many
    *B606773,1 ABD - orders at the same session. [Begin]
    IF USED(lcTempAloc)
      SELECT(lcTempAloc)
      DELE ALL
    ENDIF

    IF USED(lcAlocated)
      SELECT(lcAlocated)
      DELE ALL
    ENDIF
    *B606773,1 ABD - [End]
    
    lcSvdPo = ''
    SELECT(lcPOHdr)
    DELE ALL
    SELECT(lcPOLine)
    DELE ALL
  
    SELECT (lcOrdHdr)
    ZAP
    SELECT (lcOrdLine)
    ZAP

    =lfUpdTmp()
    LOCATE
    
  CASE laScrMode[3]      && Edit Mode.
  CASE laScrMode[4]      && Save Mode.
ENDCASE


=lfDispBrow()
SHOW GET Dumy    ENABLE

*-- End OF lpShow.
*:*************************************************************
*: Name      : lfRestVar
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Reset the screen objects.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: lc2Rest
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lfRestVar()
*:*************************************************************
*:
FUNCTION lfRestVar
PARAMETER lc2Rest

FOR lnInde = 1 TO 8
  lcInd = STR(lnInde,1)
  lcSz&lcInd = ""
  lnQty&lcInd = 0
  SHOW GET lnQty&lcInd
ENDFOR
IF lc2Rest = "A"
  STORE "" TO laData[1] , lcActName , lcStyMaj , lcStyDesc , lcColour , lcFit
ENDIF

*-- End OF lfRestVar.
*:*************************************************************
*: Name      : lfUpdTmp
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Update Temp. files.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lfUpdTmp()
*:*************************************************************
*:
FUNCTION lfUpdTmp
PRIVATE lnOldAls

lnOldAls = SELECT(0)
SELECT ORDHDR
SCATTER FIELDS &lcScFields TO laData
SCATTER MEMV MEMO
SELECT (lcOrdHdr)
APPEND BLANK
GATHER MEMV MEMO
SELECT (lcOrdLine)
SET RELATION TO 
lcOldOrder = ORDER()
SET ORDER TO STYLE

lnLineNo = Ordhdr.lastLine
SELECT ORDLINE
SCAN FOR cordtype+order+STR(lineno,6) = 'O' + laData[1]
  SCATTER MEMV MEMO
  = lfClrStyls ()
ENDSCAN

IF !Empty(lcSvdPo) .AND. !(INLIST(Ordhdr.Status,'C','X'))
  *-- Call Function to get the PO Headr data
  IF !lfGetPOLn()
    *-- Text Message
    *- Message Text   :- This Sales Order is linked to an Open Purchase Order. Cannot proceed.
    *- Message No.    :- 000000.
    *- Buttom Message :- Ok
    *- Buttom Number  :- 00000.
    = gfModalGen('INM00000B00000','F','ALERT',' ','You can modify only Hold purchase orders linked to this sales '+;
     'order. Can not proceed.')
    laScrMode = .F.
    laScrMode[1] = .T.  
    SHOW GETS
    RETURN
  ENDIF
ENDIF

SELECT (lcOrdLine)
SET RELATION TO STYLE INTO STYLE
SET ORDER TO &lcOldOrder
LOCATE
=SEEK('S'+Style.Scale,'Scale')
=lfUpVarbls()
SELECT(lnOldAls)

*-- End OF lfUpdTmp.
*:*************************************************************
*: Name      : lfvQty
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Qtys Validation function.
*:*************************************************************
*: Calls     : None
*:*************************************************************
*: Parameters: lnQty
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lfvQty()
*:*************************************************************
*:
FUNCTION lfvQty
PARAMETERS lnQty

llUpdBook = (laData[5] = 'B') .OR. (gdSysDate <= (laData[8] + laSetups[8,2]))
SELECT (lcOrdLine)
FOR lnInde = 1 TO Scale.Cnt
  lcInd = STR(lnInde,1)
  lnOldQty&lcInd = &lcOrdLine..Qty&lcInd
ENDFOR

lnOldTot = &lcOrdLine..TotQty

lcQty = STR(lnQty,1)
REPLACE Qty&lcQty WITH lnQty&lcQty,;
        TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8


IF lnQty <> 9  
  lcQty = STR(lnQty,1)
  IF Qty&lcQty < 0 
    *-- Message : 42000
    *-- Negative values are not allowed.
    *-- Button  : 40011
    *-- Ok
    = gfModalGen('TRM42000B40011','DIALOG')
    REPLACE Qty&lcQty WITH lnOldQty&lcQty,;
            TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    lnQty&lcQty = QTY&lcQty
    SHOW GET lnQty&lcQty
    RETURN
  ENDIF
  IF Qty&lcQty <> lnOldQty&lcQty
    IF Picked AND !EMPTY(PikTKt) AND Qty&lcQty < Pik&lcQty
      *-- Message : 32015
      *-- Quantity cannot be below than what has been picked for this style/color
      *-- Button : 00000
      *-- Ok
      =gfModalGen('TRM32015B00000','ALERT')
      REPLACE Qty&lcQty WITH lnOldQty&lcQty,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      lnQty&lcQty = QTY&lcQty
      SHOW GET lnQty&lcQty
      RETURN
    ENDIF
    *--  Check the sold out date of the style edited its quantity
    *--  Message :Style sold out date is XX/XX/XXXX
    *--    < Accept >  < Reenter >
    IF !EMPTY(Style.SoldOut) .AND. Style.SoldOut < laData[9] .AND. ;
      gfModalGen('QRM40010B40001','ALERT','sold out|'+DTOC(Style.SoldOut))=2
      REPLACE Qty&lcQty WITH lnOldQty&lcQty,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
      lnQty&lcQty = QTY&lcQty
      SHOW GET lnQty&lcQty
      RETURN
    ENDIF
    
    IF Qty&lcQty # Cut&lcQty
      *-- If the user has no access on CT, inform the user and ignore all.
      IF !gfUserPriv('MF','MFCUTKT')
        llIgnorAll = .T.  
        *-- Message: This order has an allocated quantity, you may need to edit the 
        *-- quantities on the cutting ticket, but the access to do this is denied.'
        =gfModalGen('INM32075B00000','DIALOG')
        REPLACE Qty&lcQty WITH lnOldQty&lcQty,;
                TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        lnQty&lcQty = QTY&lcQty
        SHOW GET lnQty&lcQty
        RETURN
      ENDIF
      *-- Message : 32016
      *-- Size xxxxx has xxx pieces allocated from orders.
      *-- Edit the detail allocated quantity from the order lines to keep 
      *-- track of the allocated quantity, Ignore tracking the allocation 
      *-- for only the quantity being modified, or for all later modifications.
      *-- Button : 32002
      *-- Edit Allo. Qty./Ignore/Ignore All/Cancel
      lnAlias = SELECT()
      SELECT CUTPICK
      SET ORDER TO TAG 'CUTORD' IN (lcAlocated)
      llUpdTmpAl = .F.
      IF !SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcAlocated) .AND. ;
        SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),'CutPick')
        SELECT CutPick
        SCAN REST WHILE TranCd+Order+cOrdLine = ;
          IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6)
          SCATTER TO laCutPick
          INSERT INTO (lcAlocated) FROM ARRAY laCutPick
        ENDSCAN
      ELSE
        IF !SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcAlocated) .AND. ;
           !SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcTempAloc) .AND. ;
           !EMPTY(lcSvdPo)
          llUpdTmpAl = .T.
          *-- Add this Line to the Temp File.
          =SEEK('2'+&lcOrdLine..Order,'CutPick')
          SELECT (lcTempAloc)
          *-- Check if we add before that or Not.
          IF !SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcTempAloc)
            APPEND BLANK
            REPLACE cTktno     WITH  CutPick.ctktno   ,;
                    Trancd     WITH '2'               ,;
                    Order      WITH &lcOrdLine..Order ,;
                    cOrdline   WITH STR(&lcOrdLine..LineNo,6),;
                    cTktlineno WITH STR(&lcOrdLine..LineNo,6),;
                    Style      WITH &lcOrdLine..Style ,;
                    Qty1       WITH 0  ,;
                    Qty2       WITH 0  ,;
                    Qty3       WITH 0  ,;
                    Qty4       WITH 0  ,;
                    Qty5       WITH 0  ,;
                    Qty6       WITH 0  ,;
                    Qty7       WITH 0  ,;
                    Qty8       WITH 0  ,;
                    Totqty     WITH 0
            llAddNewRe = .T.
          ENDIF
          *-- End Update
          ELSE
           IF !SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcAlocated) .AND. ;
              SEEK(IIF(Style.Make,'1','2')+&lcOrdLine..Order+STR(&lcOrdLine..LineNo,6),lcTempAloc) .AND. ;
              !EMPTY(lcSvdPo)
              llUpdTmpAl = .T.
           ENDIF
        ENDIF
      ENDIF
      SELECT (lcOrdLine)
      IF !EMPTY(lcSvdPo)
        SCATTER MEMVAR MEMO
        *-- Update the Allo Qty.
        IF !lfEditAlo(lcQty)
          REPLACE Qty&lcQty WITH lnOldQty&lcQty,;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        ELSE
          REPLACE Qty&lcQty WITH M.Qty&lcQty,;
                  TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
        ENDIF
        *-- Update the PoSline & POSHdr
        SCATTER MEMVAR MEMO
        = lfVPoLnHdr ()
      ENDIF
       
      SELECT (lnAlias)
    ENDIF
  ENDIF
ENDIF   

*-- Updte the Screen Fields.
For I = 1 TO 8
  lcSize = STR(I,1)
  lnQty&lcSize = QTY&lcSize
  SHOW GET lnQty&lcSize
ENDFOR
lnTotQty = &lcOrdLine..TotQty
SHOW GET lnTotQty


=SEEK(cOrdType+Order+STR(LineNo,6),'OrdLine')
laData[35] = laData[35] - TotBook
laData[36] = laData[36] - TotBook*Price
=RLOCK()
REPLACE Book1   WITH MAX(OrdLine.Book1-OrdLine.Qty1+Qty1,IIF(llUpdBook,0,OrdLine.Book1))  ,;
        Book2   WITH MAX(OrdLine.Book2-OrdLine.Qty2+Qty2,IIF(llUpdBook,0,OrdLine.Book2))  ,;
        Book3   WITH MAX(OrdLine.Book3-OrdLine.Qty3+Qty3,IIF(llUpdBook,0,OrdLine.Book3))  ,;
        Book4   WITH MAX(OrdLine.Book4-OrdLine.Qty4+Qty4,IIF(llUpdBook,0,OrdLine.Book4))  ,;
        Book5   WITH MAX(OrdLine.Book5-OrdLine.Qty5+Qty5,IIF(llUpdBook,0,OrdLine.Book5))  ,;
        Book6   WITH MAX(OrdLine.Book6-OrdLine.Qty6+Qty6,IIF(llUpdBook,0,OrdLine.Book6))  ,;
        Book7   WITH MAX(OrdLine.Book7-OrdLine.Qty7+Qty7,IIF(llUpdBook,0,OrdLine.Book7))  ,;
        Book8   WITH MAX(OrdLine.Book8-OrdLine.Qty8+Qty8,IIF(llUpdBook,0,OrdLine.Book8))  ,;
        TotBook WITH Book1+Book2+Book3+Book4+Book5+Book6+Book7+Book8 ,;
        FLAG WITH IIF(FLAG='N','N','M')
UNLOCK
laData[35] = laData[35] + TotBook
laData[36] = laData[36] + TotBook*Price
laData[41] = laData[41] - lnOldTot+TOtQty
laData[42] = laData[42] - lnOldTot*Price+ TOtQty*Price
SELECT (lcOrdHdr)
=RLOCK()
REPLACE BOOK      WITH laData[35] ,;
        BOOKAMT   WITH laData[36] ,;
        CANCEL    WITH laData[39] ,;
        CANCELAMT WITH laData[40] ,;
        OPEN      WITH laData[41] ,;
        OPENAMT   WITH laData[42]
UNLOCK
*-- update the Po Header file & po Line File.
SELECT (lcOrdLine)
=lfRefresh(lcWinCh3)
=lfRefresh(lcWinCh1)

*-- End OF lfvQty.
*:*************************************************************
*: Name      : lfDispBrow
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Function to display the Style & Qty browse
*: Ref       : *C200398,4 ABD 
*:*************************************************************
*: Called from : None.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfDispBrow
PRIVATE lnAlias

lnAlias = SELECT()

SELECT (lcOrdLine)

lcBrowStr = "cMk=IIF(RECNO()=lnBrR,'>>',' '):1:H=' ':R,Style.Style:H='Style':R,"+;
             "qty1:H='Qty1':R,qty2:H='Qty2':R,qty3:H='Qty3':R,qty4:H='Qty4':R,qty5:H='Qty5':R,"+;
             "qty6:H='Qty6':R,qty7:H='Qty7':R,qty8:H='Qty8':R,totqty:H='Tot Qty':R"
BROWSE FIELDS &lcBrowStr;
     WINDOW (lcWinCh2) ;
     WHEN lfwBrows() ;
     VALID lfvBrows() ;
     IN WINDOW (gcBaseWind) ;
     LOCK 0;
     NOAPPEND;
     NOCLEAR;
     NODELETE;
     NOWAIT;
     NOMENU;
     SAVE;
     TITLE lcBro_Titl

=lfwBrows()
SELECT(lnAlias)

*-- End OF lfDispBrow
*:*************************************************************
*: Name      : lfwBrows
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Ref       : *C200398,4 ABD 
*: Purpose   : WHEN Function for the Style Color Size browse
*:*************************************************************
*: Called from : Screen WEA200 setup
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfwBrows

IF lnBrR # RECNO()

  lcStyMaj  = STYLE.CSTYMAJOR
  lcStyDesc = STYLE.DESC1
  lcColour  = SUBSTR(STYLE.Style,lnstylewid+2,3)
  lcStySek  = STYLE.Style
  lnLnNo    = ORDLINE.LINENO

  IF SEEK('S'+Scale,'Scale')
    FOR lnInde = 1 TO 8
      lcInd = STR(lnInde,1)
      lcSz&lcInd = SCALE.SZ&lcInd
      lnQty&lcInd = &lcOrdLine..Qty&lcInd
      SHOW GET lnQty&lcInd
    ENDFOR
  ENDIF
  lnTotQty = &lcOrdLine..TotQty
  lcFit    = SCALE.CDIM1
ENDIF
  

lnBrR = RECNO()            && Varible to hold the browse record number
SHOW WINDOW (lcBro_Titl) REFRESH SAME
=lfRefresh()

*-- End OF lfwBrows.
*:*************************************************************
*: Name      : lfvBrows
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : VALID Function for the Style Color Size browse
*: Ref       : *C200398,4 ABD 
*:*************************************************************
*: Called from : None.
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfvBrows

llFromBrow = .T.

*-- End OF lfvBrows
*:*************************************************************
*: Name      : lfBrwUnTrp
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Ref       : *C200398,4 ABD 
*: Purpose   : Function to untrap the keys for the Browse
*:*************************************************************
*: Called from : None
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcBro_Titl) .AND. llFromBrow
  llFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBro_Titl)
  ON KEY LABEL ESC &lcEscTrap
  ON KEY LABEL CTRL+END &lcCtrETrap
  ON KEY LABEL CTRL+HOME &lcCtrHTrap
  ON KEY LABEL CTRL+ENTER &lcCtrNTrap
ENDIF

*-- End OF lfBrwUnTrp
*:*************************************************************
*: Name      : lfBrowTrap
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Ref       : *C200398,4 ABD 
*: Purpose   : Function to trap the keys for the Browse
*:*************************************************************
*: Called from :  None.
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfBrowTrap

*-- IF The window on top is the Browse
IF WONTOP(lcBro_Titl)
  llFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH 1
  ON KEY LABEL BACKTAB DO lfTraps WITH 2
  ON KEY LABEL ALT+A
  lcEscTrap = ON('KEY' , 'ESC')                && Varible to hold the ESC key traping
  lcCtrETrap = ON('KEY' , 'CTRL+END')          && Varible to hold the CTRL+END key traping
  lcCtrHTrap = ON('KEY' , 'CTRL+HOME')         && Varible to hold the CTRL+HOME key traping
  lcCtrNTrap = ON('KEY' , 'CTRL+ENTER')        && Varible to hold the CTRL+ENTER key traping
  ON KEY LABEL ESC DO lfTraps WITH 3
  ON KEY LABEL CTRL+END DO lfTraps WITH 4
  ON KEY LABEL CTRL+HOME DO lfTraps WITH 5
  ON KEY LABEL CTRL+ENTER DO lfTraps WITH 6
ENDIF    && End of IF

RETURN .F.

*-- End Of lfBrowTrap
*:*************************************************************
*: Name      : lfTraps
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Ref       : *C200398,4 ABD 
*: Purpose   : Function to be called from some traped keys
*:*************************************************************
*: Called from : lfBrowTrap()
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : 1 , 2
*:*************************************************************
*: Return      : None
*:*************************************************************
FUNCTION lfTraps

PARAMETERS lnTrap

DO CASE 
  *-- Case TAB
  CASE lnTrap = 1
    IF WONTOP()= lcBro_Titl
      glFromBrow = .T.
      ACTI WINDOW (lcWinCh3) TOP
    ELSE
      _CUROBJ = _CUROBJ +1

    ENDIF
  *-- Case BACKTAB
  CASE lnTrap = 2
    ACTI WINDOW (lcWinCh1) TOP
    
  *-- Case ESC
  CASE lnTrap = 3
   DO  lpDetEsc
    
  *-- Case CTRL+END
  CASE lnTrap = 4
    GO BOTTOM
  
  *-- Case CTRL+HOME
  CASE lnTrap = 5
    GO TOP
  
  *-- Case CTRL+ENTER
  CASE lnTrap = 6
    RETURN
  
ENDCASE

*-- End OF lfTraps.
*:*************************************************************
*: Name      : lfClrStyls
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Ref       : *C200398,4 ABD 
*: Purpose   : Function to get the other color for current style. 
*:*************************************************************
*: Called from : lfBrowTrap()
*:*************************************************************
*: Calls       : None
*:*************************************************************
*: Passed Parameters : 1 , 2
*:*************************************************************
*: Return      : None
*:*************************************************************
*:
FUNCTION lfClrStyls
PRIVATE lnAlias

lnAlias = Select (0)
IF SEEK(SUBSTR(m.Style,1,lnstylewid),'STYLE')

  *-- get the PO# while Scan on the orders.
  IF Empty(lcSvdPo) .AND. SEEK(IIF(Style.Make,'1','2')+M.Order+STR(M.LineNo,6),'CutPick')
    lcSvdPo = CutPick.ctktNo
  ENDIF

  *-- Seek at First time with the current srtyle
  IF SEEK(M.Style,lcOrdLine)
    SELECT (lcOrdLine)
    IF &lcOrdLine..TotQty = 0
      GATHER MEMVAR MEMO
    ELSE
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF
    REPLACE  Flag WITH 'M'
  ELSE
    *-- get all the style - color - scale.
    SELECT STYLE
    SCAN REST WHILE Style = Substr(m.Style,1,lnstylewid)
      SELECT (lcOrdLine)
      IF !SEEK(Style.Style,lcOrdLine)
        *-- Check before add the the record if valid or not.
        STORE 0 TO lnPrice , lnGrosPric , lnDiscPcnt
        *-- IF we add new style not exist in the sales order.
        IF m.Style = Style.Style .OR. lfvChkAdd()
          APPEND BLANK
          IF m.Style = Style.Style
            GATHER MEMVAR MEMO
            REPLACE  Flag WITH 'M'
          ELSE
            lnLineNo = lnLineNo + 1
            *-- Change the Fields.
            REPLACE  cOrdType   WITH m.cOrdType  ,;
                     Order      WITH m.Order     ,;
                     Account    WITH m.Account   ,;
                     cWareCode  WITH m.CWareCode ,;
                     Start      WITH m.Start     ,;
                     Complete   WITH m.Complete  ,;
                     Gl_Sales   WITH m.Gl_Sales   ,;
                     lineNo     WITH lnLineNo    ,;
                     STORE      WITH STORE       ,;
                     Style      WITH Style.Style ,;
                     Scale      WITH Style.Scale ,;
                     Desc1      WITH Style.Desc1 ,;
                     Season     WITH Style.Season,;
                     STORE      WITH M.Store     ,;
                     Qty1       WITH 0           ,;
                     Qty2       WITH 0           ,;
                     Qty3       WITH 0           ,;
                     Qty4       WITH 0           ,;
                     Qty5       WITH 0           ,;
                     Qty6       WITH 0           ,;
                     Qty7       WITH 0           ,;
                     Qty8       WITH 0           ,;                     
                     TotQty     WITH 0           ,;
                     Price      WITH lnPrice     ,;
                     gros_price WITH lnGrosPric  ,;
                     disc_pcnt  WITH lnDiscPcnt  ,;
                     LineNo     WITH lnLineNo    ,;
                     Flag       WITH 'N'
          ENDIF          
        ENDIF                 
      ENDIF
    ENDSCAN
  ENDIF  
ENDIF

SELECT (lnAlias)
*-- End of lfClrStyls
*:*************************************************************
*: Name      : lfUpVarbls
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Update the screen variabls.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lfUpVarbls()
*:*************************************************************
*:
FUNCTION lfUpVarbls

PRIVATE lnOldAls,lnInd,lcInd
lnOldAls = SELECT(0)

SELECT (lcOrdLine)
lcStyMaj  = STYLE.CSTYMAJOR
lcStyDesc = STYLE.DESC1
lcColour  = SUBSTR(STYLE.Style,13,3)
lcStySek  = STYLE.Style
lnLnNo    = ORDLINE.LINENO

FOR lnInde = 1 TO Scale.Cnt
  lcInd = STR(lnInde,1)
  lcSz&lcInd = SCALE.SZ&lcInd
  lnQty&lcInd = &lcOrdLine..Qty&lcInd
  SHOW GET lnQty&lcInd
ENDFOR

lnTotQty = &lcOrdLine..TotQty
lcFit    = SCALE.CDIM1

IF SEEK('M'+Account,'CUSTOMER')
  lcActName = CUSTOMER.BTNAME
ENDIF


SELECT(lnOldAls)
*-- End OF lfUpVarbls
*:*************************************************************
*: Name      : lfvChkAdd
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : this Function to check if we will add it or not.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lfvChkAdd()
*:*************************************************************
*:
FUNCTION lfvChkAdd
PRIVATE llReturn , lnOldAlias 
lnOldAlias = SELECT (0)
llReturn = .F.

*-- 1) Check on the Sesason 
IF SEEK(M.cOrdType+M.Order,'ORDHDR') 

  *-- Check On the Season
  IF Ordhdr.Season = '*'
    llReturn = .T.
  ELSE
    llReturn = (Ordhdr.Season = Style.Season)
  ENDIF

  *-- Check on the 
  IF llReturn
    llReturn = (Ordhdr.cDivision= Style.cDivision)
  ENDIF
ENDIF

*-- Get the Price
IF llReturn
  *- Get the customer price level.
  IF SEEK('M'+Ordhdr.account,'Customer')
    lcPriceLvl =  IIF(!EMPTY(Customer.PriceLvl),Customer.PriceLvl,'A')
  ELSE
    lcPriceLvl = 'A'
  ENDIF
  
  *-- this function to get the Price,Gross Price and Discount %.
  = lfGetDisPr()

ENDIF

SELECT(lnOldAlias)

RETURN llReturn
*-- End OF lfvChkAdd
*:*************************************************************
*: Name      : lfGetDisPr
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Function to check if we will add current style or not.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None.
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   :  =lfGetDisPr()
*:*************************************************************
*:
FUNCTION lfGetDisPr

lnGrosPric = IIF(!llMulCurr .OR. OrdHdr.cCurrCode = gcBaseCurr,Style.Price&lcPriceLvl,;
             gfStyPrice(Style.Style,lcPriceLvl,OrdHdr.cCurrCode))

*-- get the cDiscCode From stydye in every case
lcDiscCode  = IIF(SEEK(Style.Style+OrdHdr.cWareCode+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
lnDiscPcnt = 0 
IF !EMPTY(ALLTRIM(lcDiscCode))
  *-- Get the disecound related filed to now which 
  *-- type whole Sale Or Retail sale Or Both.
  DECLARE laDisType[1,2] , lastartDte[1,2] , laEndDate[1,2]
  STORE '' To lcDisType , ldstartDte ,ldEndDate
  *-- Array to get the Discount affect for DecCode.
  laDisType[1,1]  = 'CCOSTAFECT'
  laDisType[1,2]  = 'lcDisType'
  *-- Array to get the start date For DescCode.
  lastartDte[1,1] = 'START'
  lastartDte[1,2] = 'ldstartDte'
  *-- Array to get the end date For DescCode.
  laEndDate[1,1]  = 'DENDATE'
  laEndDate[1,2]  = 'ldEndDate'
  
  = gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
  = gfRltFld(lcDiscCode, @lastartDte, 'CDISCCODE')
  = gfRltFld(lcDiscCode , @laEndDate, 'CDISCCODE')
  
  nDisc_Pcnt = 0
  IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(OrdHdr.Entered,ldstartDte,ldEndDate)
    lnDisc_Pcnt = lnDiscPcnt
    =gfRltFld(lcDiscCode,@laDisRltFld,'CDISCCODE')
    lnDiscPcnt  = lnDisc_Pcnt
  ENDIF
ENDIF  
lnPrice = lnGrosPric *(100-lnDiscPcnt)/100  

*-- End OF lfGetDisPr
*:*************************************************************
*: Name      : lpDetEsc
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Trap Esc for lines entry.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Parameters: None
*:*************************************************************
*: Returns   :  None.
*:*************************************************************
*: Example   :  DO lpDetEsc
*:*************************************************************
*
PROCEDURE lpDetEsc

ACTIVATE WINDOW ('gwcContrl1')
_CUROBJ = OBJNUM(pbCls)
KEYBOARD '{ENTER}'
RETURN

*-- End Of lpDetEsc
*:*************************************************************
*: Name      : lfEditAlo
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Edit alocated quantities for a specific size
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : lcEditSize   : Size
*:*************************************************************
*: Returns            :  None
*:*************************************************************
*: Example            :  =lfEditAlo('1')
*:*************************************************************
*:
FUNCTION lfEditAlo
PARAMETERS lcEditSize
PRIVATE lnAlias,llUpdCTkt
lcWorkFile = IIF(llUpdTmpAl,lcTempAloc,lcAlocated)

lnAlias    = SELECT()
lcTempCurs = gfTempName()

SELECT (lcWorkFile)

=AFIELDS(laFileStru)
CREATE CURSOR (lcTempCurs) FROM ARRAY laFileStru
SELECT (lcWorkFile)
SCAN FOR CORDLINE = STR(&lcOrdLine..LineNo,6)
  SCATTER TO laAlocated
  INSERT INTO (lcTempCurs) FROM ARRAY laAlocated
ENDSCAN
SELECT (lcTempCurs)
GO TOP
*-- Update our Field.
REPLACE Qty&lcQty WITH lnQty&lcQty,;
        TOtQty With Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8

llUpdAlo = .T.
SET ORDER TO TAG CUTPKORD IN (lcWorkFile)

IF 'MF' $ gcCmpModules
  =gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
ENDIF

SELECT (lcTempCurs)
SCAN
  =SEEK(TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE,lcWorkFile)
  llUpdCTkt = (TranCd='1' .AND. SEEK(CTKTNO,'CUTTKTH') .OR. ;
              TranCd='2' .AND. SEEK('P'+CTKTNO,'POSHDR'))
  SELECT (lcOrdHdr)
  =RLOCK()
  REPLACE TotCut WITH TotCut - &lcWorkFile..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize
  UNLOCK
  SELECT (lcOrdLine)
  =RLOCK()
  REPLACE Cut&lcEditSize WITH Cut&lcEditSize - ;
          &lcWorkFile..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize ,;
          TotCut WITH TotCut - &lcWorkFile..Qty&lcEditSize + &lcTempCurs..Qty&lcEditSize
  UNLOCK
  SELECT (lcWorkFile)
  =RLOCK()
  REPLACE Qty&lcEditSize WITH &lcTempCurs..Qty&lcEditSize ,;
          TotQty         WITH &lcTempCurs..TotQty 
  IF llUpdCTkt .AND. !(lcEditSize $ cUpdSizes)
    REPLACE cUpdSizes WITH ALLTRIM(cUpdSizes)+lcEditSize
  ENDIF
  IF !llUpdCTkt .AND. (lcEditSize $ cUpdSizes)
    REPLACE cUpdSizes WITH STRTRAN(cUpdSizes,lcEditSize,'')
  ENDIF
  UNLOCK
ENDSCAN

IF 'MF' $ gcCmpModules
  =gfCloseFile('CUTTKTH')
ENDIF

SET ORDER TO TAG CUTORD IN (lcWorkFile)
LOCATE
SCATTER MEMVAR MEMO

SELECT (lnAlias)
RETURN(llUpdAlo)

*-- End OF lfEditAlo
*:*************************************************************
*: Name      : lfGetPOLn
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : get the po lines form the Poheder and line.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            :  None.
*:*************************************************************
*: Example            : = lfGetPOLn()
*:*************************************************************
*:
FUNCTION lfGetPOLn
PRIVATE lnAlias , llReturnSt

lnAlias = Select(0)
SELECT Poshdr
llReturnSt = .F.
IF SEEK('P'+lcSvdPo) .AND. STATUS = 'H'
  llReturnSt = .T.
  SCATTER MEMVAR MEMO
  INSERT INTO (lcPOHdr) FROM MEMVAR

  IF EMPTY(Store)
    lnShpLoc = 1 
  ELSE
    lnShpLoc = 2
  ENDIF
  
  *-- Get the lines.
  SELECT POSLN
  = SEEK('P'+lcSvdPo)
  SCAN REST WHILE  cstytype+po+style+STR(lineno,6)+trancd = 'P'+lcSvdPo
    SCATTER TO laPolin
    INSERT INTO (lcPOLine) FROM ARRAY laPolin
  ENDSCAN
ENDIF


SELECT (lnAlias)

RETURN llReturnSt

*-- End OF lfGetPOLn
*:*************************************************************
*: Name      : lfVPoLnHdr
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Update the PosHdr & Posline files.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: Returns            : None.
*:*************************************************************
*: Example            : =lfVPoLnHdr()
*:*************************************************************
*:
FUNCTION lfVPoLnHdr
PRIVATE lnOldAlas

lnOldAlas =  SELECT (0)

*-- go to header record.
SELECT (lcPOHdr)
=SEEK('P'+lcSvdPo)
REPLACE NiCost1   WITH NiCost1 + (Style.niCost1 * M.Qty&lcQty),;
        NiCost2   WITH NiCost2 + (Style.niCost2 * M.Qty&lcQty),;
        NiCost3   WITH NiCost3 + (Style.niCost3 * M.Qty&lcQty),;
        NiCost4   WITH NiCost4 + (Style.niCost4 * M.Qty&lcQty),;
        NiCost5   WITH NiCost5 + (Style.niCost5 * M.Qty&lcQty),;
        Pototal   WITH NiCost1+NiCost2+NiCost3+NiCost4+NiCost5,;
        nStyOrder WITH nStyOrder - lnOldQty&lcQty +M.Qty&lcQty,;
        Open      WITH Open      - lnOldQty&lcQty +M.Qty&lcQty,;
        TOtOrd    WITH TOTOrd    - lnOldQty&lcQty +M.Qty&lcQty,;
        nFCost1   WITH NiCost1                                ,;
        nFCost2   WITH NiCost2                                ,;
        nFCost3   WITH NiCost3                                ,;
        nFCost4   WITH NiCost4                                ,;
        nFCost5   WITH NiCost5

SELECT (lcPOLine)
IF SEEK(M.Style+STR(m.LineNo,6))
  REPLACE Qty&lcQty WITH M.Qty&lcQty,;
          TotQty    WITH TotQty - lnOldQty&lcQty +M.Qty&lcQty,;
          Ord&lcQty WITH M.Qty&lcQty,;
          TotOrd    WITH TotOrd - lnOldQty&lcQty +M.Qty&lcQty
ELSE
  APPEND BLANk
  REPLACE cStyType  WITH 'P'     ,;
          Trancd    WITH '1'     ,;
          Style     WITH M.Style ,;
          LineNo    WITH M.LineNo,;
          Po        WIth lcSvdPo ,;
          Vendor    With &lcPOHdr..Vendor,;
          Scale     WITH M.Scale         ,;
          Qty&lcQty WITH M.Qty&lcQty     ,;
          TotQty    WITH TotQty - lnOldQty&lcQty +M.Qty&lcQty,;
          Ord1      WITH M.Qty1          ,;
          Ord2      WITH M.Qty2          ,;
          Ord3      WITH M.Qty3          ,;
          Ord4      WITH M.Qty4          ,;
          Ord5      WITH M.Qty5          ,;
          Ord6      WITH M.Qty6          ,;
          Ord7      WITH M.Qty7          ,;
          Ord8      WITH M.Qty8 

  REPLACE TotOrd    WITH TotOrd - lnOldQty&lcQty +M.Qty&lcQty,;
          nCost1    WITH Style.niCost1   ,;
          nCost2    WITH Style.niCost2   ,;
          nCost3    WITH Style.niCost3   ,;
          nCost4    WITH Style.niCost4   ,;
          nCost5    WITH Style.niCost5   ,;
          cWareCode WITH &lcPOHdr..cWareCode,;
          nEcost1   WITH Style.niCost1   ,;
          nEcost2   WITH Style.niCost2   ,; 
          nEcost3   WITH Style.niCost3   ,;
          nEcost4   WITH Style.niCost4   ,;
          nEcost5   WITH Style.niCost5   ,;
          cStygrade WITH Style.cStygrade ,;
          dAdd_date WITH gdSysDate       ,;
          cAdd_time WITH TIME()          ,;
          cAdd_user WITH gcUser_id
          
          
ENDIF
SELECT(lnOldAlas)
*-- End OF lfVPoLnHdr
*:*************************************************************
*: Name      : lfCrUnSess
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Create uncomplete session record.
*:*************************************************************
*: Call      : lfUpdVars()
*:*************************************************************
*: Passed Parameters  :lcPushBt  -> push botton
*:                     lcUnCmSta -> status
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfCrUnSess()
*:*************************************************************
*:
FUNCTION lfCrUnSess
PARA lcPushBt,lcUnCmSta

lnAlias = SELECT()
SELECT UNCMSESS
IF lcUnCmSta='O'
  IF !SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
    IF !SEEK('I'+lcUnCmPgId+PADR(gcUser_id,10)) 
	  lcSvdPo    = " "
      INSERT INTO UNCMSESS ;
      (Status,cUTranType,cUserId,cSession,cProgram,cCurrScr,dTranDate,cTranTime,cCurrObj) VALUES ;
      ('O',lcUnCmPgId,gcUser_id,lcSession,lcAProgm,'',gdSysDate,TIME(),lcPushBt)
    ELSE
	  lcSvdPo    = " "
      REPLACE Status    WITH 'O',;
              cSession  WITH lcSession,;
              dTranDate WITH gdSysDate,;
              cTranTime WITH TIME(),;
              ccurrobj  WITH lcPushBt
    ENDIF
  ELSE
    REPLACE cCurrObj WITH lcPushBt
  ENDIF
  =lfUpdVars(.F.)
  =RLOCK('UNCMSESS')

ELSE  && lcUnCmSta $ 'IC'
 
  *--Cancel OR Complete unCmSess record.
  IF SEEK('O'+lcUnCmPgId+PADR(gcUser_id,10)+lcSession)
    REPLACE Status WITH lcUnCmSta
    UNLOCK
  ENDIF
ENDIF
SELECT(lnAlias)
RETURN

*-- End OF lfCrUnSess 
*:*************************************************************
*: Name      : lfUpdVars
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Function to create a string to be saved in the field
*:             UNCMSESS.mComent and UNCMSESS.mTmpFiles
*:*************************************************************
*: Call      : gfSavSess()
*:*************************************************************
*: Passed Parameters  :llUpdCutPik-> .T. for Update cutpick file.
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfUpdVars()
*:*************************************************************
*:
FUNCTION lfUpdVars
PARA llUpdCutPik
IF llUpdCutPik
  lcFiles = "lcPOHdr,"+lcPOHdr+","+lcPOHdr+";lcPOLine,"+lcPOLine+","+lcPOLine+;
            ";lcTmpCtPk,"+lcTmpCtPk+","+lcTmpCtPk+";"
ELSE
  lcFiles = "lcPOHdr,"+lcPOHdr+","+lcPOHdr+";lcPOLine,"+lcPOLine+","+lcPOLine+";"
ENDIF
= gfSavSess(lcUnCmPgId, lcFiles, @laVariables,lcSession)
RETURN

*-- End OflfUpdVars
*:*************************************************************
*: Name      : lfvEquCost
*: Developer : Abdou Elgendy
*: Date      : 11/07/2002
*: Purpose   : Get eguevalent cost by pathing the foreign.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lf..()
*:*************************************************************
*:
FUNCTION lfvEquCost
PARAMETERS lcCstNo,lnFrnCost,lnCurRate,lnCurUnt

lnCstType = lcIType&lcCstNo
IF lnCstType $ 'PMD'
  IF lnCstType='P'
    lcPMethod = gfGetExSin(@lcPUnMeth,laData[26])
    lcPMethod = IIF(EMPTY(lcPMethod),'*',lcPMethod)
    lcPUnMeth = IIF(EMPTY(lcPUnMeth),'/',lcPUnMeth)
    lnEquCost = lnFrnCost &lcPMethod lnCurRate &lcPUnMeth lnCurUnt
  ELSE
    lcDMethod = gfGetExSin(@lcDUnMeth,laData[27])
    lcDMethod = IIF(EMPTY(lcDMethod),'*',lcDMethod)
    lcDUnMeth = IIF(EMPTY(lcDUnMeth),'/',lcDUnMeth)
    lnEquCost = lnFrnCost &lcDMethod lnCurRate &lcDUnMeth lnCurUnt
  ENDIF
ELSE
  lnEquCost = lnFrnCost
ENDIF
lnEquCost=ROUND(lnEquCost,3)
RETURN (lnEquCost)

*-- End OF lfvEquCost.
*:*************************************************************