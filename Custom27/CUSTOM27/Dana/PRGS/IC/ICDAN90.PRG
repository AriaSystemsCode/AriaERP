****************************************************************************
*: Program file      : ICDAN90.PRG (C#101783)
*: Program desc.     : CUSTOMIZED INVENTORY ADJUSTMENT PROGRAM FOR DANA UNDIES
*: System            : Aria Apparel System (A27).
*: Module            : Inventory Control (IC)
*: Developer         : Khalid Mohi El-Din Mohamed KHM
*: Date              : 05/29/2000
*:**************************************************************************

*--Program variables initialization.
lcType = 'A'
DIMENSION laTStk[9],laOTStk[9],laAdjCode[1],laCodInfo[1,10],laOpFile[4],;
          laConvQty[9]
STORE 0   TO laTStk,laOTStk,lnNewCost,lnOldCost,laConvQty
STORE ' ' TO lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,;
             lcTmpAdj,lcTmpGlDt,lcTemLoc
STORE ''  TO lcFromWare,lcToWare,lcWFromDsc,lcWToDsc,lcReason,;
             lcDyelot,lcGlFYear,lcGlPeriod,lcWareAny,lcDefWareh,;
             lcAReason,lcRefer,lcOldValue
STORE .F. TO llDyelot,llAvrCost,llWareLoc,llMultiWH,llGlLink
STORE .F. TO llModiMod,llBrowse,llDyeLvl,llGoOut,llLoc,llCUpdate,llExtMFlg
STORE ' ' TO lcStyHdr,lcStyPict,lcStyDesc,laAdjCode,laCodInfo,lcCostMth
STORE ' ' TO lcHldTab,lcHldBtb,lcHldEsc,lcHldAtB,lcBtMode,lcBtNMod
STORE 0   TO lnstylewid,lnMjrWid
STORE gdSysDate TO ldPstDate,ldDate
lnAdjCode =  1
llChkDate = .T.

IF !gfSetup()
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)
  *-- Get the setup variables from the memory files.
  llDyelot   = gfGetMemVar('M_Dyelot')   ='Y'
  lcCostMth  = gfGetMemVar('M_Cost_Meth')
  llWareLoc  = gfGetMemVar('M_WareLoc')  ='Y'
  llMultiWH  = gfGetMemVar('M_WareHouse')='Y'
  llGlLink   = gfGetMemVar('M_Link_GL')  ='Y'
  
  lcStyHdr   = gfItemMask('HI')
  lcStyPict  = gfItemMask('PI')
  lnstylewid = LEN(lcStyPict)
  lnMjrWid   = LEN(gfItemMask('PM'))
  
  IF llGlLink
    SELECT CODES
    lcCodTag=TAG()
    SET ORDER TO TAG cCode_no
    IF !SEEK("N"+'CADJREASON','CODES')
      *--You have to edit the Adjustment reasons codes first, Cannot proceed.
      =gfModalGen('TRM42111B42001','DIALOG')
      SET ORDER TO TAG &lcCodTag
      SELECT INVTADJ
      RETURN 
    ENDIF  
    SET ORDER TO TAG &lcCodTag
  ENDIF

  *--Program screen windows.
  lcWinCh0    = gfTempName()
  lcWinCh1    = gfTempName()
  lcWinCh2    = gfTempName()
  lcWinCh3    = gfTempName()
  SELECT STYLE
  SET ORDER TO TAG Style

  SELECT INVTADJ
  *-- Create tmp adjustment file.
  lcTmpAdj = gfTempName()
  =AFIELDS(laFStru)
  lnNo1=ASCAN(laFStru,'UNT_COST')
  lnNo2=ASCAN(laFStru,'OLD_COST')
  *--Make the lenth of this two fields as ave_cost field.
  STORE 15 TO laFStru(lnNo1+2),laFStru(lnNo2+2)
  STORE  7 TO laFStru(lnNo1+3),laFStru(lnNo2+3)
  lnFStru = ALEN(laFStru,1)
  DIMENSION laFStru[lnFStru+11,4]  
  laFStru[lnFStru+1,1] = 'cAdjReason'
  laFStru[lnFStru+1,2] = 'C'
  laFStru[lnFStru+1,3] = 6
  laFStru[lnFStru+1,4] = 0
  laFStru[lnFStru+2,1] = 'cRefer'
  laFStru[lnFStru+2,2] = 'C'
  laFStru[lnFStru+2,3] = 6
  laFStru[lnFStru+2,4] = 0  
  laFStru[lnFStru+3,1] = 'ConvQty1'
  laFStru[lnFStru+3,2] = 'N'
  laFStru[lnFStru+3,3] = 6
  laFStru[lnFStru+3,4] = 0
  laFStru[lnFStru+4,1] = 'ConvQty2'
  laFStru[lnFStru+4,2] = 'N'
  laFStru[lnFStru+4,3] = 6
  laFStru[lnFStru+4,4] = 0
  laFStru[lnFStru+5,1] = 'ConvQty3'
  laFStru[lnFStru+5,2] = 'N'
  laFStru[lnFStru+5,3] = 6
  laFStru[lnFStru+5,4] = 0
  laFStru[lnFStru+6,1] = 'ConvQty4'
  laFStru[lnFStru+6,2] = 'N'
  laFStru[lnFStru+6,3] = 6
  laFStru[lnFStru+6,4] = 0
  laFStru[lnFStru+7,1] = 'ConvQty5'
  laFStru[lnFStru+7,2] = 'N'
  laFStru[lnFStru+7,3] = 6
  laFStru[lnFStru+7,4] = 0
  laFStru[lnFStru+8,1] = 'ConvQty6'
  laFStru[lnFStru+8,2] = 'N'
  laFStru[lnFStru+8,3] = 6
  laFStru[lnFStru+8,4] = 0
  laFStru[lnFStru+9,1] = 'ConvQty7'
  laFStru[lnFStru+9,2] = 'N'
  laFStru[lnFStru+9,3] = 6
  laFStru[lnFStru+9,4] = 0
  laFStru[lnFStru+10,1] = 'ConvQty8'
  laFStru[lnFStru+10,2] = 'N'
  laFStru[lnFStru+10,3] = 6
  laFStru[lnFStru+10,4] = 0
  laFStru[lnFStru+11,1] = 'ConvTotQty'
  laFStru[lnFStru+11,2] = 'N'
  laFStru[lnFStru+11,3] = 6
  laFStru[lnFStru+11,4] = 0
  CREATE DBF (gcWorkDir+lcTmpAdj) FROM ARRAY laFStru
  lcExpr=IIF(llDyelot,'Style+Dyelot+STR(RECNO(),6)','Style+STR(RECNO(),6)')
  INDEX ON &lcExpr TAG &lcTmpAdj
  SET RELATION TO Style INTO STYLE

  *-Open file ,prepare arrays and creat cursor and set the index. 
  IF llWareLoc 
    DIMENSION laSource[1],laTarget[1]
    IF !laOpFile[1]
      laOpFile[1]=gfOpenFile(gcDataDir+'WhsLoc','WhsLocSt','SH')
    ENDIF
    SELECT WhsLoc
    =AFIELDS(laStru)
    lcTemLoc  = gfTempName()   
    CREATE DBF (gcWorkDir+lcTemLoc) FROM ARRAY laStru
    INDEX ON STYLE+CWARECODE+CLOCATION TAG (lcTemLoc) OF (gcWorkDir+lcTemLoc)
    lcWareAny = SPACE(06)
  ENDIF  

  IF !llMultiWH
    *--Read the default warehouse.
    GO TOP IN WAREHOUS
    lcDefWareh = WAREHOUS.cWareCode
  ENDIF
    
  lcFromWare = IIF(llMultiWH, SPACE(6), lcDefWareh )
  
  DIMENSION laTStk[9],laOTStk[9],laConvQty[9]
  STORE 0 TO laTStk,laOTStk,laConvQty

  IF llGlLink
    laCodInfo[1,01] = "CADJREASON"
    laCodInfo[1,02] = "laAdjCode"
    laCodInfo[1,03] = "lnAdjCode"
    laCodInfo[1,04] = ""
    laCodInfo[1,05] = .F.
    laCodInfo[1,06] = .F.
    laCodInfo[1,07] = "&lcTmpAdj"
    laCodInfo[1,08] = "&lcTmpAdj"
    IF llDyelot
      laCodInfo[1,09] = "m.Style + m.Dyelot"
    ELSE
      laCodInfo[1,09] = "m.Style"
    ENDIF
    laCodInfo[1,10] = "cAdjReason"
  ENDIF 

  llNoShow = .F.

ENDIF

EXTERNAL ARRAY laDefProc
laDefProc[9]  = .F.      && Disable the control panel save proc.  (lpSavScr)
laDefProc[10] = .F.      && Disable the control panel close proc. (lpClsScr)

*-- Declare the globle variables.
lcBrow_Ttl = "Transaction lines"
lcLevel    = IIF(!llDyelot AND !llMultiWH,'Style','StyDye')

SELECT (lcTmpAdj)
lnCurrRec = RECNO()

PUSH KEY
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrow_Ttl)
DO (gcScrDir+gcWinAppl+"\ICDAN90A.SPX")
RELEASE WINDOW (lcBrow_Ttl)
POP KEY

*--Normal exit.
IF glQuitting
  SELECT (lcTmpAdj)
  USE
  ERASE (gcWorkDir+lcTmpAdj+'.DBF')
  ERASE (gcWorkDir+lcTmpAdj+'.CDX')
  IF USED(lcTmpGlDt)
    USE IN (lcTmpGlDt)
    ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
  ENDIF
  IF llWareLoc 
    SELECT (lcTemLoc)
    USE
    ERASE (gcWorkDir+lcTemLoc+'.DBF')
    ERASE (gcWorkDir+lcTemLoc+'.CDX')
  ENDIF
  IF laOpFile[1] AND USED('WhsLoc')
    =gfCloseFile('WhsLoc')
  ENDIF  
  IF laOpFile[2] AND USED('GLDist')
    =gfCloseFile('GLDist')
  ENDIF  
  IF laOpFile[3] AND USED('StyInvJl')
    =gfCloseFile('StyInvJl')
  ENDIF
  IF laOpFile[4] AND USED('DanaConv')
    =gfCloseFile('DanaConv')
  ENDIF  
ENDIF
RETURN

*!*************************************************************
*! Name      : lpShow
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Show objects function.
*!*************************************************************
*! Example   :  =lpShow()
*!*************************************************************
FUNCTION lpShow
laCtrStat[10] = 'DISABLE'
SHOW GET pbBrws DISABLE

DO CASE
  CASE laScrMode[1]

    IF !llMultiWH AND !llGlLink
      lcBtNMod = 'ENABLE'
    ELSE
      lcBtNMod = 'DISABLE'
    ENDIF
    
    IF llMultiWH
      SHOW GET lcFromWare ENABLE
      SHOW GET ibWCode1   ENABLE
      IF !llGlLink
        _CUROBJ = OBJNUM(lcFromWare)
      ENDIF
    ELSE
      IF !llGlLink
        _CUROBJ = OBJNUM(pbNew)
      ENDIF
    ENDIF
    IF llGlLink
       SHOW GET ldPstDate  ENABLE    
       IF llChkDate
        _CUROBJ = OBJNUM(ldPstDate)
       ELSE
         IF llMultiWH
          _CUROBJ = OBJNUM(lcFromWare)
         ELSE
          _CUROBJ = OBJNUM(pbNew)
         ENDIF
       ENDIF
    ENDIF
    SHOW GET pbKey      DISABLE
    lcBtMode = 'DISABLE'

  CASE laScrMode[4]  
    SELECT (lcTmpAdj)
    lnSavRec=IIF(!EOF(),RECNO(),1)
    LOCATE
    IF !FOUND()
      laCtrStat[11] = 'DISABLE'
      SHOW GET pbSav      DISABLE
      SHOW GET lcFromWare ENABLE
      SHOW GET ibWCode1   ENABLE
      SHOW GET ldPstDate  ENABLE    
      SHOW GET pbKey      DISABLE
      lcBtMode = 'DISABLE'
      IF !EMPTY(lcFromWare)
        lcBtNMod = 'ENABLE'
      ELSE
        lcBtNMod = 'DISABLE'
      ENDIF
    ELSE
      laCtrStat[11] = 'ENABLE'
      SHOW GET pbSav      ENABLE
      SHOW GET lcFromWare DISABLE
      SHOW GET ibWCode1   DISABLE
      SHOW GET ldPstDate  DISABLE
      SHOW GET pbKey      ENABLE
      lcBtNMod = 'ENABLE'
      lcBtMode = 'ENABLE'
      IF lnSavRec<>1
        GOTO lnSavRec
      ENDIF
    ENDIF  
ENDCASE
SHOW GET pbNew    &lcBtNMod
SHOW GET pbModify &lcBtMode
SHOW GET pbRemove &lcBtMode

RETURN

*!*************************************************************
*! Name      : lfActBrow()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Function to activate the browse window.
*!*************************************************************
*! Example   :  =lfActBrow()
*!*************************************************************
FUNCTION lfActBrow

SELECT (lcTmpAdj)
IF llDyelot
  lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
               "STYLE        :30 :H=lcStyHdr :R ,"+;
               "STYLE.Desc   :24 :H='Description' :R,"+;
               "Dyelot       :19 :H='Dyelot':R,"+;
               "TotOld       :10  :H='Existing':R,"+;
               "Old_Cost     :09:P='999999.99'  :H='Old Cost':R,"+;
               "TotAdj       :10  :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
               "Unt_Cost     :09:P='999999.99'  :H='New Cost':R,"+;
               "Total=IIF(lcType='P',TotAdj,(TotOld+ConvTotQty)) :10:P='9999999' :H='  Total':R "
ELSE  
  lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
               "STYLE       :30 :H=lcStyHdr :R,"+;
               "STYLE.Desc  :24 :H='Description':R,"+;
               "TotOld      :12 :H='Existing':R,"+;
               "Old_Cost    :09:P='999999.99' :H='Old Cost':R,"+;
               "TotAdj      :12 :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
               "Unt_Cost    :09:P='999999.99' :H='New Cost':R,"+;
               "Total=IIF(lcType='P',TotAdj,(TotOld+ConvTotQty)) :12 :P='9999999' :H='  Total':R "
ENDIF

lcBrowWind=IIF(!llMultiWH AND !llGlLink,lcWinCh3,lcWinCh1)
BROWSE FIELDS &lcBfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       TITLE (lcBrow_Ttl) ;
       WHEN lfwBrow() ;
       VALID :F lfvBrows();
       WINDOW (lcBrowWind);
       IN WINDOW (gcBaseWind)
RETURN

*!*************************************************************
*! Name      : lfwBrow
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : When Browse function.
*!*************************************************************
*! Example   :  =lfwBrow()
*!*************************************************************
FUNCTION lfwBrow
lnCurrRec = RECNO()
SHOW WINDOW (lcBrow_Ttl) REFRESH
RETURN

*!*************************************************************
*! Name      : lfvBrows
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Valid Browse function.
*!*************************************************************
*! Example   :  =lfvBrows()
*!*************************************************************
FUNCTION lfvBrows

IF WONTOP() # (lcBrow_Ttl)
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfReadAct
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Read Activate function clear traping.
*!*************************************************************
*! Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct

ON KEY LABEL TAB
ON KEY LABEL BACKTAB

*:*************************************************************
*! Name      : lfTrapKy
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Trap key.
*!*************************************************************
*! Example   :  =lfTrapKy()
*!*************************************************************
FUNCTION lfTrapKy

IF WONTOP() = lcBrow_Ttl
  ON KEY LABEL TAB     DO lptab
  ON KEY LABEL backtab DO lpshifttab
ENDIF  
RETURN

*!*************************************************************
*! Name      : lfUnTrapKeys
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Releases Key traps necessary for browse.
*!*************************************************************
*! Example   :  =lfUnTrapKeys()
*!*************************************************************
FUNCTION lfUnTrapKeys

ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL ESC DO lpDetEsc
RETURN

*!*************************************************************
*! Name      : lfActBrWin
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : key trapping for activate browse.
*!*************************************************************
*! Example   :  =lfActBrWin()
*!*************************************************************
FUNCTION lfActBrWin 

IF !WONTOP(lcBrow_Ttl)
  ACTIVATE WINDOW (lcBrow_Ttl)
ENDIF
RETURN

*!*************************************************************
*! Name      : lpTab
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Tab key trapping.
*!*************************************************************
*! Example   :  DO lpTab
*!*************************************************************
PROCEDURE lpTab 

IF WONTOP(lcBrow_Ttl)
  _CUROBJ=OBJNUM(pbNew)
  ACTIVATE WINDOW (lcWinCh2)
ELSE
  _CUROBJ=_CUROBJ+1
ENDIF
RETURN

*!*************************************************************
*! Name      : lpShiftTab
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Shift Tab key trapping.
*!*************************************************************
*! Example   :  DO lpShiftTab
*!*************************************************************
PROCEDURE lpShiftTab 

IF WONTOP(lcBrow_Ttl)
  ACTIVATE WINDOW ("gwccontrl1")
  _CUROBJ=OBJNUM(pbCls)
ENDIF
RETURN

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
*! Example   :  =lfoldvalue()
*!*************************************************************
FUNCTION lfoldvalue

lcOldValue = EVALUATE(SYS(18))
RETURN

*!*************************************************************
*! Name      : lfvWareH()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Validate the warehouses.
*!*************************************************************
*! Parameters: lcWareH  -> Warehouse code.
*!             lnWareNm -> Which warehouse ,1st or 2nd.
*!*************************************************************
*! Example   :  =lfvWareH()
*!*************************************************************
FUNCTION lfvWareH
PARA lcWareH,lnWareNm

IF MDOWN()
  RETURN
ENDIF

PRIVATE lcWareH,lcWareDesc

IF llBrowse .OR. (EMPTY(lcWareH) AND laScrMode[4] ) .OR.;
                 (!EMPTY(lcWareH) AND !SEEK(lcWareH,'WareHous') )
  = gfBrowWare(.F.)
  lcWareH = WareHous.cWareCode
ENDIF
lcWareDesc = IIF(!EMPTY(lcWareH),WareHous.cDesc,' ')
llBrowse = .F.

IF lnWareNm = 1
  lcFromWare = lcWareH
  lcWFromDsc = lcWareDesc
ELSE
  lcToWare   = lcWareH
  lcWToDsc   = lcWareDesc
ENDIF

IF !EMPTY(lcFromWare)
  laScrMode[1] = .F.
  laScrMode[4] = .T.
  llCUpdate = .T.
  SHOW GETS   
ENDIF 
=lfRefresh()
RETURN

*:*************************************************************
*! Name    : lfPstDate
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose : Valid Posting date.
*!*************************************************************
*! Example   :  =lfPstDate()
*!*************************************************************
FUNCTION lfPstDate

*-- If the user pressed "Enter" and did not use the mouse..
*-- We usually use this condition to validate all the key fields..

IF !MDOWN()
  IF ldPstDate<>lcOldValue
    llChkDate=.T.
  ENDIF
  IF llChkDate AND !CHECKPRD(ldPstDate,'lcGLFYear','lcGLPeriod',IIF(lcType='P','IP','IA'))
    _CUROBJ = OBJNUM(ldPstDate)
    RETURN
  ELSE
    IF !llMultiWH
      lcBtNMod = 'ENABLE'
      SHOW GET pbNew    &lcBtNMod
    ENDIF  
  ENDIF
  llChkDate = .F.
ENDIF
RETURN

*:*************************************************************
*! Name    : lfVldDate
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose : Valid Transaction date.
*!*************************************************************
*! Example   :  =lfVldDate()
*!*************************************************************
FUNCTION lfVldDate

IF llGlLink AND ldDate>ldPstDate
  *--The xxxx date cannot be after than the posting date.
  = gfModalGen('TRM42106B42000','DIALOG','adjustment')
  _CUROBJ=OBJNUM(ldDate)
  RETURN
ENDIF

*:*************************************************************
*! Name    : lfHoldKey
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose : Save and restore traping.
*!*************************************************************
*! Example   :  =lfHoldKey()
*!*************************************************************
FUNCTION lfHoldKey
PARA lcKyOper

DO CASE
  CASE lcKyOper='S'
    lcHldTab = ON('KEY','TAB')
    lcHldBtb = ON('KEY','BACKTAB')    
    lcHldEsc = ON('KEY','ESC')
    lcHldAtB = ON('KEY','ALT+B')
  CASE lcKyOper='R'
    ON KEY LABEL TAB     &lcHldTab
    ON KEY LABEL BACKTAB &lcHldBtb
    ON KEY LABEL ESC     &lcHldEsc
    ON KEY LABEL ALT+B   &lcHldAtB
ENDCASE

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Cancel P/O.
*!*************************************************************
*! Example   :  =lpClsScr()
*!*************************************************************
FUNCTION lpClsScr

SELECT (lcTmpAdj)
GO TOP
IF !EOF()
  ZAP
  lnCurrRec=RECNO()
  GO TOP
  =lfActBrow()
ENDIF
STORE ' ' TO lcToWare,lcWFromDsc,lcWToDsc,lcReason,lcDyelot,;
             lcGlFYear,lcGlPeriod,lcWareAny,lcAReason,lcRefer
STORE 0   TO laTStk,laOTStk,lnNewCost,lnOldCost,laConvQty

lcFromWare = IIF(llMultiWH, SPACE(6), lcDefWareh )
llLoc  = .F.
laScrMode = .F.
laScrMode[1] = .T.

llCUpdate = .F.
SHOW GETS
=lfRefresh()
RETURN

*!*************************************************************
*! Name      : lfvNew()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Add new lines.
*!*************************************************************
*! Example   :  =lfvNew()
*!*************************************************************
FUNCTION lfvNew

llModiMod = .F.
llExtMFlg = .F.
SELECT (lcTmpAdj)
lcScFields = 'Style,Dyelot,Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj'
SCATTER FIELDS &lcScFields MEMO MEMVAR BLANK
STORE 0 TO m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk,;
           laConvQty
IF lcType = 'A'
  STORE 0 TO laTStk,laConvQty
ENDIF

m.Style = SPACE(19)
ldDate  = gdSysDate
lcStyDesc = ' '
llLoc = .F.

=lfHoldKey('S')
=lfUnTrapKeys()
DO (gcScrDir+gcWinAppl+"\ICDAN900.SPX")
=lfHoldKey('R')

SELECT (lcTmpAdj)
IF EOF()
  GO TOP
ENDIF
lnCurrRec = RECNO()
GO TOP
=lfActBrow()

IF !EOF()
  GOTO lnCurrRec
  _CUROBJ = OBJNUM(pbNew)

  IF !llMultiWH
    laScrMode = .F.
    laScrMode[4] = .T.
    llCUpdate = .T.
  ENDIF

ELSE
  
  IF !llMultiWH
    laScrMode = .F.
    laScrMode[1] = .T.
    llCUpdate = .F.
  ENDIF

ENDIF
SHOW GETS
IF !llMultiWH AND laScrMode[1]
  _CUROBJ = OBJNUM(ldPstDate)
ENDIF  
RETURN

*!*************************************************************
*! Name      : lpDetEsc
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Trap Esc for lines entry.
*!*************************************************************
*! Example   :  DO lpDetEsc
*!*************************************************************
PROCEDURE lpDetEsc
_CUROBJ = OBJNUM(pbNClose)
KEYBOARD '{ENTER}'
RETURN

*!*************************************************************
*! Name      : lfvStyle()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Valid function to validate style field.
*!*************************************************************
*! Example   :  =lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

IF MDOWN() OR (!llBrowse AND EMPTY(SUBSTR(m.Style,1,lnMjrWid)) )
  RETURN
ENDIF

SELECT STYLE 
m.Style=IIF(llBrowse ,'?',m.Style)
IF llBrowse OR (!EMPTY(m.Style) AND !SEEK(m.Style,'STYLE'))
  lcHdEsc1= ON('KEY','ESC')
  ON KEY LABEL ESC 
  m.Style = gfStyBrw('I',m.Style,'',.F.)
  ON KEY LABEL ESC &lcHdEsc1
  llbrowse = .F.
  IF EMPTY(m.Style)
    _CUROBJ = OBJNUM(m.Style)
    RETURN
  ENDIF
ENDIF

IF EMPTY(m.Style)
  =lfClrInfo()
  RETURN
ENDIF

IF !llDyelot OR Style.cDye_Flg<>'Y'
  =lfGetInfo()
ELSE
  lcStyDesc = Desc1
  SHOW GET ibDye    ENABLE
  SHOW GET m.Dyelot ENABLE
  =lfRefresh('ICDAN900')
ENDIF

IF !EMPTY(m.Style)
  IF !laOpFile[4]
    = gfOpenFile (gcDataDir+'DanaConv','Group','SH')
  ENDIF  
  IF !SEEK(ALLTRIM(Style.cStyGroup),'DanaConv')
    =gfModalGen('TRM00000B00000',.F.,.F.,.F.,"Style - Color : "+;
                ALLTRIM(m.Style)+" has no group. Cannot proceed.")
    m.Style = Style
    SHOW GET m.Style ENABLE
    SHOW GET ibStyle ENABLE
    =lfClrInfo()
    _CUROBJ = OBJNUM(m.Style)
    RETURN 
  ENDIF  
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvDyelot()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Valid function to validate the dyelot.
*!*************************************************************
*! Calls     : DO gpStDyBrow WITH lcStyle,lcWareHous,lcDyelot
*!*************************************************************
*! Example   :  =lfvDyelot()
*!*************************************************************
FUNCTION lfvDyelot

IF MDOWN() OR (!llBrowse AND EMPTY(m.Dyelot) )
  RETURN
ENDIF

lcDyelot=IIF(llBrowse ,'?',m.Dyelot)
IF llBrowse OR (!EMPTY(m.lcDyelot) AND !SEEK(PADR(m.Style,19)+lcFromWare+lcDyelot,'STYDYE'))
   DO gpStDyBrow WITH m.Style,lcFromWare,lcDyelot
ENDIF

llBrowse = .F.
IF EMPTY(lcDyelot)
  m.Dyelot = ' '
  SHOW GET ibDye    DISABLE
  SHOW GET m.Dyelot DISABLE
  _CUROBJ = OBJNUM(m.Style)
  RETURN
ENDIF

m.Dyelot=lcDyelot
=SEEK(PADR(m.Style,19)+lcFromWare+lcDyelot,'STYDYE')
laScrMode=.F.
laScrMode[4]=.T.

=lfGetInfo()
_CUROBJ = OBJNUM(ldDate)
=lfRefresh('ICDAN900')
SHOW GET ibDye    DISABLE
SHOW GET m.Dyelot DISABLE

RETURN


*!*************************************************************
*! Name      : lfGetInfo()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Get the style key accepted information.
*!*************************************************************
*! Example   :  =lfGetInfo()
*!*************************************************************
FUNCTION lfGetInfo

SELECT STYLE
SEEK m.Style
lcSScale  = Scale
lcStyDesc = Desc1
SELECT (lcLevel)
IF lcLevel<>'Style'
    
  IF !SEEK(PADR(m.Style,19)+lcFromWare+SPACE(10) ) .AND. llMultiWH
    IF lcType = 'P'
      *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
      lnChoice=gfModalGen('TRM42056B42006','DIALOG',ALLTRIM(m.Style)+'|'+lcFromWare)
    ELSE
      *-Style: xxx is not assigned to warehouse: xxx,"\<Reenter"
      =gfModalGen('TRM42056B42005','DIALOG',ALLTRIM(m.Style)+'|'+lcFromWare)
      lnChoice = 2
    ENDIF
    IF lnChoice = 1
      DO gpAdStyWar WITH m.Style,SPACE(10),lcFromWare
    ELSE
      m.Style = Style
      SHOW GET m.Style ENABLE
      SHOW GET ibStyle ENABLE
      =lfClrInfo()
      _CUROBJ = OBJNUM(m.Style)
      RETURN 
    ENDIF
  ENDIF
ENDIF

SELECT (lcLevel)

IF lcType='A'
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laTStk
ENDIF

SHOW GET m.Style DISABLE
SHOW GET ibStyle DISABLE


llContK = .F.
SELECT (lcLevel)
IF llDyelot AND llMultiWH AND lcCostMth<>'S' AND !llModiMod
  = SEEK(PADR(m.Style,19)+lcFromWare) 
  lnOldCost = Ave_Cost
  llContK = .T.
ENDIF
IF llMultiWH
  = SEEK(PADR(m.Style,19)+lcFromWare+lcDyelot) 
ELSE
  IF llDyelot
    = SEEK(PADR(m.Style,19)+lcDefWareh+lcDyelot) 
  ELSE
    = SEEK(m.Style) 
  ENDIF
ENDIF
SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk MEMVAR
IF !llModiMod
  IF !llContK
    IF llDyelot 
      lnOldCost = IIF(lcCostMth<>'S',STYLE.Ave_Cost,STYLE.TotCost)
    ELSE
      lnOldCost = IIF(lcCostMth<>'S',&lcLevel->Ave_Cost,STYLE.TotCost)  
    ENDIF
  ENDIF 
  lnNewCost = lnOldCost
ENDIF

IF llGlLink
  =gfwCodePop(@laCodInfo, "CADJREASON", "D") 
  lcAReason = laAdjCode[lnAdjCode,2]
ENDIF

=SEEK('S'+lcSScale,'SCALE')
SELECT (lcTmpAdj)
SHOW GET pbNewOk    ENABLE
SHOW GET pbNClose,1 PROMPT gcBmpHome+"can.bmp"
IF llDyelot AND Style.cDye_Flg='Y'
  SHOW GET ibDye    ENABLE
  SHOW GET m.Dyelot ENABLE
ELSE
  SHOW GET ibDye    DISABLE
  SHOW GET m.Dyelot DISABLE
ENDIF 
SHOW GET ldDate     ENABLE
SHOW GET lcReason   ENABLE  
SHOW GET lcRefer    ENABLE
SHOW GET lnAdjCode  ENABLE  
SHOW GET llLoc      ENABLE
SHOW GET lnOldCost  DISABLE
SHOW GET m.Adj1     ENABLE
FOR I=2 TO 8
  Z=STR(I,1)
  IF !EMPTY(SCALE.Sz&Z)
    SHOW GET m.Adj&Z ENABLE
  ELSE
    m.Adj&Z = 0
    SHOW GET m.Adj&Z DISABLE
  ENDIF
ENDFOR
SHOW GET m.TotAdj DISABLE
IF lcType='A' .AND. m.TotAdj <= 0
  lnNewCost=lnOldCost
  SHOW GET lnNewCost DISABLE
ELSE
  SHOW GET lnNewCost ENABLE
ENDIF
llExtMFlg = .T.
llCupdate=.T.
=lfRefresh('ICDAN900')
RETURN


*!*************************************************************
*! Name      : lfClrInfo()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Get the style key accepted information.
*!             or initial information.
*!*************************************************************
*! Example   :  =lfClrInfo()
*!*************************************************************
FUNCTION lfClrInfo

STORE 0 TO m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk
STORE 0 TO m.unt_cost,m.Adj1,m.Adj2,m.Adj3,m.Adj4,m.Adj5,m.Adj6,m.Adj7,m.Adj8,m.TotAdj
STORE 0 TO lnNewCost,lnOldCost,laConvQty

lcStyDesc = ' '
lnAdjCode = 0
llLoc = .F.
llExtMFlg = .F.
IF lcType $ 'AT'
  laTStk  = 0
  laOTStk = 0
ENDIF
=SEEK('P','SCALE')              &&To show empty sizes.
SHOW GET pbNewOk    DISABLE
SHOW GET pbNClose,1 PROMPT gcBmpHome+"cls.bmp"
IF llDyelot
  STORE SPACE(10) TO m.Dyelot
  SHOW GET ibDye    DISABLE
  SHOW GET m.Dyelot DISABLE
ENDIF 
SHOW GET ldDate     DISABLE
SHOW GET lcReason   DISABLE
SHOW GET lcRefer    DISABLE
SHOW GET lnAdjCode  DISABLE  
SHOW GET llLoc      DISABLE
SHOW GET lnOldCost  DISABLE
SHOW GET lnNewCost  DISABLE
SHOW GET m.Adj1     DISABLE
SHOW GET m.Adj2     DISABLE
SHOW GET m.Adj3     DISABLE
SHOW GET m.Adj4     DISABLE
SHOW GET m.Adj5     DISABLE
SHOW GET m.Adj6     DISABLE
SHOW GET m.Adj7     DISABLE
SHOW GET m.Adj8     DISABLE
SHOW GET m.TotAdj   DISABLE
SHOW GET lnNewCost  DISABLE
*khalid
*=lfRefresh('ICINVT_A')
=lfRefresh('ICDAN900')
RETURN

*!*************************************************************
*! Name      : lfvAdj()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Calculate the total adjustment.
*!*************************************************************
*! Parameters: lcCont->No of size adjusted.
*!*************************************************************
*! Example   :  =lfvAdj()
*!*************************************************************
FUNCTION lfvAdj
PARA lcCont

I=INT(VAL(lcCont))

*--Unable to make -ve adj more than stk. in LIFO and FIFO methods.
IF lcCostMth $ 'LF' AND lcType='A' AND ;
   m.Adj&lcCont<0 AND ABS(m.Adj&lcCont) > m.Stk&lcCont
  RETURN .F.
ENDIF

IF lcType='A'
  laConvQty[I] = IIF(DanaConv.nConv3<>0,;
                  ROUND((m.Adj&lcCont*DanaConv.nConv1)/DanaConv.nConv3,0),0)
  laConvQty[9] = laConvQty[1]+laConvQty[2]+laConvQty[3]+laConvQty[4]+;
                 laConvQty[5]+laConvQty[6]+laConvQty[7]+laConvQty[8]
  laTStk[I] = m.Stk&lcCont + laConvQty[I]
  laTStk[9] = laTStk[1]+laTStk[2]+laTStk[3]+laTStk[4]+laTStk[5]+laTStk[6]+laTStk[7]+laTStk[8]
ENDIF

m.TotAdj=m.Adj1+m.Adj2+m.Adj3+m.Adj4+m.Adj5+m.Adj6+m.Adj7+m.Adj8
SHOW GET m.TotAdj DISABLE
IF lcType='A' .AND. m.TotAdj <= 0
  lnNewCost=lnOldCost
  SHOW GET lnNewCost DISABLE
ELSE
  SHOW GET lnNewCost ENABLE
ENDIF

IF I = Scale.Cnt
  =lfRefresh('ICDAN900')
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvTrnOk()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Valid function for Transaction lines fields.
*!*************************************************************
*! Example   :  =lfvTrnOk()
*!*************************************************************
FUNCTION lfvTrnOk

IF !llModiMod
  IF lcType <> 'P' .AND. m.Adj1=0 .AND. m.Adj2=0 .AND. m.Adj3=0 .AND.;
       m.Adj4=0 .AND. m.Adj5=0 .AND. m.Adj6=0 .AND. m.Adj7=0 .AND. m.Adj8=0
    *--All adjustments becomes zero.
    =gfModalGen('INM42060B42001','DIALOG')
  ELSE
    SELECT (lcTmpAdj)  
    APPEND BLANK
    REPLACE Style      WITH m.Style    ,;
            Dyelot     WITH m.Dyelot   ,;
            cFromWare  WITH lcFromWare ,;
            cToWare    WITH lcToWare   ,;
            cReason    WITH lcReason   ,;
            cRefer     WITH lcRefer    ,;
            cAdjReason WITH lcAReason  ,;
            Date       WITH ldDate     ,;
            Type       WITH lcType     ,; 
            Unt_Cost   WITH lnNewCost  ,;
            Old_Cost   WITH lnOldCost
    REPLACE Adj1       WITH m.Adj1     ,;
            Adj2       WITH m.Adj2     ,;
            Adj3       WITH m.Adj3     ,;
            Adj4       WITH m.Adj4     ,;
            Adj5       WITH m.Adj5     ,;
            Adj6       WITH m.Adj6     ,;
            Adj7       WITH m.Adj7     ,;
            Adj8       WITH m.Adj8     ,;                                                            
            TotAdj     WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8,;
            TotOld     WITH m.TotStk   ,; 
            dAdd_Date  WITH gdSysDate  ,;
            cAdd_Time  WITH TIME()     ,;
            cAdd_User  WITH gcUser_id  ,;
            GlFYear    WITH lcGlFyear  ,;
            GlPeriod   WITH lcGlPeriod
    REPLACE ConvQty1 WITH laConvQty[1],;
            ConvQty2 WITH laConvQty[2],;
            ConvQty3 WITH laConvQty[3],;
            ConvQty4 WITH laConvQty[4],;
            ConvQty5 WITH laConvQty[5],;
            ConvQty6 WITH laConvQty[6],;
            ConvQty7 WITH laConvQty[7],;
            ConvQty8 WITH laConvQty[8],;
            ConvTotQty WITH ConvQty1+ConvQty2+ConvQty3+ConvQty4+;
                            ConvQty5+ConvQty6+ConvQty7+ConvQty8
     
    SELECT (lcLevel)
    IF lcLevel='Style' .OR. lcType='P'
      SELECT STYLE
      SKIP
      IF EOF()
        SKIP -1
      ENDIF
    ELSE
      SET ORDER TO TAG STYDYEW
      DO WHILE cWareCode = lcFromWare
        SKIP
        IF EOF() .OR. cWareCode <> lcFromWare
          SKIP -1
          EXIT
        ELSE
          IF EMPTY(Dyelot)
            EXIT
          ENDIF 
        ENDIF  
      ENDDO
      SET ORDER TO TAG STYDYE
    ENDIF
  ENDIF
  m.Style = Style
  SHOW GET m.Style ENABLE
  SHOW GET ibStyle ENABLE
  SELECT (lcLevel)
  =lfClrInfo()
  _CUROBJ = OBJNUM(m.Style)

ELSE  && llModiMod

  IF lcType='A' .AND. m.Adj1=0 .AND. m.Adj2=0 .AND. m.Adj3=0 .AND.;
     m.Adj4=0 .AND. m.Adj5=0 .AND. m.Adj6=0 .AND. m.Adj7=0 .AND. m.Adj8=0
    *--All adjustments becomes zero,Cannot modify.
    = gfModalGen('INM42061B42001','DIALOG')
  ELSE
    SELECT (lcTmpAdj)  
    REPLACE cReason WITH lcReason,;
            cAdjReason WITH lcAReason ,;
            cRefer  WITH lcRefer,;
            Date    WITH ldDate,;
            Unt_Cost WITH lnNewCost,;
            Adj1    WITH m.Adj1,;
            Adj2    WITH m.Adj2,;
            Adj3    WITH m.Adj3,;
            Adj4    WITH m.Adj4,;
            Adj5    WITH m.Adj5,;
            Adj6    WITH m.Adj6,;
            Adj7    WITH m.Adj7,;
            Adj8    WITH m.Adj8,;                                                            
            TotAdj  WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8
    REPLACE ConvQty1 WITH laConvQty[1],;
            ConvQty2 WITH laConvQty[2],;
            ConvQty3 WITH laConvQty[3],;
            ConvQty4 WITH laConvQty[4],;
            ConvQty5 WITH laConvQty[5],;
            ConvQty6 WITH laConvQty[6],;
            ConvQty7 WITH laConvQty[7],;
            ConvQty8 WITH laConvQty[8],;
            ConvTotQty WITH ConvQty1+ConvQty2+ConvQty3+ConvQty4+;
                            ConvQty5+ConvQty6+ConvQty7+ConvQty8

  ENDIF
  CLEAR READ
ENDIF
RETURN

*!*************************************************************
*! Name      : lfvTrnClos()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Valid function new/modify close.
*!*************************************************************
*! Example   :  =lfvTrnClos()
*!*************************************************************
FUNCTION lfvTrnClos

IF llExtMFlg
  *--Are you sure ? You will lose your XXXX ,\!\<Yes;\?\<No
  IF gfModalGen('TRM42063B42002','DIALOG',IIF(llModiMod,'changes','current entry')) = 2
    _CUROBJ = OBJNUM(ldDate)
    SHOW GET ldDate ENABLE
    RETURN
  ELSE
    =lfClrInfo()
  ENDIF
ENDIF
lcReason  = SPACE(25)
lcAReason = SPACE(6)
CLEAR READ
RETURN

*!*************************************************************
*! Name      : lfvLocat()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Do Functions mover to select locations.
*!*************************************************************
*! Example   :  =lfvLocat()
*!*************************************************************
FUNCTION lfvLocat

SELECT WHSLOC 
lcWareAny = IIF(lcType='T',lcToWare,lcFromWare)
IF !SEEK(SPACE(19)+SPACE(6)+lcWareAny) 
  *--No locations have been assigned to warehouse '+ALLTRIM(lcWareAny)+' .'
  = gfModalGen('TRM42058B42001','DIALOG',ALLTRIM(lcWareAny))
  llLoc = .F.
  SHOW GET llLoc
  RETURN
ENDIF

DIMENSION laSource[1],laTarget[1]
STORE ' ' TO laSource,laTarget
lsSource = 1
SELECT cLocation FROM WHSLOC ;
  WHERE Style+Color+cWareCode == SPACE(19)+SPACE(6)+lcWareAny ;
  INTO ARRAY laSource

SELECT cLocation FROM (lcTemLoc) ;
  WHERE Style+cWareCode+cLocation=PADR(m.Style,19)+lcWareAny;
  INTO ARRAY laTarget

=gfMover(@laSource,@laTarget,"Assign Locations",.T.,'lfvLoc')
SELECT (lcTemLoc)
DELETE FOR Style+cWareCode+cLocation = PADR(m.Style,19)+lcWareAny

IF !EMPTY(laTarget[1])
  FOR I = 1 TO ALEN(laTarget)
     APPEND BLANK
     REPLACE Style     WITH m.Style;
             cWareCode WITH lcWareAny;
             cLocation WITH laTarget[I]
  ENDFOR
ENDIF

SELECT (lcTemLoc)
GO TOP
llLoc = !EOF()
SHOW GET llLoc
RETURN

*!*************************************************************
*! Name      : lfvLoc
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Validate locations.
*!*************************************************************
*! Example   :  =lfvLoc()
*!*************************************************************
FUNCTION lfvLoc
PARAMETERS lnOption

DO CASE 
  CASE lnOption=1
    lcLocatin=laSource[lsSource]
    IF !SEEK(PADR(m.Style,19)+SPACE(6)+lcWareAny+lcLocatin,"WhsLoc")
      *--Location XXX is not assigned to '+'style XXX in warehouse XXX,'\!\<Assign;\<Cancel
      IF gfModalGen('TRM42064B42007','DIALOG',ALLTRIM(lcLocatin)+'|'+ALLTRIM(m.Style)+'|'+ALLTRIM(lcWareAny)) = 1
        RETURN .T.
      ELSE
        RETURN .F. 
      ENDIF
    ELSE
      RETURN .T.
    ENDIF
  CASE lnOption=2
    RETURN .F.
  CASE (lnOption=3 OR lnOption=4)
    RETURN .T.
ENDCASE

*!*************************************************************
*! Name      : lfvModify()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Modify added transaction.
*!*************************************************************
*! Example   :  =lfvModify()
*!*************************************************************
FUNCTION lfvModify

llModiMod = .T.
SELECT (lcTmpAdj)
lnCurrRec = IIF(!EOF(),RECNO(),1)
lcScFields = 'Style,Dyelot,Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj'
FOR lnConvCnt = 1 TO 8
    lcConvCnt = STR(lnConvCnt,1)
    laConvQty[lnConvCnt] = ConvQty&lcConvCnt
ENDFOR    
laConvQty[9] = ConvTotQty

SCATTER FIELDS &lcScFields MEMO MEMVAR
STORE 0 TO m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk
IF lcType = 'AT'  
  laTStk  = 0
  laOTStk = 0
ENDIF

ldDate    = Date
lnOldCost = Old_Cost
lnNewCost = Unt_Cost
lcReason  = cReason
lcAReason = cAdjReason
lcRefer   = cRefer
IF llGlLink AND !EMPTY(lcAReason)
  =gfwCodePop(@laCodInfo, "CADJREASON", "T") 
ENDIF

=lfHoldKey('S')
=lfUnTrapKeys()
DO (gcScrDir+gcWinAppl+"\ICDAN900.SPX")
=lfHoldKey('R')

SELECT (lcTmpAdj)
GOTO lnCurrRec
SHOW WINDOW (lcBrow_Ttl) REFRESH
SHOW GETS
_CUROBJ = OBJNUM(pbModify)
RETURN


*!*************************************************************
*! Name      : lfOpnInfo()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Show gets in modify mode.
*!*************************************************************
*! Example   :  =lfOpnInfo()
*!*************************************************************
FUNCTION lfOpnInfo

IF !llModiMod
  RETURN
ENDIF

SELECT STYLE
=SEEK(m.Style)
lcSScale=Scale
lcStyDesc=Desc1

SELECT (lcLevel)
IF lcLevel<>'Style'
  IF llMultiWH
    = SEEK(PADR(m.Style,19)+lcFromWare+m.Dyelot) 
  ELSE
    IF llDyelot
      = SEEK(PADR(m.Style,19)+lcDefWareh+m.Dyelot) 
    ELSE
      = SEEK(m.Style) 
    ENDIF
  ENDIF
ENDIF
SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk MEMVAR

IF lcType = 'A'
  FOR I=1 TO 8
    z=STR(I,1)
    laTStk[I] = m.Stk&z+laConvQty[I]
  ENDFOR
  laTStk[9] = m.TotStk+laConvQty[I]
ENDIF

=SEEK('S'+lcSScale,'SCALE')
SELECT (lcTmpAdj)
SHOW GET pbNewOk    ENABLE
SHOW GET pbNClose,1 PROMPT gcBmpHome+"can.bmp"
SHOW GET ibStyle    DISABLE
SHOW GET ibDye      DISABLE
SHOW GET ldDate     ENABLE
SHOW GET lcReason   ENABLE  
SHOW GET lcRefer    ENABLE
SHOW GET lnAdjCode  ENABLE  
SHOW GET llLoc      ENABLE
SHOW GET lnNewCost  ENABLE
SHOW GET lnOldCost  DISABLE

SHOW GET m.Adj1     ENABLE
FOR I=2 TO 8
  Z=STR(I,1)
  IF !EMPTY(SCALE.Sz&Z)
    SHOW GET m.Adj&Z ENABLE
  ELSE
    m.Adj&Z = 0
    SHOW GET m.Adj&Z DISABLE
  ENDIF
ENDFOR
SHOW GET m.TotAdj DISABLE
IF lcType='A' .AND. m.TotAdj <= 0
  lnNewCost=lnOldCost
  SHOW GET lnNewCost DISABLE
ELSE
  SHOW GET lnNewCost ENABLE
ENDIF
=lfRefresh('ICDAN900')
_CUROBJ = OBJNUM(ldDate)
RETURN


*!*************************************************************
*! Name      : lfvRemove()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Remove added transaction.
*!*************************************************************
*! Example   :  =lfvRemove()
*!*************************************************************
FUNCTION lfvRemove

*--The Style :XXX will be removed?','\<Yes;\<No
IF gfModalGen('TRM42065B42002','DIALOG',ALLTRIM(Style)) = 1
  SELECT (lcTmpAdj)  
  DELETE
  GO TOP
  IF !llMultiWH AND EOF()
    laScrMode = .F.
    laScrMode[1] = .T.
    llCUpdate = .F.
  ENDIF
  lnCurrRec=RECNO()
  =lfActBrow()
ENDIF
SHOW GETS
RETURN

*!*************************************************************
*! Name      : lpUnLock
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Unlock the saved locked records.
*!*************************************************************
*! Example   :  DO lpUnLock
*!*************************************************************
PROCEDURE lpUnLock
IF llDyeLvl
  SET MULTILOCKS OFF
ELSE
  UNLOCK ALL
ENDIF

*!*************************************************************
*! Name      : lfRecLock
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Record Lock.
*!*************************************************************
*! Parameters: lcFile->Locked file.
*!*************************************************************
*! Example   :  =lfRecLock()
*!*************************************************************

FUNCTION lfRecLock
PARAMETERS lcFile

SET REPROCESS TO 5 SECONDS 
DO WHILE .T.
  *-This record is in use by another user !','\!\<Retry;\<Cancel'
  lnChoice=gfModalGen('INM00029B00015','DIALOG')
  IF lnChoice = 1 
    IF !RLOCK(lcFile)
      LOOP
    ELSE
      lnRet = .T.
      EXIT
    ENDIF 
  ELSE
    lnRet = .F.
    EXIT   
  ENDIF
ENDDO
SET REPROCESS TO 0
RETURN (lnRet)

*!*************************************************************
*! Name      : lpSavScr()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Update the master files.
*!*************************************************************
*! Calls     : None.
*:  Procs & Fncts: lpUnLock
*:                 lfRecLock()
*:                 lfInvUpdt()
*:                 lfVldDate()
*!*************************************************************
*! Example   :  =lpSavScr()
*!*************************************************************
FUNCTION lpSavScr
*-- Open a temp file to be used in calling gl distributer proc.
IF llGlLink
  IF !laOpFile[2]
    laOpFile[2]=gfOpenFile(gcDataDir+'GLDist','GLDistAc','SH')
  ENDIF
  SELECT GLDist
  lcTmpGlDt = gfTempName()
  COPY STRU TO &gcWorkDir.&lcTmpGlDt
  USE (gcWorkDir+lcTmpGlDt) IN 0 EXCLUSIVE
  SELECT (lcTmpGlDt)
ENDIF  

IF !laOpFile[3]
  laOpFile[3]=gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")
ENDIF

*--Save to master files.
DIMENSION laToSave[8],laTemp[8]
SELECT (lcTmpAdj)
SCAN
  *--Check if nothing to adjust or transfer (all Zero).
  IF lcType $ 'AT'
    SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8 TO laToSave
    llZeroAdj = .T.
    FOR lnI = 1 To 8 
      llZeroAdj = IIF(laToSave[lnI]=0, .T., .F.)
      IF !llZeroAdj
        EXIT
      ENDIF
    ENDFOR
    IF llZeroAdj
      DELETE
      LOOP
    ENDIF
  ENDIF
 
  = SEEK( Style,'STYLE')
  = SEEK( Style + cFromWare + SPACE(10),'STYDYE')

  llDyeLvl = ( llDyelot AND STYLE.cDye_Flg = 'Y' )
  IF llDyeLvl
    SET MULTILOCKS ON
  ENDIF 
  llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE')) AND ;
           (RLOCK('STYLE' ) OR lfRecLock('STYLE' ))
  IF llDyeLvl AND llGoOn
    =SEEK(Style+cFromWare+Dyelot,'STYDYE')
    llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE'))
  ENDIF  

  SELECT (lcTmpAdj)
  IF !llGoOn
    *-Style XDX: XXX/XXX is in use by another user, Unable to update.
    =gfModalGen('TRM42067B42001','DIALOG',+'|'+IIF(llDyelvl,'/Dyelot','')+'|'+ALLTRIM(STYLE)+'|'+IIF(llDyelvl,'/'+Dyelot+'',''))
    DELETE
    DO lpUnLock
    LOOP
  ENDIF

  *-- Store Style old cost, Old stock, And link code.
  SELECT Style
  lnOldStk   = TotStk
  lnOldCost  = IIF(lcCostMth<>'S',Ave_Cost,TotCost)  
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(Link_Code),Link_Code,'DEFDEF'),"")
  SELECT (lcTmpAdj)

  SELECT IIF(!llMultiWH .AND. !llDyeLvl, 'Style', 'StyDye')
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8 TO laToSave
  lnTotStk = TotStk
  SELECT (lcTmpAdj)
  GATHER FROM laToSave FIELDS OldQty1,OldQty2,OldQty3,OldQty4 ,;
                              OldQty5,OldQty6,OldQty7,OldQty8
  REPLACE TotOld    WITH lnTotStk
  REPLACE dPostDate WITH ldPstDate
  
  WAIT WINDOW 'Start updating => '+ALLTRIM(Style)+ ;
              IIF(llDyelvl,'/'+&lcTmpAdj->Dyelot,'') NOWAIT


  *-- Store WAREHOUSE link code. If warehouse link code is empty default
  *-- warehouse link code to style link code. If the last one is empty
  *-- default to 'DEF' link code.
  lcLinkCode = IIF(llGlLink ,IIF(!EMPTY(STYDYE.GL_Link),STYDYE.GL_Link,lcLinkCode), "")

  *--Start Updating.
  IF ! lfInvUpdt()
    SELECT (lcTmpAdj)
    LOOP
  ENDIF
    
  
  SELECT (lcTmpAdj)
  REPLACE cSession WITH STYINVJL.cSession

  *--Update Master file for locations.
  IF llWareLoc 
    SELECT (lcTemLoc)
    SCAN FOR !EMPTY(cLocation)
      IF !SEEK(Style+SPACE(6)+cWareCode+cLocation,'WHSLOC')
        SCATTER MEMVAR
        INSERT INTO WhsLoc FROM MEMVAR
      ENDIF
    ENDSCAN
  ENDIF 

  SELECT (lcTmpAdj)
  DO lpUnLock
ENDSCAN

*--Update master adjustment file.
WAIT WINDOW ' Updating the master adjustment file.' NOWAIT

IF !USED(lcTmpAdj)
  = gfOpenFile (gcWorkDir+'lcTmpAdj',' ','EX')
ENDIF  

SELECT (lcTmpAdj)
SCAN FOR !DELETED()
  SELECT InvtAdj
  APPEND BLANK
  REPLACE Style      WITH &lcTmpAdj..Style      ,;
          Dyelot     WITH &lcTmpAdj..Dyelot     ,;
          Date       WITH &lcTmpAdj..Date       ,;
          Type       WITH &lcTmpAdj..Type       ,; 
          Unt_Cost   WITH &lcTmpAdj..Unt_Cost   ,;
          Old_Cost   WITH &lcTmpAdj..Old_Cost    ,;
          Adj1       WITH &lcTmpAdj..ConvQty1   ,;
          Adj2       WITH &lcTmpAdj..ConvQty2   ,;
          Adj3       WITH &lcTmpAdj..ConvQty3   ,;
          Adj4       WITH &lcTmpAdj..ConvQty4   ,;
          Adj5       WITH &lcTmpAdj..ConvQty5   ,;
          Adj6       WITH &lcTmpAdj..ConvQty6   ,;
          Adj7       WITH &lcTmpAdj..ConvQty7   ,;
          Adj8       WITH &lcTmpAdj..ConvQty8   ,;                                                            
          TotAdj     WITH &lcTmpAdj..ConvTotQty
  
  REPLACE OldQty1    WITH &lcTmpAdj..OldQty1,;
          OldQty2    WITH &lcTmpAdj..OldQty2,;
          OldQty3    WITH &lcTmpAdj..OldQty3,;
          OldQty4    WITH &lcTmpAdj..OldQty4,;
          OldQty5    WITH &lcTmpAdj..OldQty5,;
          OldQty6    WITH &lcTmpAdj..OldQty6,;
          OldQty7    WITH &lcTmpAdj..OldQty7,;
          OldQty8    WITH &lcTmpAdj..OldQty8,;
          TotOld     WITH &lcTmpAdj..TotOld
  
  REPLACE GlFYear    WITH lcGlFyear  ,;
          GlPeriod   WITH lcGlPeriod ,;
          cFromWare  WITH &lcTmpAdj..cFromWare  ,;
          cReason    WITH &lcTmpAdj..cReason    ,;
          dPostDate  WITH &lcTmpAdj..dPostDate  ,;                    
          cSession   WITH &lcTmpAdj..cSession   ,;
          cAdd_User  WITH gcUser_id  ,;
          cAdd_Time  WITH TIME()     ,;
          dAdd_Date  WITH gdSysDate
ENDSCAN

SELECT (lcTmpAdj)
SET ORDER TO TAG &lcTmpAdj
SELECT (lcTmpAdj)
ZAP

*-- Update distripution master file
IF llGlLink 
  WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
  SELECT (lcTmpGlDt)
  *-- Generate a unique session number.
  lcGlSess = gfsequence('GLSESSION')
  REPLACE ALL GLSESSION WITH lcGlSess
  USE
  SELECT GLDIST  
  APPEND FROM &gcWorkDir.&lcTmpGlDt     
  ERASE (gcWorkDir+lcTmpGlDt+'.DBF')
ENDIF  
WAIT CLEAR

*--Would you like to print the Inventory Adjustment journal ?
IF gfModalGen('TRM42066B42002','DIALOG') = 1
  lcFromWare = IIF( llMultiWH, SPACE(6), lcDefWareh)
  lcToWare   = SPACE(06)
  lcWFromDsc = SPACE(30)
  lcWToDsc   = SPACE(30)
  SELECT (lcTmpAdj) 
  GO TOP
  =lfActBrow()
  laScrMode = .F.
  laScrMode[1] = .T.
  SHOW GETS  
  LNCUROBJ = OBJNUM(lcFromWare)

  =gfStatic()                   && Save inviroment before terminate
  CLEAR READ                    && Terminat old program
  =gfClrBro()                   && Call Function to release browse windows
  IF glFromBrow 
    glFromBrow = .F.
    ON KEY LABEL CTRL+Q
    KEYBOARD "{CTRL+Q}" PLAIN CLEAR
  ENDIF  
  gcNextRepo = "ICSTKJL"
ELSE
  lcFromWare = IIF( llMultiWH, SPACE(6), lcDefWareh)
  lcToWare   = SPACE(06)
  lcWFromDsc = SPACE(30)
  lcWToDsc   = SPACE(30)
  SELECT (lcTmpAdj) 
  GO TOP
  =lfActBrow()
  ACTIVATE WINDOW (lcWinCh0) TOP
  _CUROBJ = OBJNUM(lcFromWare)
ENDIF  
*-- End of lfActBrow.

*!*************************************************************
*! Name      : lfInvUpdt()
*! Developer : Khalid Mohi El-Din Mohamed
*! Date      : 05/28/2000
*! Purpose   : Update inventory.
*!*************************************************************
*! Example   :  =lfInvUpdt()
*!*************************************************************
FUNCTION lfInvUpdt

SELECT (lcTmpAdj)
*--Gl adjustment account.
lcAdjAcct = ' '
IF llGlLink AND !EMPTY(cAdjReason)
  lcAdjReason = cAdjReason
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  = gfRltFld(lcAdjReason , @laTrmRltFd , "CADJREASON")
ELSE
  lcAdjReason = ' '
ENDIF

*--G/L Array difinition and initialization.
IF llGlLink
  DECLARE laGLDistAr[2,13]
  laGLDistAr[1,1] = lcLinkCode
  laGLDistAr[2,1] = lcLinkCode
  laGLDistAr[1,2] = '006'
  laGLDistAr[2,2] = '007'
  laGLDistAr[1,3] = 1
  laGLDistAr[2,3] = -1
  STORE IIF(lcType = 'P','IP','IA') TO laGLDistAr[1,4],laGLDistAr[2,4]
  STORE ''        TO laGLDistAr[1,5],laGLDistAr[2,5]
  STORE ldPstDate TO laGLDistAr[1,6],laGLDistAr[2,6]
  STORE GLFYear   TO laGLDistAr[1,7],laGLDistAr[2,7]
  STORE GLPeriod  TO laGLDistAr[1,8],laGLDistAr[2,8]
  STORE lcTmpGlDt TO laGLDistAr[1,9],laGLDistAr[2,9]
  laGLDistAr[2,10] = lcAdjAcct
ELSE
  DIME laGLDistAr[1,1]
  laGLDistAr = ''
ENDIF

SELECT (lcTmpAdj)

*--Warehouse Code.
lcAdjWareH = IIF(lcType = 'T',&lcTmpAdj..cToWare,&lcTmpAdj..cFromWare )

*--Adjustment quantity array.
DECLARE laAdjust[9]
SCATTER FIELDS ConvQty1,ConvQty2,ConvQty3,ConvQty4,ConvQty5,ConvQty6,ConvQty7,ConvQty8,ConvTotQty TO laAdjust

*--Adjustment cost.
lnACost = IIF(TotAdj>0,Unt_Cost,Old_Cost)
*--Type of the adjustment.
lcAdjTyp = IIF(lcType = 'P','2','1')

PRIVATE lcRefer 
lcRefer = IIF(EMPTY(cReason),gfCodDes(cAdjReason,'CADJREASON'),cReason)
  
lcAdjRef = &lcTmpAdj..cRefer
lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
               lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)

*--Return with no save if function return fulse.
IF lnRet = 0
  RETURN .F.
ENDIF  
RETURN