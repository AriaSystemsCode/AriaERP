*!**********************************************************************************************
*! Program file  : SoSoc20.PRG
*! Program desc. : Order Tracking Sceen for Soccer Select
*! Screen        : SoSoc20.SPR
*! System        : Aria Apparel System
*! Module        : Sales Order (SO)
*! Developer     : Sameh Saiid Ezzat 
*! Date          : 02/05/2001
*! Reference     : C102118
*!**********************************************************************************************
*! Functions  : lfExit, lfVald, lfDoScr, lfBrowDet, lfOrdSta, lfwBrow, lfSelect, lfModify, 
*!              lfSelAll, lfSelNon, lfInvert, lfCancel, lfMSelect, lfreturn, lfVOrd, lfVOSta
*!              lfVTSta, lfAccept, lfTrapKey
*! Procedures : lpEsc, lpTab, lpShiftTab, lpEscD
*!**********************************************************************************************
*! Passed Parameters  : lcOrdType  : Order Type (always 'O')
*!                      lcOrder    : Order Number 
*!                      lcSalesScr : Called from Sales order screen or not
*!                      lcDivision : Division 
*!                      lcTeam     : Team
*!**********************************************************************************************
*! Example    : DO SoSoc20
*!**********************************************************************************************
*
PARAMETER lcOrdType , lcOrder , lcSalesScr , lcDivision , lcTeam
*lcSalesScr --> variable to detect if this screen is called from Sales Order Screen.

PRIVATE llCalled, lcOrder , lcDivision , lcTeam , llModify , llCancel , lcConfirm , lcDet_Ttl
STORE 1 TO lnOrdSta,lnTemSta
STORE 'DISABLE' TO lcSelStat,lcAcpStat,lcModStat
STORE 'ENABLE'  TO lcOrdStat

llCalled   = .F.
lcTempName = gfTempName()
lcDet_Ttl  = 'Detail Screen'

*--- Open Data File
IF TYPE('lcSalesScr') <> 'C'
  =gfOpenFile (gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')
  =gfOpenFile (gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
  =gfOpenFile (gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
  =gfOpenFile (gcDataDir+'SOCCER',gcDataDir+'SOCCER','SH')
ENDIF  

SELECT Soccer
= AFields(laFileStru)

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'Chk'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'Pointer'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

lnNewFld = ALEN(laFileStru,1)+1
DIMENSION laFileStru[lnNewFld,4]
laFileStru[lnNewFld,1] = 'Flag'
laFileStru[lnNewFld,2] = 'C'
laFileStru[lnNewFld,3] = 1
laFileStru[lnNewFld,4] = 0

CREATE TABLE (gcWorkDir+lcTempName) FROM ARRAY laFileStru

*--- Variable Declaration
*--- Variable Declaration
IF TYPE('lcOrdType') <> 'C'
  lcOrdType = 'O'
ENDIF

IF TYPE('lcOrder') <> 'C'
  lcOrder  = SPACE(06)
ELSE
  llCalled = .T.
ENDIF

IF TYPE('lcDivision') <> 'C'
  lcDivision = SPACE(04)
ENDIF  

IF TYPE('lcTeam') <> 'C'
  lcTeam     = SPACE(04)
ENDIF

IF llCalled
  KEYBOARD '{ENTER}' + '{ENTER}' + '{ENTER}' CLEAR
ENDIF   

lcPrompt = '\<Close'
STORE SPACE(05) TO lcAccount , lcLeague
STORE SPACE(10) TO lcOrdStaC , lcTemStaC
STORE SPACE(01) TO lcMainOSta , lcMainTSta
STORE .T. TO llFirst , llModify
lcChk      = "»"
llCancel   = .F.

*--- Array for order tracking Popup
Dimension laOrder[3]
laOrder[1] = 'New'
laOrder[2] = 'Accepted'
laOrder[3] = 'Charted'

*--- Array for team tracking Popup
Dimension laTeam[5,2]
laTeam[1,1] = 'Not Picked'
laTeam[1,2] = 'K'
laTeam[2,1] = 'Picked'
laTeam[2,2] = 'I'
laTeam[3,1] = 'Printed'
laTeam[3,2] = 'T'
laTeam[4,1] = 'Shipped'
laTeam[4,2] = 'H'
laTeam[5,1] = 'BackOrder'
laTeam[5,2] = 'O'

*---Main screen
PUSH KEY
ON KEY LABEL ESCAPE DO lpEsc
lcConfirm = SET('CONFIRM')
SET CONFIRM OFF
DO (gcScrDir+gcWinAppl+"\SOSOC20.SPX")
SET CONFIRM &lcConfirm
POP KEY

*-- If screen is not called from Sales Order screen , we have to close these files.
IF TYPE('lcSalesScr') <> 'C'
  IF USED('OrdHdr')
    USE IN OrdHdr
  ENDIF

  IF USED('OrdLine')
    USE IN OrdLine
  ENDIF

  IF USED('Soccer')
    USE IN Soccer
  ENDIF

  IF USED('Customer')
    USE IN Customer
  ENDIF
ENDIF  
*-- EndIf of screen is not called from Sales Order screen
*-- End of program.

*!*******************************************************************************
*! Name      : lfExit
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Exit the program - (For Soccer).
*!*******************************************************************************
*! Example   : =lfExit()
*!*******************************************************************************
*
FUNCTION lfExit
IF lcPrompt ='\<Cancel'
  lnAnswer = gfModalGen("QRM00000B00006","DIALOG",'','','Are you sure ? You will lose all changes.')
  IF lnAnswer = 1 
    IF llCalled
     CLEAR READ
    ENDIF
    lcPrompt ='\<Close'
    SHOW GET pbClose,1 PROMPT lcPrompt
    =lfMSelect()
  ENDIF
ELSE
 CLEAR READ
ENDIF
*-- End of lfExit.

*!*******************************************************************************
*! Name      : lfVald
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : To validate order field - (For Soccer).
*!*******************************************************************************
*! Example   : =lfVald()
*!*******************************************************************************
*
FUNCTION lfVald
PRIVATE lnAlias 
lnAlias = SELECT()

IF SEEK(lcOrdType+lcOrder+IIF(!EMPTY(lcDivision),lcDivision,'')+IIF(!EMPTY(lcTeam),lcTeam,''),'SOCCER')
   IF SEEK(lcOrdType + lcOrder,'OrdHdr')
      lcAccount  = OrdHdr.Account
      lcLeague   = OrdHdr.Dept
   ENDIF
   lcMainOSta = SOCCER.OSTATION
   lcMainTSta = SOCCER.TSTATION
   =lfOrdSta()
   =lfRefresh()
   SHOW GET lcDivision DISABLE
   SHOW GET lcOrder    DISABLE
   SHOW GET lcTeam     DISABLE
   SHOW GET lcLeague   DISABLE
   SHOW GET lcAccount  DISABLE
   SHOW GET pbModify   ENABLE
   IF !llCalled
     SHOW GET pbSelect   ENABLE
   ENDIF 
ELSE
    IF !EMPTY(lcOrder) 
      =gfModalGen("INM00000B00000","DIALOG",'','','(Order/Division/Team)'+SPACE(30)+'('+lcOrder+'/'+lcDivision+'/'+lcTeam+')'+SPACE(45)+'Not'+SPACE(04)+'found.')
    ENDIF
    IF EMPTY(lcOrder) AND EMPTY(lcDivision) AND EMPTY(lcTeam)
      _CUROBJ  = OBJNUM(pbClose)
    ELSE
      _CUROBJ  = OBJNUM(lcOrder)
    ENDIF
    IF !llCalled
      lcOrder    = SPACE(06)
      lcDivision = SPACE(04)
      lcTeam     = SPACE(04)
    ENDIF
ENDIF

IF llCalled
 SHOW GET lcDivision DISABLE
 SHOW GET lcOrder    DISABLE
 SHOW GET lcTeam     DISABLE
 SHOW GET lcLeague   DISABLE
 SHOW GET lcAccount  DISABLE
ENDIF

SHOW GET lcDivision
SHOW GET lcOrder
=lfRefresh()
SELECT(lnAlias)
*-- End of lfVald.

*!*******************************************************************************
*! Name      : lfDoScr
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Do the Browse screen - (For Soccer).
*!*******************************************************************************
*! Example   : =lfDoScr()
*!*******************************************************************************
*
FUNCTION lfDoScr
=lfOrdSta()
lnBrRecNo = 0
SHOW GET lnTemSta ENABLE

PUSH KEY
=lfTrapKey()
DO (gcScrDir+gcWinAppl+"\SOSOC2A.SPX")
POP KEY 
*-- End of lfDoScr.

*!*******************************************************************************
*! Name      : lfBrowDet
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Browse - (For Soccer).
*!*******************************************************************************
*! Example   : =lfBrowDet()
*!*******************************************************************************
*
FUNCTION lfBrowDet
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcTempName)
lcBrowfields = "cMarker=IIF(RECNO()=lnBrRecNo,'>',''):1:R:H='':W=.F.,"+;
               "Chk :H= 'Chk',"+; 
               "Div :H= 'Division',"+; 
               "Team:H= 'Team#'," +; 
               "Youthpkg:H = 'Yth Bkg.'," + ;
               "Adultpkg:H = 'Adlt Bkg.'," + ;
               "OStation=laorder[ASCAN(laOrder,OStation)]:H  = 'Order Tracking Station',"+;
               "TStation=laTeam[ASCAN(laTeam,TStation)/2,1]:H  = 'Team Tracking Station'"               

BROWSE FIELDS &lcBrowfields;
       LOCK 0   ;
       NOAPPEND ;
       NOEDIT   ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       TITLE (lcDet_Ttl);
       WHEN  lfwBrow();
       WINDOW SOSOC2C IN WINDOW SOSOC2A
SELECT(lnAlias)
*-- End of lfBrowDet.

*!*******************************************************************************
*! Name      : lfOrdSta
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Browse For Soccer Select.
*!*******************************************************************************
*! Example   : =lfOrdSta()
*!*******************************************************************************
*
FUNCTION lfOrdSta
PRIVATE lnAlias

DO CASE
  CASE  lcMainOSta ='N'
    lnOrdSta  = 1
    lcOrdStaC = 'New'
  CASE  lcMainOSta ='A'
    lnOrdSta  = 2
    lcOrdStaC = 'Accepted'
  CASE  lcMainOSta ='C'
    lnOrdSta  = 3
    lcOrdStaC = 'Charted'
ENDCASE
SHOW GET lnOrdSta DISABLE
IF !EMPTY(lcDivision) AND !EMPTY(lcTeam)
  DO CASE
    CASE  lcMainTSta ='K'
      lnTemSta = 1
    CASE  lcMainTSta ='I'
      lnTemSta = 2
    CASE  lcMainTSta ='T'
      lnTemSta = 3
    CASE  lcMainTSta ='H'
      lnTemSta = 4
    CASE  lcMainTSta ='O'
     lnTemSta = 5
  ENDCASE
  SHOW GET lnTemSta DISABLE  
ENDIF
*-- End of lfOrdSta.

*!*******************************************************************************
*! Name      : lfwBrow
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Browse - (For Soccer).
*!*******************************************************************************
*! Example   : =lfwBrow()
*!*******************************************************************************
*
FUNCTION lfwBrow
PRIVATE lnAliAs,lnRecNo

lnAlias = SELECT()
SELECT(lcTempName)
lnBrRecNo = RECNO()
lcPromp = IIF(EMPTY(Chk),'\<Select','Un\<Select')
SHOW GET pbSelect,1 PROMPT lcPromp
SHOW WINDOW (lcDet_Ttl) REFRESH
SELECT(lnAliAs)
*-- End of lfwBrow.

*!*******************************************************************************
*! Name      : lfSelect
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : When press Select bottom - (For Soccer).
*!*******************************************************************************
*! Example   : =lfSelect()
*!*******************************************************************************
*
FUNCTION lfSelect
PRIVATE lnAlias

lnAlias = SELECT()
SELECT (lcTempName)
IF  &lcTempName..TStation = lcMainTSta
  IF EMPTY(&lcTempName..Chk)
    Replace &lcTempName..Chk WITH lcChk
  ELSE
    Replace Chk WITH SPACE(1)
  ENDIF
  =lfwBrow()
ELSE
  =gfModalGen("INM00000B00000","DIALOG",'','','This division team does not have the same team tracking station as the selected one . Cannot select.')
ENDIF  
SELECT(lnAlias)
*-- End of lfSelect.

*!*******************************************************************************
*! Name      : lfModify
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : When press modify bottom - (For Soccer).
*!*******************************************************************************
*! Example   : =lfModify()
*!*******************************************************************************
*
FUNCTION lfModify
PRIVATE lnAlias , llDoScreen
llDoScreen = .F.
lnAlias = SELECT()
IF !EMPTY(lcDivision) AND !EMPTY(lcTeam)
  lnAnswer = gfModalGen("TRM00000B00006","DIALOG",'','',[Do you want to scan other ITC's from the same order.])
  IF lnAnswer = 1
    SELECT (lcTempName)
    *-- append from the main file only the first calling
    IF llFirst 
      APPEND FROM (gcDataDir+'Soccer') FOR cOrdtype + Order = lcOrdType + lcOrder AND (DIV  <> lcDivision OR TEAM <> lcTeam)
      llFirst    = .F.
      LOCATE
      SCAN
        IF &lcTempName..TSTATION = lcMainTSta
          Replace &lcTempName..Chk WITH lcChk
        ENDIF
      ENDSCAN    
    ENDIF

    SELECT (lcTempName)
    LOCATE
    SCAN
       REPLACE &lcTempName..Flag WITH &lcTempName..Chk
    ENDSCAN    
    LOCATE
    IF EOF()
      =gfModalGen("TRM00000B00000","DIALOG",'','','No record match this station.')
    ELSE
      *lcProgToRun ='LL=lfDoScr() AND lfCntShow()'
      *CLEAR READ
      llDoScreen = .T.
    ENDIF
  ELSE
    SHOW GET lnTemSta ENABLE
  ENDIF
ELSE
  SHOW GET lnOrdSta ENABLE
  SHOW GET pbSelect DISABLE
ENDIF
=lfCntShow()

IF llDoScreen
  =lfDoScr() AND lfCntShow()
ENDIF

SELECT(lnAlias)
*-- End of lfModify.

*!*******************************************************************************
*! Name      : lfCntShow
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Control Show for Soccer Select.
*!*******************************************************************************
*! Example   : =lfCntShow()
*!*******************************************************************************
*
FUNCTION lfCntShow

lcSelStat = 'DISABLE'
lcAcpStat = 'ENABLE'
lcOrdStat = 'DISABLE'
SHOW GET pbSelect    &lcSelStat
SHOW GET lcOrder     &lcOrdStat
SHOW GET lcDivision  &lcOrdStat
SHOW GET lcTeam      &lcOrdStat
lcPrompt = '\<Cancel'
SHOW GET pbClose,1 PROMPT lcPrompt
SHOW GET pbAccept  &lcAcpStat

lcModStat = IIF(EMPTY(lcDivision) AND EMPTY(lcTeam),'DISABLE','ENABLE')
SHOW GET pbModify &lcModStat
SHOW GET lnTemSta &lcModStat
*-- End of lfCntShow.

*!*******************************************************************************
*! Name      : lfSelAll
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Select All - (For Soccer).
*!*******************************************************************************
*! Example   : =lfSelAll()
*!*******************************************************************************
*
FUNCTION lfSelAll
PRIVATE lnAlias,lnRecNo

lnAlias = SELECT()
SELECT (lcTempName)
lnRecNo = RECNO()
LOCATE
SCAN
  IF lcMainTSta <> TSTATION
    =gfModalGen("TRM00000B00000","DIALOG",'','','One or more division team does not have the same team tracking station as the selected one only those matching will be selected.')
    EXIT
  ENDIF
ENDSCAN
LOCATE
SCAN FOR EMPTY(Chk) AND lcMainTSta = TSTATION
    Replace &lcTempName..Chk WITH lcChk
ENDSCAN
GOTO lnRecNo
=lfwBrow()
SELECT(lnAlias)
*-- End of lfSelAll.

*!*******************************************************************************
*! Name      : lfSelNon
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Select None - (For Soccer).
*!*******************************************************************************
*! Example   : =lfSelNon()
*!*******************************************************************************
*
FUNCTION lfSelNon
PRIVATE lnAlias,lnRecNo

lnAlias = SELECT()
SELECT (lcTempName)
lnRecNo = RECNO()
LOCATE
SCAN FOR !EMPTY(Chk)
  Replace &lcTempName..Chk WITH SPACE(01)
ENDSCAN
GOTO lnRecNo
=lfwBrow()
SELECT(lnAlias)
*-- End of lfSelNon

*!*******************************************************************************
*! Name      : lfInvert
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Invert Select To None Select and vice versa - (For Soccer).
*!*******************************************************************************
*! Example   : =lfInvert()
*!*******************************************************************************
*
FUNCTION lfInvert
PRIVATE lnAlias,lnRecNo

lnAlias = SELECT()
SELECT (lcTempName)
lnRecNo = RECNO()
LOCATE
SCAN
  IF lcMainTSta <> TSTATION
    =gfModalGen("TRM00000B00000","DIALOG",'','','One or more division team does not have the same team tracking station as the selected one only those matching will be inverted.')
    EXIT
  ENDIF
ENDSCAN
LOCATE
SCAN FOR lcMainTSta = TSTATION
  IF EMPTY(Chk)
    Replace &lcTempName..Chk WITH lcChk
  ELSE
    Replace &lcTempName..Chk WITH SPACE(01)
  ENDIF
ENDSCAN
GOTO lnRecNo
=lfwBrow()
SELECT(lnAlias)
*-- End of lfInvert.

*!*******************************************************************************
*! Name      : lfCancel
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Cancel all changes - (For Soccer).
*!*******************************************************************************
*! Example   : =lfCancel()
*!*******************************************************************************
*
FUNCTION lfCancel
PRIVATE lnAnswer,lnAlias,lnRecNo

lnAlias = SELECT()
lnAnswer = gfModalGen("TRM00000B00006","DIALOG",'','','Are you sure ? You will lose all changes.')
IF lnAnswer = 1
  SELECT (lcTempName)
  LOCATE
  SCAN
     Replace &lcTempName..Chk WITH &lcTempName..Flag
  ENDSCAN 
  llCancel   = .T.
  CLEAR READ
ENDIF
SELECT(lnAlias)
*-- End of lfCancel.

*!*******************************************************************************
*! Name      : lfMSelect
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : When press Select bottom(Main screen) - (For Soccer).
*!*******************************************************************************
*! Example   : =lfMSelect()
*!*******************************************************************************
*
FUNCTION lfMSelect

SELECT (lcTempName)
DELETE ALL
lcOrder    = SPACE(06)
lcAccount  = SPACE(05)
lcDivision = SPACE(04)
lcTeam     = SPACE(04)
lcLeague   = SPACE(05)
llFirst    = .T.
STORE 1 TO lnOrdSta,lnTemSta
SHOW GET lcDivision ENABLE
SHOW GET lcOrder    ENABLE
SHOW GET lcTeam     ENABLE
SHOW GET lcLeague   ENABLE
SHOW GET lcAccount  ENABLE
SHOW GET lnOrdSta   DISABLE
SHOW GET lnTemSta   DISABLE
SHOW GET pbModify   DISABLE
SHOW GET pbSelect   DISABLE
SHOW GET pbAccept   DISABLE
_CUROBJ  = OBJNUM(lcOrder)
=lfRefresh()
*-- End of lfMSelect.

*!*******************************************************************************
*! Name      : lfReturn
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Return to the main screen with all modification - (For Soccer).
*!*******************************************************************************
*! Example   : =lfReturn()
*!*******************************************************************************
*
FUNCTION lfReturn
SHOW GET lnOrdSta DISABLE
SHOW GET pbSelect DISABLE
SHOW GET pbAccept ENABLE
CLEAR READ
*-- End of lfReturn.

*!*******************************************************************************
*! Name      : lfVOrd
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : To validate order field - (For Soccer).
*!*******************************************************************************
*! Example   : =lfVOrd()
*!*******************************************************************************
*
FUNCTION lfVOrd
IF EMPTY(lcOrder) AND LASTKEY() = 13
  =gfModalGen("TRM00000B00000","DIALOG",'','','Order cannot be empty...')
  _CUROBJ  = OBJNUM(lcOrder)  
ENDIF
*-- End of lfVOrd.

*!*******************************************************************************
*! Name      : lfVOSta
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : To validate order field - (For Soccer).
*!*******************************************************************************
*! Example   : =lfVOSta()
*!*******************************************************************************
*
FUNCTION lfVOSta

IF lcMainOSta = 'A'
  IF lnOrdSta  = 3
  =gfModalGen("TRM00000B00000","DIALOG",'','','Cannot change accepted order to charted.')
  lnOrdSta  = 2
  SHOW GET lnOrdSta 
  ENDIF
ENDIF

IF lcMainOSta = 'C'
  IF lnOrdSta  = 2
  =gfModalGen("TRM00000B00000","DIALOG",'','','Cannot change charted order to accepted.')
  lnOrdSta  = 3
  SHOW GET lnOrdSta 
  ENDIF
ENDIF

IF lcMainOSta = 'A' .OR. lcMainOSta = 'C'
  IF lnOrdSta  = 1
  =gfModalGen("TRM00000B00000","DIALOG",'','','Cannot change accepted or charted order to new.')  
  IF lcMainOSta = 'A'
    lnOrdSta  = 2
  ELSE  
    lnOrdSta  = 3
  ENDIF  
  SHOW GET lnOrdSta
  ENDIF
ENDIF
*-- End of lfVOSta.

*!*******************************************************************************
*! Name      : lfVTSta
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : To validate order field - (For Soccer).
*!*******************************************************************************
*! Example   : =lfVTSta()
*!*******************************************************************************
*
FUNCTION lfVTSta

IF lcMainTSta = 'K'
  IF lnTemSta  = 2 AND lnOrdSta =1
    =gfModalGen("TRM00000B00000","DIALOG",'','','Cannot change not picked to picked in new order station.')
    lnTemSta  = 1
    SHOW GET lnTemSta ENABLE
  ENDIF
ENDIF
*-- End of lfVTSta.

*!*******************************************************************************
*! Name      : lfAccept
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : To Save changes - (For Soccer).
*!*******************************************************************************
*! Example   : =lfAccept()
*!*******************************************************************************
*
FUNCTION lfAccept
PRIVATE lnAlias

lnAlias = SELECT()
SELECT SOCCER
IF !EMPTY(lcDivision) AND !EMPTY(lcTeam)
  IF SEEK(lcOrdType+lcOrder+lcDivision + lcTeam,'SOCCER')
      DO CASE
        CASE  lnTemSta  = 1
          REPLACE SOCCER.TStation WITH 'K'
        CASE  lnTemSta  = 2
          REPLACE SOCCER.TStation WITH 'I'
        CASE  lnTemSta  = 3
          REPLACE SOCCER.TStation WITH 'T'
        CASE  lnTemSta  = 4
          REPLACE SOCCER.TStation WITH 'H'
        CASE  lnTemSta  = 5
          REPLACE SOCCER.TStation WITH 'O'
      ENDCASE
  ENDIF
  SELECT (lcTempName)
  LOCATE
  SCAN FOR !EMPTY(&lcTempName..Chk)
    SELECT Soccer
    IF SEEK(lcOrdType+lcOrder+&lcTempName..Div + &lcTempName..Team,'SOCCER')
      DO CASE
        CASE  lnTemSta  = 1
          REPLACE TStation WITH 'K'
        CASE  lnTemSta  = 2
          REPLACE TStation WITH 'I'
        CASE  lnTemSta  = 3
          REPLACE TStation WITH 'T'
        CASE  lnTemSta  = 4
          REPLACE TStation WITH 'H'
        CASE  lnTemSta  = 5
          REPLACE TStation WITH 'O'
      ENDCASE
    ENDIF
  ENDSCAN
  DELETE ALL
ELSE
  SELECT Soccer
  DO CASE
    CASE  lnOrdSta  = 1
      REPLACE ALL OStation WITH 'N' FOR Soccer.Order = lcOrder
    CASE  lnOrdSta  = 2
      REPLACE ALL OStation WITH 'A' FOR Soccer.Order = lcOrder
    CASE  lnOrdSta  = 3
      REPLACE ALL OStation WITH 'C' FOR Soccer.Order = lcOrder
  ENDCASE
ENDIF
=lfMSelect()
lcPrompt = '\<Close'
SHOW GET pbClose,1 PROMPT lcPrompt
IF llCalled
   CLEAR READ
ENDIF
SELECT(lnAlias)
*-- End of lfAccept.

*!*******************************************************************************
*! Name      : lpEsc
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : To trap Esc key - (For Soccer).
*!*******************************************************************************
*! Example   : DO lpEsc
*!*******************************************************************************
*
PROCEDURE lpEsc
_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13)
*-- End of lpEsc.

*!*******************************************************************************
*! Name      : lfTrapKey
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : Trap key - (For Soccer).
*!*******************************************************************************
*! Example   : =lfTrapKey()
*!*******************************************************************************
*
FUNCTION lfTrapKey

ON KEY LABEL TAB        DO lpTab
ON KEY LABEL BACKTAB    DO lpShiftTab
ON KEY LABEL ESC        DO lpEscD
ON KEY LABEL CTRL+ENTER DO lpEscD
*-- End of lfTrapKey.

*!*******************************************************************************
*! Name      : lpTab
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : trapping for  tab key - (For Soccer).
*!*******************************************************************************
*! Example   : DO lpTab
*!*******************************************************************************
*
PROCEDURE lpTab

DO CASE
  CASE WONTOP('SOSOC2D') AND _CUROBJ = OBJNUM(pbCancel)
    ACTIVATE WINDOW (lcDet_Ttl)

  CASE WONTOP(lcDet_Ttl)
    ACTIVATE WINDOW ('SOSOC2D')
  OTHERWISE
    _CUROBJ = _CUROBJ + 1
ENDCASE
*-- End of lpTab.

*!*******************************************************************************
*! Name      : lpShiftTab
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : trapping for Shift+Tab key - (For Soccer).
*!*******************************************************************************
*! Example   : DO lpShiftTab
*!*******************************************************************************
*
PROCEDURE lpShiftTab

DO CASE
  CASE WONTOP(lcDet_Ttl)
    ACTIVATE WINDOW ('SOSOC2D')
    _CUROBJ = OBJNUM(pbCancel)
  CASE WONTOP('SOSOC2D') AND _CUROBJ = OBJNUM(pbOk)
    ACTIVATE WINDOW (lcDet_Ttl)
  OTHERWISE
    _CUROBJ = _CUROBJ - 1
ENDCASE
*-- End of lpShiftTab.

*!*******************************************************************************
*! Name      : lpEscD
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 02/05/2001
*! Purpose   : trapping for Esc key - (For Soccer).
*!*******************************************************************************
*! Example   : DO lpEscD
*!*******************************************************************************
*
PROCEDURE lpEscD
ACTIVATE WINDOW ('SOSOC2D')
_CUROBJ = OBJNUM(pbCancel)
KEYBOARD CHR(13)
*-- End of lpEscD. 
