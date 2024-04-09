*!******************************************************************************
*! Program file  : ICSTWIP.PRG
*! Program desc. : Add STYLE WIP ADJUSTMENTS.
*! For Screen    : ICSTWIP.SPR & ICSTWIPA.SPR 
*! System        : Aria Apparel System  (A27)
*! Module        : Inventory Contol     (IC )
*! Developer     : ABDOU ELGENDI      - (ABD)
*! Date          : 12/22/99
*! Note          : FOR PEOPLE WHO DON'T HAVE MANUFACTURING AND STYLES IMPORTING MODULES.
*!******************************************************************************
*! Calls         : 
*!           Functions  : lfvStyle    ,lfGetInfo ,lfvAdj    ,lfActBrow,;
*!                      : lfwBrow     ,lfvBrows  ,lfvNew    ,lfHoldKey,;
*!                      : lfUnTrapKeys,lpDetEsc  ,lfvTrnClos,lfClrInfo,;
*!                      : lfvTrnOk    ,lfvRemove ,lfvModify ,lfvWareH ,;
*!                      : lpSavScr    ,lfRecLock ,lfReadAct ,lfTrapKy ,;
*!                      : lfOpnInfo.
*!          ------------------------------------------------------------  
*!           Procedures : lpShow      ,lpUnLock  ,lpTab     ,lpShiftTab.
*!******************************************************************************
*! Example : DO Icstwip
*!******************************************************************************
*! This Program is due to B802874 ...
*! WIP INVENTORY ADJUSTMENTS JOURNAL ONLY FOR SYSTEM THAT DO NOT HAVE IMPORTING.
*!******************************************************************************
*! Modifications      :
*!******************************************************************************
*
*-- Variable declarations [ Begin]
DIMENSION laTwip[9] ,laOTWIP[9], laOldVal[8]    &&& Arrays To Hold Wip And Old Value For WIP.
STORE SPACE(1) To lcStyHdr,lcWinCh0,lcWinCh1,lcWinCh2,lcScFields,lcStyDesc,lcTmpAdj,lcFromWare,;
                  lcHldTab,lcHldBtb,lcHldEsc,lcHldAtB,lcWFromDsc,llMultiWH,lcDefWareh,lcDyelot
STORE 0   To lnStyLngth,laTwip,laOTWIP,laOldVal,lnMjrWid,;
             m.WIP1,m.WIP2,m.WIP3,m.WIP4,m.WIP5,m.WIP6,m.WIP7,m.WIP8,m.TotWIP,;
             m.Adj1,m.Adj2,m.Adj3,m.Adj4,m.Adj5,m.Adj6,m.Adj7,m.Adj8,m.TotAdj
STORE .F. To llBrowse , llDyelot,llModiMod,llExtMFlg
*-- Variable declarations [ End ]

*-- WIP INVENTORY ADJUSTMENTS JOURNAL ONLY FOR SYSTEM THAT DO NOT HAVE IMPORTING.
IF 'MF' $ gcCmpModules .OR. 'PO' $ gcCmpModules
  *---Text : - 'You have Manufacturing and Style Purchase Orders Modules'
  =gfModalGen('TRM42080B00000','DIALOG','You have Manufacturing and Style Purchase Orders Modules')
  CLEAR READ
  RETURN
ENDIF

*-- Opening Files.
IF !gfSetup()    
  RETURN
ENDIF  

*-- The first time we run the screen.
IF !WEXIST(gcBaseWind)
  *-- Initialize some variables.
  *-- Get the setup variables from the memory files.
  
  *-- Cheak If Company Dyelot Or Not.
  llDyelot   = gfGetMemVar('M_Dyelot')   ='Y'
  
  *-- Cheak If Company Multi WareHouse Or Not.
  llMultiWH  = gfGetMemVar('M_WareHouse')='Y'
  
  lcStyHdr   = gfItemMask("HI")
  lnStyLngth = LEN(lcStyHdr)       && Var. hold the length of the style.
  lnMjrWid   = LEN(gfItemMask('PM'))   
ENDIF

*-- Declare the globle variables.
lcBrow_Ttl = "WIP Adjustment lines"
lcLevel    = IIF(llDyelot .AND. llMultiWH,'StyDye','Style')

*-- Screens    : lcBfields,gcBaseWind,lcWinCh0,lcWinCh1,lcWinCh2,lcWinCh3,ICSTWIPA.
*-- Screen Windows.
lcWinCh0  = gfTempName()
lcWinCh1  = gfTempName()
lcWinCh2  = gfTempName()
lcWinCh3  = gfTempName()

*-- Create temp adjustment file.
SELECT WIPADJ
lcTmpAdj = gfTempName()
=AFIELDS(laFStru)
lnFStru = ALEN(laFStru,1)
DIMENSION laFStru[lnFStru+1,4]
laFStru[lnFStru+1,1] = 'Dyelot'
laFStru[lnFStru+1,2] = 'C'
laFStru[lnFStru+1,3] = 10
laFStru[lnFStru+1,4] = 0

CREATE DBF (gcWorkDir+lcTmpAdj) FROM ARRAY laFStru
lcExpr=IIF(llDyelot,'Style+Dyelot+STR(RECNO(),6)','Style+STR(RECNO(),6)')
INDEX ON &lcExpr TAG &lcTmpAdj
SET RELATION TO Style INTO STYLE

SELECT (lcTmpAdj)
PUSH KEY
lnCurrRec = RECNO()

IF !llMultiWH
  *--Read the default warehouse.
  GO TOP IN WAREHOUS
  lcDefWareh = WAREHOUS.cWareCode
ENDIF
lcFromWare = IIF(llMultiWH, SPACE(6), lcDefWareh )

*-- Array To Disable The Save Bottom in The Control.
EXTERNAL ARRAY laDefProc
laDefProc[9]  = .F.      && Disable the control panel save proc.  (lpSavScr)

*-- Disable The Browe Bottom In The Control Screen.
SHOW GET pbBrws DISABLE
laCtrStat[10] = 'DISABLE'
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrow_Ttl)

lcWhStat = IIF(llMultiWH  .AND. EMPTY(lcFromWare)  ,"DISABLE", "ENABLE" )

DO (gcScrDir+gcWinAppl+"\ICSTWIP.SPX")
RELEASE WINDOW (lcBrow_Ttl)
POP KEY

*--Normal exit.
IF glQuitting
  USE IN (lcTmpAdj)
  ERASE (gcWorkDir+lcTmpAdj+'.DBF')
  ERASE (gcWorkDir+lcTmpAdj+'.CDX')
ENDIF
*End...!

*!******************************************************************************
*-- Functions and Procedures :
*!******************************************************************************
*! Name      : lpShow
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Show objecrts function.
*!******************************************************************************
*! Parameters: None
*!******************************************************************************
*! Returns   : None.
*!******************************************************************************
*! Example   : =lpShow()
*!******************************************************************************
*
FUNCTION lpShow

laCtrStat[10] = 'DISABLE'
SHOW GET pbBrws DISABLE

DO CASE

  ***--- S E L E C T   M O D E ---***
  CASE laScrMode[1]
    IF llMultiWH
      SHOW GET lcFromWare ENABLE
      SHOW GET ibWCode1   ENABLE
      _CUROBJ = OBJNUM(lcFromWare)
    ELSE
      SHOW GET pbKey ENABLE
      _CUROBJ = OBJNUM(pbNew)
    ENDIF
    SELECT (lcTmpAdj)
    lnSavRec=IIF(!EOF(),RECNO(),1)
    LOCATE
    IF FOUND()
      SHOW GEt pbRemove ENABLE
      SHOW GEt pbModify ENABLE
      SHOW GEt pbNew    ENABLE
    ELSE
      SHOW GEt pbNew    DISABLE
      SHOW GEt pbRemove DISABLE
      SHOW GEt pbModify DISABLE
    ENDIF
  ***--- A D D   M O D E ---***
  CASE laScrMode[4]
    SELECT (lcTmpAdj)
    lnSavRec=IIF(!EOF(),RECNO(),1)
    LOCATE
    IF !FOUND()
      laCtrStat[11]  =   'DISABLE'
      SHOW GET pbSav      DISABLE
      SHOW GET lcFromWare ENABLE
      SHOW GET ibWCode1   ENABLE
      SHOW GET pbKey      DISABLE
    ELSE
      laCtrStat[11]  =   'ENABLE'
      SHOW GET pbSav      ENABLE
      SHOW GET lcFromWare DISABLE
      SHOW GET ibWCode1   DISABLE
      SHOW GET pbKey      ENABLE
      IF lnSavRec<>1
        GOTO lnSavRec
      ENDIF
    ENDIF  
ENDCASE
RETURN

*-- END OF lpShow
*!********************************************************************************
*! Name      : lfvStyle
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Validate Entered Styles
*!********************************************************************************
*! Called    : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvStyle()
*!********************************************************************************
*
FUNCTION lfvStyle

IF MDOWN() OR (!llBrowse AND EMPTY(SUBSTR(m.Style,1,lnMjrWid)) )
  RETURN
ENDIF

SELECT STYLE 
m.Style=IIF(llBrowse ,'?',m.Style)
IF llBrowse OR (!EMPTY(m.Style) AND !SEEK(m.Style,'STYLE'))
  lcHdEsc1= ON('KEY','ESC')
  ON KEY LABEL ESC 
  m.Style = gfStyBrw('I','','',.F.)
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
  SHOW GETS
  =lfRefresh('ICSTWIPA')
ENDIF
RETURN

*-- End OF lfvStyle.
*!********************************************************************************
*! Name      : lfvDyelot()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Valid function to validate the dyelot.
*!********************************************************************************
*! Calls     : DO gpStDyBrow WITH lcStyle,lcWareHous,lcDyelot
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvDyelot()
*!********************************************************************************
*
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

*-- Return To The Select Mode.
laScrMode = .F.
laScrMode[1] = .T.
*-- Get The Information From The STYDYE.
=lfGetInfo()

_CUROBJ = OBJNUM(ldDate)
=lfRefresh('ICSTWIPA')
SHOW GET ibDye    DISABLE
SHOW GET m.Dyelot DISABLE

RETURN

*-- End Of lfvDyelot.
*!********************************************************************************
*! Name      : lfGetInfo
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Get the style key accepted information.
*!********************************************************************************
*! Called    : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Return    : None.
*!********************************************************************************
*! Example   : =lfGetInfo()
*!********************************************************************************
*
FUNCTION lfGetInfo

SELECT STYLE
=SEEK (m.Style)
lcSScale  = Scale
lcStyDesc = Desc1
SELECT (lcLevel)
IF lcLevel # 'Style'
  IF !SEEK(PADR(m.Style,19)+lcFromWare+SPACE(10) ) .AND. llMultiWH
    *-Style: xxx is not assigned to warehouse: xxx. "\<Add;\<Reenter"
    lnChoice=gfModalGen('TRM42056B42006','DIALOG',ALLTRIM(m.Style)+'|'+lcFromWare)
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


*-- Scatter filed To Memvar That Existing Before That.
SELECT (lcLevel)
SCATTER FIELDS WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TotWIP MEMVAR
SCATTER FIELDS WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TotWIP To laTwip
SCATTER FIELDS WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TotWIP To laOTwip

SHOW GET m.Style DISABLE
SHOW GET ibStyle DISABLE

IF llMultiWH
  = SEEK(PADR(m.Style,19)+lcFromWare+lcDyelot) 
ELSE
  IF llDyelot
    = SEEK(PADR(m.Style,19)+lcDefWareh+lcDyelot) 
  ELSE
    = SEEK(m.Style) 
  ENDIF
ENDIF

=SEEK('S'+lcSScale,'SCALE')
FOR I=1 TO 8
  Z=STR(I,1)
  IF EMPTY(SCALE.Sz&Z)
    m.Adj&Z = 0
    SHOW GET m.Adj&Z DISABLE
  ELSE
    SHOW GET m.Adj&Z ENABLE
  ENDIF
ENDFOR

SELECT (lcTmpAdj)
SHOW GET pbNewOk    ENABLE
SHOW GET pbNClose,1 PROMPT gcBmpHome+"can.bmp"

IF llDyelot
  IF Style.cDye_Flg='Y'
    SHOW GET ibDye    ENABLE
    SHOW GET m.Dyelot ENABLE
  ELSE
    SHOW GET ibDye    DISABLE
    SHOW GET m.Dyelot DISABLE
  ENDIF 
ENDIF

SHOW GET ldDate     ENABLE
SHOW GET lcReason   ENABLE
SHOW GET pbNewOk    ENABLE  
SHOW GET m.TotAdj   DISABLE
SHOW GETS
llExtMFlg = .T.
=lfRefresh('ICSTWIPA')
RETURN

*-- End Of lfGetInfo.
*!********************************************************************************
*! Name      : lfvAdj()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Calculate the total adjustment.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: lcCont->No of size adjusted.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvAdj()
*!********************************************************************************
*
FUNCTION lfvAdj
PARAMETER lcCont


I=INT(VAL(lcCont))
IF  m.Adj&lcCont # laOldVal[I]
  IF llModiMod
    laTWIP[I] = ( laOTWIP[I] - &lcTmpAdj..Adj&lcCont ) + m.Adj&lcCont  
  ELSE
    laTWIP[I] = laOTWIP[I] + m.Adj&lcCont
  ENDIF
  *-- Save The Old Value To Voild Accumulate More Than One Time.
  laOldVal[I] = m.Adj&lcCont
  
  laTwip[9]   = laTWIP[1]+laTWIP[2]+laTWIP[3]+laTWIP[4]+laTWIP[5]+laTWIP[6]+laTWIP[7]+laTWIP[8]
  m.TotAdj    = m.Adj1+m.Adj2+m.Adj3+m.Adj4+m.Adj5+m.Adj6+m.Adj7+m.Adj8
  SHOW GET m.TotAdj DISABLE
ENDIF
=lfRefresh('ICSTWIPA')
RETURN

*-- End OF lfvAdj.
*!********************************************************************************
*! Name      : lfActBrow()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Function to activate the browse window.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfActBrow()
*!********************************************************************************
*
FUNCTION lfActBrow

SELECT (lcTmpAdj)
DO CASE

CASE llDyelot .AND. llMultiWH
  lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
               "STYLE        :30 :H=lcStyHdr :R,"+;
               "STYLE.Desc   :24 :H='Description':R,"+;
               "Dyelot       :19 :H='Dyelot'     :R,"+;
               "cWareCode    :19 :H='Warehous'   :R,"+;
               "Date         :10 :H='Date':R,"+;
               "cReason      :25 :H='Reason':R,"+;
               "STYLE.TOTWIP :12 :P='9999999' :H='Existing':R,"+;
               "TotAdj       :12 :P='9999999' :H='Adj +\-':R,"+;
               "Total=STYLE.TOTWIP+TotAdj :12 :P='9999999' :H='  Total':R "

CASE llDyelot .AND. !llMultiWH
  lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
               "STYLE        :30 :H=lcStyHdr     :R,"+;
               "STYLE.Desc   :24 :H='Description':R,"+;
               "Dyelot       :19 :H='Dyelot'     :R,"+;
               "Date         :10 :H='Date'       :R,"+;
               "cReason      :25 :H='Reason'     :R,"+;
               "STYLE.TOTWIP :12 :P='9999999' :H='Existing':R,"+;
               "TotAdj       :12 :P='9999999' :H='Adj +\-':R,"+;
               "Total=STYLE.TOTWIP+TotAdj :12 :P='9999999' :H='  Total':R "

CASE llMultiWH .AND. !llDyelot
  lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
               "STYLE        :30 :H=lcStyHdr     :R,"+;
               "STYLE.Desc   :24 :H='Description':R,"+;
               "cWareCode    :19 :H='Warehous'   :R,"+;
               "Date         :10 :H='Date'       :R,"+;
               "cReason      :25 :H='Reason'     :R,"+;
               "STYLE.TOTWIP :12 :P='9999999' :H='Existing':R,"+;
               "TotAdj       :12 :P='9999999' :H='Adj +\-':R,"+;
               "Total=STYLE.TOTWIP+TotAdj :12 :P='9999999' :H='  Total':R "

CASE !llMultiWH .AND. !llDyelot
  lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
               "STYLE        :30 :H=lcStyHdr     :R,"+;
               "STYLE.Desc   :24 :H='Description':R,"+;
               "Date         :10 :H='Date'       :R,"+;
               "cReason      :25 :H='Reason'     :R,"+;
               "STYLE.TOTWIP :12 :P='9999999' :H='Existing':R,"+;
               "TotAdj       :12 :P='9999999' :H='Adj +\-':R,"+;
               "Total=STYLE.TOTWIP+TotAdj :12 :P='9999999' :H='  Total':R "
ENDCASE
lcBrowWind=IIF(llMultiWH ,lcWinCh1,lcWinCh3)
BROWSE FIELDS &lcBfields;
       NOAPPEND ;
       NOCLEAR  ;
       NODELETE ;
       NOMENU   ;
       NOWAIT   ;
       SAVE     ;
       TITLE (lcBrow_Ttl) ;
       WHEN lfwBrow()     ;
       VALID :F lfvBrows();
       WINDOW (lcBrowWind);
       IN WINDOW (gcBaseWind)
RETURN

*-- End Of lfActBrow.
*!********************************************************************************
*! Name      : lfwBrow
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : When Browse function.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfwBrow()
*!********************************************************************************
*
FUNCTION lfwBrow
lnCurrRec = RECNO()
SHOW WINDOW (lcBrow_Ttl) REFRESH
glFromBrow = .T.
RETURN

*-- End OF lfwBrow.
*!********************************************************************************
*! Name      : lfvBrows
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Valid Browse function.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvBrows()
*!********************************************************************************
*
FUNCTION lfvBrows

IF WONTOP() # (lcBrow_Ttl)
  = gfStopBrow()
ENDIF

*-- End OF lfvBrows.
*!********************************************************************************
*! Name      : lfvNew()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Add new lines.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvNew()
*!********************************************************************************
*
FUNCTION lfvNew

STORE .F. To llExtMFlg , llModiMod

SELECT (lcTmpAdj)
lcScFields = 'Style,date,cReason,Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj'
SCATTER FIELDS &lcScFields MEMO MEMVAR BLANK
STORE 0 TO m.WIP1,m.WIP2,m.WIP3,m.WIP4,m.WIP5,m.WIP6,m.WIP7,m.WIP8,m.TotWIP,laTWIP,laOldVal

m.Style = SPACE(19)
ldDate  = gdSysDate
lcStyDesc  = ''

=lfHoldKey('S')
=lfUnTrapKeys()
SELECT STYLE
DO (gcScrDir+gcWinAppl+"\ICSTWIPA.SPX")
=lfHoldKey('R')

SELECT (lcTmpAdj)
IF EOF()
  laScrMode    = .F.
  laScrMode[1] = .T.
ENDIF
lnCurrRec = RECNO()
GO TOP
=lfActBrow()

IF !EOF()
  GOTO lnCurrRec
  _CUROBJ = OBJNUM(pbNew)
  laScrMode    = .F.
  laScrMode[4] = .T.
ENDIF
SHOW GETS
*=lfRefresh('ICSTWIP')
RETURN

*-- End OF lfvNew.
*!********************************************************************************
*! Name      : lfHoldKey
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Save and restore traping.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: lcKyOper -> Operation Save or Restore
*!********************************************************************************
*! Returns   :  None.
*!********************************************************************************
*! Example   :  =lfHoldKey()
*!********************************************************************************
*
FUNCTION lfHoldKey
PARAMETER lcKyOper

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

*-- End OF lfHoldKey.
*!********************************************************************************
*! Name      : lfUnTrapKeys
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Releases Key traps necessary for browse.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfUnTrapKeys()
*!********************************************************************************
*
FUNCTION lfUnTrapKeys

ON KEY LABEL TAB        
ON KEY LABEL BACKTAB    
ON KEY LABEL ESC DO lpDetEsc
RETURN

*-- End OF lfUnTrapKeys.
*!********************************************************************************
*! Name      : lpDetEsc
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Trap Esc for lines entry.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : DO lpDetEsc
*!********************************************************************************
*
PROCEDURE lpDetEsc
_CUROBJ = OBJNUM(pbNClose)
KEYBOARD '{ENTER}'
RETURN

*-- End OF lpDetEsc.
*!********************************************************************************
*! Name      : lfvTrnClos()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Valid function new/modify close.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvTrnClos()
*!********************************************************************************
*
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
CLEAR READ
RETURN

*-- End OF lfvTrnClos.
*!********************************************************************************
*! Name      : lfClrInfo()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Get the style key accepted information.
*!             or initial information.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfClrInfo()
*!********************************************************************************
*
FUNCTION lfClrInfo

STORE 0 TO m.WIP1,m.WIP2,m.WIP3,m.WIP4,m.WIP5,m.WIP6,m.WIP7,m.WIP8,m.TotWIP,laTWIP,laOTWIP
STORE 0 TO m.Adj1,m.Adj2,m.Adj3,m.Adj4,m.Adj5,m.Adj6,m.Adj7,m.Adj8,m.TotAdj,laOldVal
STORE '' To lcStyDesc,lcReason
llExtMFlg = .F.

=SEEK('P','SCALE')              &&To show empty sizes.
SHOW GET pbNewOk    DISABLE
SHOW GET pbNClose,1 PROMPT gcBmpHome+"cls.bmp"

IF llDyelot
  *-- If Dyelot Clear Info. And Made It Disable.
  STORE SPACE(10) TO m.Dyelot
  SHOW GET ibDye    DISABLE
  SHOW GET m.Dyelot DISABLE
ENDIF 

SHOW GET m.Style    ENABLE
SHOW GET ibStyle    ENABLE
SHOW GET ldDate     DISABLE
SHOW GET lcReason   DISABLE
SHOW GET m.Adj1     DISABLE
SHOW GET m.Adj2     DISABLE
SHOW GET m.Adj3     DISABLE
SHOW GET m.Adj4     DISABLE
SHOW GET m.Adj5     DISABLE
SHOW GET m.Adj6     DISABLE
SHOW GET m.Adj7     DISABLE
SHOW GET m.Adj8     DISABLE
SHOW GET m.TotAdj   DISABLE

SHOW GETS
=lfRefresh('ICSTWIPA')
RETURN

*--End Of lfClrInfo.
*!********************************************************************************
*! Name      : lfvTrnOk
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Valid function for Transaction lines fields.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvTrnOk()
*!********************************************************************************
*
FUNCTION lfvTrnOk

*-- Cheack If Come From Modification Or Add A new  Recored.
IF llModiMod    &&& IF Modification
  IF m.Adj1=0 .AND. m.Adj2=0 .AND. m.Adj3=0 .AND. m.Adj4=0 .AND.;
    m.Adj5=0 .AND. m.Adj6=0 .AND. m.Adj7=0 .AND. m.Adj8=0
    *--All adjustments becomes zero,Cannot modify.
    = gfModalGen('INM42061B42001','DIALOG')
  ELSE
    SELECT (lcTmpAdj)
    REPLACE cReason    WITH lcReason   ,;
            Date       WITH ldDate     ,;
            Adj1       WITH m.Adj1     ,;
            Adj2       WITH m.Adj2     ,;
            Adj3       WITH m.Adj3     ,;
            Adj4       WITH m.Adj4     ,;
            Adj5       WITH m.Adj5     ,;
            Adj6       WITH m.Adj6     ,;
            Adj7       WITH m.Adj7     ,;
            Adj8       WITH m.Adj8     ,;                                                            
            TotAdj     WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8
  ENDIF
 CLEAR READ
ELSE  && !llModiMod
  IF m.Adj1=0 .AND. m.Adj2=0 .AND. m.Adj3=0 .AND. m.Adj4=0 .AND. ;
    m.Adj5=0 .AND. m.Adj6=0 .AND. m.Adj7=0 .AND. m.Adj8=0
    *--All adjustments becomes zero.
    =gfModalGen('INM42060B42001','DIALOG')
  ELSE
    SELECT (lcTmpAdj)  
    IF SEEK(m.Style)
      REPLACE cReason    WITH lcReason   ,;
              Date       WITH ldDate     ,;
              Adj1       WITH m.Adj1     ,;
              Adj2       WITH m.Adj2     ,;
              Adj3       WITH m.Adj3     ,;
              Adj4       WITH m.Adj4     ,;
              Adj5       WITH m.Adj5     ,;
              Adj6       WITH m.Adj6     ,;
              Adj7       WITH m.Adj7     ,;
              Adj8       WITH m.Adj8     ,;                                                            
              TotAdj     WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8
    ELSE
      APPEND BLANK
      REPLACE Style      WITH m.Style    ,;
              cReason    WITH lcReason   ,;
              Date       WITH ldDate     ,;
              Dyelot     WITH IIF(llDyelot,m.Dyelot,'')    ,;
              cWareCode  WITH IIF(llMultiWH,lcFromWare,'') ,;
              Adj1       WITH m.Adj1     ,;
              Adj2       WITH m.Adj2     ,;
              Adj3       WITH m.Adj3     ,;
              Adj4       WITH m.Adj4     ,;
              Adj5       WITH m.Adj5     ,;
              Adj6       WITH m.Adj6     ,;
              Adj7       WITH m.Adj7     ,;
              Adj8       WITH m.Adj8     ,;                                                            
              TotAdj     WITH Adj1+Adj2+Adj3+Adj4+Adj5+Adj6+Adj7+Adj8
    ENDIF
    SELECT (lcLevel)
    IF lcLevel='Style'
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
ENDIF 
RETURN

*-- End Of lfvTrnOk.
*!********************************************************************************
*! Name      : lfvRemove()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Remove added transaction.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvRemove()
*!********************************************************************************
*
FUNCTION lfvRemove

*--The Style :XXX will be removed?','\<Yes;\<No
IF gfModalGen('TRM42065B42002','DIALOG',ALLTRIM(Style)) = 1
  SELECT (lcTmpAdj)  
  DELETE
  GO TOP
  IF EOF()
    laScrMode    = .F.
    laScrMode[1] = .T.
    SHOW GEt pbRemove DISABLE
    SHOW GEt pbModify DISABLE
  ENDIF
  lnCurrRec=RECNO()
  =lfActBrow()
ENDIF
SHOW GETS
RETURN

*-- End OF lfvRemove.
*!*********************************************************************************
*! Name      : lfvModify()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Modify added transaction.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvModify()
*!********************************************************************************
*
FUNCTION lfvModify

llModiMod = .T.
SELECT (lcTmpAdj)
lnCurrRec = IIF(!EOF(),RECNO(),1)

lcScFields = 'Style,Dyelot,date,cReason,Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj'
SCATTER FIELDS &lcScFields MEMO MEMVAR
STORE 0 TO m.WIP1,m.WIP2,m.WIP3,m.WIP4,m.WIP5,m.WIP6,m.WIP7,m.WIP8,m.TotWIP,laTStk,laOTStk
ldDate    = Date
lcReason  = cReason

=lfHoldKey('S')
=lfUnTrapKeys()
=lfGetInfo()

SHOW GET ldDate     ENABLE
SHOW GET lcReason   ENABLE
DO (gcScrDir+gcWinAppl+"\ICSTWIPA.SPX")
Z=lfHoldKey('R')

SELECT (lcTmpAdj)
GOTO lnCurrRec
SHOW WINDOW (lcBrow_Ttl) REFRESH
SHOW GETS
_CUROBJ = OBJNUM(pbModify)
RETURN

*-- End Of lfvModify.
*!********************************************************************************
*! Name      : lfvWareH()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Validate the warehouses.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: lcWareH  -> Warehouse code.
*!             lnWareNm -> Which warehouse ,1st or 2nd.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfvWareH()
*!********************************************************************************
*
FUNCTION lfvWareH

IF MDOWN() OR (!llBrowse .AND. EMPTY(lcFromWare))
  RETURN
ENDIF

IF llBrowse OR (!EMPTY(lcFromWare) .AND. !SEEK(lcFromWare,'WareHous'))
  = gfBrowWare(.F.)
  lcFromWare = WareHous.cWareCode
ENDIF
lcWFromDsc = IIF(EMPTY(WareHous.cDesc),'',WareHous.cDesc)

IF EMPTY(lcFromWare) 
  SHOW GET pbNew DISABLE
ELSE
  SHOW GET pbNew ENABLE
ENDIF  
llBrowse = .F.
=lfRefresh()
RETURN

*-- End OF lfvWareh
*!********************************************************************************
*! Name      : lpSavScr()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Update the master files.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lpSavScr()
*!********************************************************************************
*
FUNCTION lpSavScr

*--Save to master files.
DIMENSION laToSave[8]
SELECT (lcTmpAdj)
SCAN 

  SCATTER MEMVAR MEMO 
  *--Check if nothing to adjust or transfer (all Zero).  
  llZeroAdj = ( m.adj1=0 .AND. m.adj2=0 .AND. m.adj3=0 .AND. m.adj4=0 .AND. ;
                m.adj5=0 .AND. m.adj6=0 .AND. m.adj7=0 .AND. m.adj8=0  )
  *-- If no adjustment, loop (no thing to update)
  IF llZeroAdj
    LOOP
  ENDIF
  
  WAIT WINDOW 'Updating the WIP for style:'+STYLE NOWAIT
  *-- Add new recored in the WIPADJ File.

  INSERT INTO WIPADJ FROM MEMVAR
  
  = SEEK(Style + lcFromWare + SPACE(10),'STYDYE')
  llDyeLvl = (llDyelot AND STYLE.cDye_Flg = 'Y' )
  IF llDyeLvl
    SET MULTILOCKS ON
  ENDIF 
  
  *-- A record lock can't be placed on a record already locked by another user
  *-- or in a table/.DBF locked by another user.  If the record lock or locks 
  *-- can't be placed for any reason, a false value (.F.) is returned.

  llGoOn = (RLOCK('STYDYE') OR lfRecLock('STYDYE')) AND ;
           (RLOCK('STYLE' ) OR lfRecLock('STYLE' ))

  IF llDyeLvl AND llGoOn
    =SEEK(Style+lcFromWare+ SPACE(10),'STYDYE')
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

    
  *-- Update The Style File.
  SELECT STYLE
  REPLACE WIP1   WITH WIP1 + m.ADJ1 , WIP2 WITH WIP2 + m.ADJ2 ,;
          WIP3   WITH WIP3 + m.ADJ3 , WIP4 WITH WIP4 + m.ADJ4 ,;
          WIP5   WITH WIP5 + m.ADJ5 , WIP6 WITH WIP6 + m.ADJ6 ,;
          WIP7   WITH WIP7 + m.ADJ7 , WIP8 WITH WIP8 + m.ADJ8 ,;
          TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8

  *-- Update The STYDYE File.
  SELECT STYDYE
  REPLACE WIP1   WITH WIP1 + m.ADJ1 , WIP2 WITH WIP2 + m.ADJ2 ,;
          WIP3   WITH WIP3 + m.ADJ3 , WIP4 WITH WIP4 + m.ADJ4 ,;
          WIP5   WITH WIP5 + m.ADJ5 , WIP6 WITH WIP6 + m.ADJ6 ,;
          WIP7   WITH WIP7 + m.ADJ7 , WIP8 WITH WIP8 + m.ADJ8 ,;
          TotWip WITH Wip1+Wip2+Wip3+Wip4+Wip5+Wip6+Wip7+Wip8

  SELECT (lcTmpAdj)
  DO lpUnLock
ENDSCAN

SELECT (lcTmpAdj)
ZAP
WAIT CLEAR

*--TexT :- print the work in process adjustment journal report ?
IF gfModalGen('TRM42208B42002','DIALOG','the work in process adjustment journal') = 1
  =lfActBrow()
  laScrMode = .F.
  laScrMode[1] = .T.
  SHOW GETS  
  =gfStatic()                   && Save inviroment before terminate
  CLEAR READ                    && Terminat old program
  =gfClrBro()                   && Call Function to release browse windows
  IF glFromBrow 
    glFromBrow = .F.
    ON KEY LABEL CTRL+Q
    KEYBOARD "{CTRL+Q}" PLAIN CLEAR
  ENDIF  
  gcNextRepo = "ICSTYWIP"
ELSE
  CLEAR READ  
ENDIF  

*-- End Of lpSavScr.
*!********************************************************************************
*! Name      : lfRecLock
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Record Lock.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: lcFile->Locked file.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfRecLock()
*!********************************************************************************
*
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

*-- End Of lfRecLock.
*!********************************************************************************
*! Name      : lpUnLock
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Unlock the saved locked records.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : DO lpUnLock
*!********************************************************************************
*
PROCEDURE lpUnLock
IF llDyeLvl
  SET MULTILOCKS OFF
ELSE
  UNLOCK ALL
ENDIF

*-- End OF lpUnLock.
*!********************************************************************************
*! Name      : lfReadAct
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Read Activate function clear traping.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfReadAct()
*!********************************************************************************
*
FUNCTION lfReadAct

 ON KEY LABEL TAB
 ON KEY LABEL BACKTAB
    
*-- End OF lfReadAct.
*!********************************************************************************
*! Name      : lfTrapKy
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Trap key.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfTrapKy()
*!********************************************************************************
*
FUNCTION lfTrapKy

IF WONTOP() = lcBrow_Ttl
  ON KEY LABEL TAB     DO lptab
  ON KEY LABEL backtab DO lpshifttab
ENDIF  
RETURN

*-- End Of lfTrapKy.
*!********************************************************************************
*! Name      : lpTab
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Tab key trapping.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : DO lpTab
*!********************************************************************************
*
PROCEDURE lpTab

IF WONTOP(lcBrow_Ttl)
  _CUROBJ=OBJNUM(pbNew)
  ACTIVATE WINDOW (lcWinCh2)
ELSE
  IF WONTOP("gwccontrl1")
    _CUROBJ=OBJNUM(lcFromWare)
    ACTIVATE WINDOW (lcWinCh0)
  ELSE
    _CUROBJ=_CUROBJ+1
  ENDIF
ENDIF
RETURN

*-- End OF lpTab.
*!********************************************************************************
*! Name      : lpShiftTab
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Shift Tab key trapping.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : DO lpShiftTab
*!********************************************************************************
*
PROCEDURE lpShiftTab 

IF WONTOP(lcBrow_Ttl)
  _CUROBJ=OBJNUM(lcFromWare)
  ACTIVATE WINDOW (lcWinCh0)
ELSE
    ACTIVATE WINDOW ("gwccontrl1")
    _CUROBJ=OBJNUM(pbCls)
ENDIF
RETURN

*-- End OF lpShiftTab. 
*!********************************************************************************
*! Name      : lfOpnInfo()
*! Developer : ABDOU ELGENDI - (ABD)
*! Date      : 12/22/1999
*! Purpose   : Show gets in modify mode.
*!********************************************************************************
*! Calls     : None.
*!********************************************************************************
*! Parameters: None.
*!********************************************************************************
*! Returns   : None.
*!********************************************************************************
*! Example   : =lfOpnInfo()
*!********************************************************************************
*
FUNCTION lfOpnInfo

IF !llModiMod
  RETURN
ENDIF

SELECT STYLE
=SEEK(m.Style)
lcSScale=Scale
lcStyDesc=Desc1

SELECT (lcLevel)
*-- If Multi WearHouse Seek With WearHouse.
IF llMultiWH
  = SEEK(PADR(m.Style,19)+lcFromWare+SPACE(10))
ELSE
  IF llDyelot
    = SEEK(PADR(m.Style,19)+lcDefWareh+SPACE(10))
  ELSE
    = SEEK(m.Style) 
  ENDIF
ENDIF

SCATTER FIELDS WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TotWIP TO laTWIP
SCATTER FIELDS WIP1,WIP2,WIP3,WIP4,WIP5,WIP6,WIP7,WIP8,TotWIP TO laOTWIP
FOR I=1 TO 8
  Z=STR(I,1)
  laTWIP [I] = laTWIP[I] + &lcTmpAdj..Adj&Z
  laOTWIP[I] = laTWIP[I]
ENDFOR
laTWIP [9] = laTWIP[1]+laTWIP[2]+laTWIP[3]+laTWIP[4]+laTWIP[5]+laTWIP[6]+laTWIP[7]+laTWIP[8]
laOTWIP[9] = laTWIP[9]

SELECT(lcTmpAdj)

SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj MEMVAR
SCATTER FIELDS Adj1,Adj2,Adj3,Adj4,Adj5,Adj6,Adj7,Adj8,TotAdj To laOldVal

=SEEK('S'+lcSScale,'SCALE')
SELECT (lcTmpAdj)
SHOW GET pbNewOk    ENABLE

SHOW GET pbNClose,1 PROMPT gcBmpHome+"can.bmp"
SHOW GET ibStyle    DISABLE
SHOW GET ibDye      DISABLE
SHOW GET ldDate     ENABLE
SHOW GET lcReason   ENABLE  

FOR I=1 TO 8
  Z=STR(I,1)
  IF !EMPTY(SCALE.Sz&Z)
    SHOW GET m.Adj&Z ENABLE
  ELSE
    m.Adj&Z = 0
    SHOW GET m.Adj&Z DISABLE
  ENDIF
ENDFOR
=lfRefresh('ICSTWIPA')
_CUROBJ = OBJNUM(ldDate)
RETURN

*-- End OF lfOpnInfo.
*!********************************************************************************