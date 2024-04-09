*:********************************************************************
*: Procedure file: SMWCUST.PRG  N#000185
*:                 (Codes)
*:
*:         System: ARIA ADVANTAGE SERIES
*:         Module: SYSTEM MANAGER
*:         Author: Ahmed Salah Shalaby - (SSH)
*:      Copyright (c) 
*:********************************************************************
*: Modification :
*:********************************************************************
PRIVATE laCodName , laCodes
DECLARE laCodes[1,2] , laCodName[1,2] , laLiStarr[1,2]
laCodes    = SPACE(1)
laCodName  = SPACE(1)
laLiStarr  = SPACE(1)
lcPrompt   = "S\<elect"    && Hold the word "S\<elect"
=gfOpenFile(gcSysHome+"SYWCUST","CUSTOMER","SH")
lcCustTmp = gfTempName()
lcStorTmp = gfTempName()
lcCScrHlp = 'Select customer'
PRIVATE lnCodes
lnAllCodes = 0
lnCodes = 1
lcWinTitl  = "Customer Screen"
=lfWRun()
lcABrowTt = "Default Customer"
lcAccount = &lcCustTmp..Account
lcSBrowTt  = "Customer : "+lcAccount+" stores"

DO (gcScrDir + gcWinAppl + '\SMWCUST.SPR')

=lfEraTemp(lcCustTmp)
=lfEraTemp(lcStorTmp)
POP KEY
RELEASE WINDOW (lcABrowTt)
RELEASE PAD _Option OF _MSYSMENU

RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01

RELEASE WINDOW (lcSBrowTt)
RELEASE PAD _Option OF _MSYSMENU

RELEASE BAR 101 OF P01PU01
RELEASE BAR 102 OF P01PU01

*!*************************************************************
*! Name      : lfWRun
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Function to collect customer & stores ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWRun()
*!*************************************************************
*!
FUNCTION lfWRun

*--- Select fields for the browse 
*--- Account , Btname , cAddress3 , cAddress4 , cAddress5 , Phone1 , Buyer , SalesRep
SELECT " " AS cMark , IIF(SYWCUST.lSelect,"»"," ") As cSelect ,Account , Btname , cAddress3 , cAddress4 , cAddress5 , Phone1 , Buyer , SalesRep;
    FROM SYWCUST ;
    WHERE Type="M" ;
    INTO DBF gcWorkDir+(lcCustTmp)
INDEX ON Account TAG (lcCustTmp)

SELECT " " AS cMark , IIF(SYWCUST.lSelect,"»"," ") As cSelect ,Account ,Store, Btname , cAddress3 , cAddress4 , cAddress5 , Phone1 , Buyer , SalesRep;
    FROM SYWCUST ;
    WHERE Type="S" ;
    INTO DBF gcWorkDir+(lcStorTmp)
INDEX ON Account+Store TAG (lcStorTmp)

*!*************************************************************
*! Name      : lfUnTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Activate screen function....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
*!
FUNCTION lfUnTrap

* -- Clear Trap
IF glFromBrow
  = gfStopBrow()
  glFromBrow = .F.
ENDIF

*-- If TOP window is not one of the browses window                                        
IF (ALLTRIM(WONTOP()) # ALLTRIM((lcABrowTt))) .OR. (ALLTRIM(WONTOP()) # ALLTRIM((lcSBrowTt)))
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL CTRL+TAB
  ON KEY LABEL CTRL+ENTER
  ON KEY LABEL CTRL+HOME
  ON KEY LABEL CTRL+END
  ON KEY LABEL CTRL+W
  ON KEY LABEL ENTER
ENDIF
*-- end of lfUnTrap.

*!*************************************************************
*! Name      : lfKey
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Screen ICINVLK When Function.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Calls              :  
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfKey()
*!*************************************************************
*!
FUNCTION lfKey

PUSH KEY
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcABrowTt KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcABrowTt)


DEFINE BAR 101 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 102 OF P01PU01 PROMPT lcSBrowTt KEY ALT+S
ON SELECTION BAR 102 OF P01PU01 ACTIVATE WINDOW (lcSBrowTt)

*!*************************************************************
*! Name      : lfTrap
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trap function for Browse....
*!           : Deactivate screen function.
*!*************************************************************
*! Calls     : 
*!             Procedures : lpTab,lpBackTab
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrap()
*!*************************************************************
FUNCTION lfTrap

*-- if TOP window is one of the browse window
IF (ALLTRIM(WONTOP()) = ALLTRIM((lcABrowTt)) ) .OR.  (ALLTRIM(WONTOP()) = ALLTRIM((lcSBrowTt)))
  glFromBrow = .T.
  ON KEY LABEL CTRL+ENTER lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+HOME  lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+W     lnDummy = 1		&&Do nothing
  ON KEY LABEL CTRL+END   lnDummy = 1		&&Do nothing
  ON KEY LABEL TAB           DO lpTab
  ON KEY LABEL CTRL+TAB      DO lpTab
  ON KEY LABEL BACKTAB       DO lpBackTab
  ON KEY LABEL ENTER         DO lpEnter
  IF (ALLTRIM(WONTOP()) = ALLTRIM((lcABrowTt)) )
    =lfGetHelp("CUSTSCR")
  ENDIF
  IF (ALLTRIM(WONTOP()) = ALLTRIM((lcSBrowTt)))
    =lfGetHelp("STORSCR")
  ENDIF
ENDIF
*-- end of lfTrap.


*!*************************************************************
*! Name      : lpTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping TAB order for browse window
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpTab
*!*************************************************************
PROCEDURE lpTab

ON KEY LABEL TAB
ON KEY LABEL CTRL+TAB

*-- Go to next window
DO CASE
  CASE ALLTRIM(WONTOP()) = ALLTRIM((lcABrowTt))
    ACTIVATE WINDOW SMWCUST3
    _CUROBJ = OBJNUM(pbSel)
  CASE ALLTRIM(WONTOP()) = ALLTRIM((lcSBrowTt))
    ACTIVATE WINDOW SMWCUST4
    _CUROBJ = OBJNUM(pbSSel)
  OTHERWISE
   ACTIVATE WINDOW (lcABrowTt)
  _CUROBJ = OBJNUM(ibBBrow)
ENDCASE

*-- end of lpTab.

*!*************************************************************
*! Name      : lpBackTab
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Trapping BACKTAB order for browse window
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpBackTab
*!*************************************************************
PROCEDURE lpBackTab

ON KEY LABEL BACKTAB

DO CASE
  CASE ALLTRIM(WONTOP()) = ALLTRIM((lcABrowTt))
    ACTIVATE WINDOW SMWCUST2
    _CUROBJ = OBJNUM(pbResume)
  CASE ALLTRIM(WONTOP()) = ALLTRIM((lcSBrowTt))
    ACTIVATE WINDOW SMWCUST3
    _CUROBJ = OBJNUM(pbInv)
  OTHERWISE
   ACTIVATE WINDOW (lcABrowTt)
  _CUROBJ = OBJNUM(ibBBrow) && Activate control panel window
ENDCASE
*-- end of lpBackTab.

*!*************************************************************
*! Name      : lfActsBrow
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
FUNCTION lfActsBrow

SELECT &lcCustTmp
GO TOP

*--- Account , Btname , cAddress3 , cAddress4 , cAddress5 , Phone1 , Buyer , SalesRep

lcBrfield1 = [cMark     :02 :R :H = ''             ,] +;
             [cSelect   :02 :R :H = ''             ,] +;
             [Account   :15 :R :H = 'Customer'     ,] +;
             [Btname    :20 :R :H = 'Customer name',] +;
             [cAddress3 :20 :R :H = 'City'         ,] +;
             [cAddress4 :20 :R :H = 'Province'     ,] +;
             [cAddress5 :20 :R :H = 'Postal c'     ,] +;
             [Phone1    :16 :R :H = 'Phone#'       ,] +;
             [Buyer     :20 :R :H = 'Buyer'        ,] +;
             [SalesRep  :04 :R :H = 'SalesRep'      ]


BROWSE FIELDS &lcBrField1;
       WHEN lfwBrows('1');
       VALID :F lfvBrowse() ;
       WINDOW SMWCUSTB IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcABrowTt)


*!*************************************************************
*! Name      : lfwBrows
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : When Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : lfShowBtns
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfwBrows()
*!*************************************************************
*!
FUNCTION lfwBrows
PARAMETER lcBrowNo
IF lcBrowNo = '1'
  SELECT (lcStorTmp)
  SET FILTER TO
  SET FILTER TO Account+Store = &lcCustTmp..Account
  GO TOP
  SELECT (lcCustTmp)
ELSE
  SELECT (lcStorTmp)
ENDIF
lnNewRec = RECNO()
REPLACE ALL cMark WITH ' '
GO TOP
IF !EOF()
  GO lnNewRec
ENDIF
REPLACE cMark WITH '>'

IF lcBrowNo = '1'
  lcSBrowTt  = "Customer : "+Account+" stores"
  SHOW WINDOW (lcSBrowTt) REFRESH
  =lfActsSBrow()
  lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
  SHOW GET pbSSel,1 PROMPT lcPrompt
  SHOW WINDOW (lcABrowTt) REFRESH
  lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
  SHOW GET pbSel,1 PROMPT lcPrompt
ELSE
  SHOW WINDOW (lcSBrowTt) REFRESH
  lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
  SHOW GET pbSSel,1 PROMPT lcPrompt

ENDIF
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrowse
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : Valid Browse Temp. sequence file fn.
*!*************************************************************
*! Calls     : 
*!             Procedures : None.
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvBrowse()
*!*************************************************************
*!
FUNCTION lfvBrowse

IF (ALLTRIM(WONTOP()) # ALLTRIM((lcABrowTt))) .OR.  (ALLTRIM(WONTOP()) # ALLTRIM((lcSBrowTt)))
  glFromBrow = .T.
  = gfStopBrow()
ENDIF
*-- end of lfvBrowse.

*!*************************************************************
*! Name      : lpEnter
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Function to trap enter key ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lpEnter()
*!*************************************************************
*!
FUNCTION lpEnter

IF (ALLTRIM(WONTOP()) = ALLTRIM((lcABrowTt)))
  =lfvSel('1')
ELSE
  SELECT (lcStorTmp)
  REPLACE cSelect WITH IIF(cSelect="»",' ',"»")
  lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
  SHOW GET pbSSel,1 PROMPT lcPrompt
ENDIF

*!*************************************************************
*! Name      : lfvSel
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Valid Function for select button ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSel()
*!*************************************************************
*!
FUNCTION lfvSel
PARAMETER lcScrNo

IF lcScrNo = '1'
  SELECT (lcCustTmp)
  IF cSelect = "»"
    REPLACE cSelect WITH " "
    lcPrompt = "S\<elect"
    SELECT (lcStorTmp)
    =SEEK(&lcCustTmp..Account)
    REPLACE REST cSelect WITH " " WHILE Account = &lcCustTmp..Account
    SHOW GET pbSSel,1 PROMPT lcPrompt
    SHOW GET pbSel,1 PROMPT lcPrompt
  ELSE
    REPLACE cSelect WITH "»"
    lcPrompt = "\<Unselect"
    SELECT (lcStorTmp)
    =SEEK(&lcCustTmp..Account)
    REPLACE REST cSelect WITH "»" WHILE Account = &lcCustTmp..Account
    SHOW GET pbSSel,1 PROMPT lcPrompt  
    SHOW GET pbSel,1 PROMPT lcPrompt
  ENDIF
ELSE
  SELECT (lcStorTmp)
  REPLACE cSelect WITH IIF(cSelect="»",' ',"»")
  lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
  SHOW GET pbSSel,1 PROMPT lcPrompt
ENDIF

IF lcScrNo = '1'
  SHOW WINDOW (lcSBrowTt) REFRESH
  SHOW WINDOW (lcABrowTt) REFRESH
ELSE
  SHOW WINDOW (lcSBrowTt) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfvAll
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Valid Function for select all button ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvAll()
*!*************************************************************
*!
FUNCTION lfvAll
PARAMETER lcScrNo

IF lcScrNo = '1'
  SELECT (lcCustTmp)
ELSE
  SELECT (lcStorTmp)
ENDIF

REPLACE ALL cSelect WITH "»"
lcPrompt = "\<Unselect"
SHOW GET pbSel,1 PROMPT lcPrompt
GO TOP
IF lcScrNo = '1'
  SHOW GET pbSel,1 PROMPT lcPrompt
  SHOW WINDOW (lcABrowTt) REFRESH
ELSE
  SHOW GET pbSSel,1 PROMPT lcPrompt
  SHOW WINDOW (lcSBrowTt) REFRESH
ENDIF


*!*************************************************************
*! Name      : lfvNon
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Valid Function for select non button ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNon()
*!*************************************************************
*!
FUNCTION lfvNon
PARAMETER lcScrNo

IF lcScrNo = '1'
  SELECT (lcCustTmp)
ELSE
  SELECT (lcStorTmp)
ENDIF

REPLACE ALL cSelect WITH " "
lcPrompt = "S\<elect"
GO TOP
IF lcScrNo = '1'
  SHOW GET pbSel,1 PROMPT lcPrompt
  SHOW WINDOW (lcABrowTt) REFRESH
ELSE
  SHOW GET pbSSel,1 PROMPT lcPrompt
  SHOW WINDOW (lcSBrowTt) REFRESH
ENDIF

*!*************************************************************
*! Name      : lfvInv
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Valid Function for invert button ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvInv()
*!*************************************************************
*!
FUNCTION lfvInv
PARAMETER lcScrNo

IF lcScrNo = '1'
  SELECT (lcCustTmp)
ELSE
  SELECT (lcStorTmp)
ENDIF

lnNewRec = RECNO()
REPLACE ALL cSelect WITH IIF(cSelect="»",' ',"»")
GO TOP
IF !EOF()
  GO lnNewRec
ENDIF
lcPrompt = IIF(cSelect="»","\<Unselect","S\<elect")
IF lcScrNo = '1'
  SHOW GET pbSel,1 PROMPT lcPrompt
  SHOW WINDOW (lcABrowTt) REFRESH
ELSE
  SHOW GET pbSSel,1 PROMPT lcPrompt
  SHOW WINDOW (lcSBrowTt) REFRESH
ENDIF


*!*************************************************************
*! Name      : lfvNvigatr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Navigator function ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNvigatr()
*!*************************************************************
*!
FUNCTION lfvNvigatr
PARAMETER lcAction

=lfUpdCust()
lcNextActn = lcAction

*!*************************************************************
*! Name      : lfvNvigatr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : Navigator function ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvNvigatr()
*!*************************************************************
*!
FUNCTION lfvNext
=lfvNvigatr("N")

*!*************************************************************
*! Name      : lfvPre
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : previous button ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvPre()
*!*************************************************************
*!
FUNCTION lfvPre

=lfvNvigatr("B")

*!*************************************************************
*! Name      : lfvResu
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : resume later button ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvResu()
*!*************************************************************
*!
FUNCTION lfvResu

lcNextActn = "U"
=lfUpdCust()
CLEAR READ

*!*************************************************************
*! Name      : lfUpdCust
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : function to update customer ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfUpdCust()
*!*************************************************************
*!
FUNCTION lfUpdCust

SELECT (lcCustTmp)
SET FILTER TO
GO TOP
SCAN
  lcKey = "M"+ACCOUNT
  IF SEEK(lcKey,'SYWCUST')
    SELECT SYWCUST
    REPLACE lSelect WITH IIF(&lcCustTmp..cSelect="»",.T.,.F.)
  ENDIF
ENDSCAN
SELECT (lcStorTmp)
SET FILTER TO
GO TOP
SCAN
  lcKey = "S"+ACCOUNT+STORE
  IF SEEK(lcKey,'SYWCUST') .AND. SEEK(ACCOUNT,lcCustTmp) .AND. &lcCustTmp..cSelect="»"
    SELECT SYWCUST
    REPLACE lSelect WITH IIF(&lcStorTmp..cSelect="»",.T.,.F.)
  ELSE
    SELECT SYWCUST
    REPLACE lSelect WITH .F.
  ENDIF
ENDSCAN
*!*************************************************************
*! Name      : lfActsBrow
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/07/99
*! Purpose   : Activate the browse.
*!*************************************************************
*! Passed Parameters  :  None.
*!*************************************************************
*! Returns            :  None.
*!*************************************************************
*! Example            : =lfActsBrow()
*!*************************************************************
FUNCTION lfActsSBrow

SELECT &lcStorTmp
GO TOP

*--- Account , Btname , cAddress3 , cAddress4 , cAddress5 , Phone1 , Buyer , SalesRep

lcBrfield = [cMark     :02 :R :H = ''             ,] +;
             [cSelect   :02 :R :H = ''             ,] +;
             [Store     :15 :R :H = 'Store'        ,] +;
             [Account   :15 :R :H = 'Customer'     ,] +;
             [Btname    :20 :R :H = 'Customer name',] +;
             [cAddress3 :20 :R :H = 'City'         ,] +;
             [cAddress4 :20 :R :H = 'Province'     ,] +;
             [cAddress5 :20 :R :H = 'Postal c'     ,] +;
             [Phone1    :16 :R :H = 'Phone#'       ,] +;
             [Buyer     :20 :R :H = 'Buyer'        ,] +;
             [SalesRep  :04 :R :H = 'SalesRep'      ]


BROWSE FIELDS &lcBrField;
       WHEN lfwBrows('2');
       VALID :F lfvBrowse() ;
       WINDOW SMWCUSTC IN WINDOW (gcBaseWind);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE ALLTRIM(lcSBrowTt)

*!*************************************************************
*! Name      : lfWheScr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 01/07/99
*! Purpose   : UnTrap function for Browse
*!           : When function for screen ....
*!*************************************************************
*! Calls     : 
*!             Procedures : .....
*!             Functions  : gfStopBrow
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfWheScr()
*!*************************************************************
*!
FUNCTION lfWheScr

KEYBOARD"{ALT+B}"