*:************************************************************************
*: Program file  : SOBLKSPL.PRG                  (037186,1)
*: Program desc. : Order Split.
*: For screen    : SOBLKSPL.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Sales Order (SO).
*:      Developer: MHM - Mohamed Shokry
*:           Date: 12/02/2003
*:************************************************************************
*: Calls : 
*:         Functions  : gfTempName, gfSetup,gfOpenFile ,gfModalGen
*:                    : gfBrows 
*:*************************************************************
*: Passed Parameters  : .... 
*:*************************************************************
*: Modifications      :
*:****************************************************************

*--Variable Declaration
*--lcRpTmp      --------------------> Variable to get temp. file
*--lcBrTrttl    --------------------> Variable to get Tital
*--lnBrRecNo    --------------------> Variable to Get record Number
*-- cAccount    --------------------> Variable to Get Account Number

STORE '' TO lcBrTrttl ,cAccount,lcAccName
STORE 0  TO lnBrRecNo
lcOrdTmp = gfTempName()

lcBrTrttl  ='Split Multi Store Order'
lcClsBmp    = gcBmpHome + "CLS.bmp"

*-- for in Range Screen
llBrowse = .F.
llSelected = .F.
IF !gfSetup()
  RETURN
ENDIF

*--Open needed files
DO lpOpnFil
*--Create Temp File
DO lfCrtTmp

PUSH KEY
ON KEY

SELECT (lcOrdTmp)
DO (gcScrDir + gcWinAppl + '\Soblkspl.SPR')

POP KEY

*!*************************************************************
*! Name      : lfVBrw
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : Browse Orders Record
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfVBrw()
*!*************************************************************

FUNCTION lfVBrw

PRIVATE lnCurAlias

lnCurAlias =SELECT(0)

SELECT (lcOrdTmp)
GOTO TOP

BROWSE FIELDS ;
       cFlag  :H=' '         :R:1:W=.F.,;
       Order  :H = 'Order#'  :R ,;
       lcStatus=IIF(Status='O','Open','Hold') :H = 'Status' :R ,;
       lcSesDesc=gfCodDes(SEASON,'SEASON'):H="Season" :31,;
       lcdivision=gfCodDes(CDIVISION,'CDIVISION'):H="Division" :31,;
       lcShipvia=gfCodDes(SHIPVIA,'SHIPVIA'):H="Shipvia" :31,;
       lctermcode=gfCodDes(CTERMCODE,'CTERMCODE'):H="Termcode" :31;
       WINDOW SoBlkSp2     ;
       WHEN lfwTrnBrow()   ;
       IN WINDOW 'Soblkspl';
       LOCK 0              ;
       NOAPPEND            ;
       NOEDIT              ;
       NOCLEAR             ;
       NODELETE            ;
       NOMENU              ;    
       SAVE                ;
       TITLE (lcBrTrttl)   ;
       NOWAIT

ON KEY LABEL TAB        DO lfTrBrTab
SELECT (lnCurAlias)               

*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : (MHM) Mohamed Shokry
*! Date      : 11/30/2003
*! Purpose   : Create Temp. File 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lfCrtTmp
*!*************************************************************
PROCEDURE lfCrtTmp
IF USED(lcOrdTmp)
  SELECT (lcOrdTmp)
  ZAP
ELSE
  CREATE CURSOR (lcOrdTmp);
   ( Order C(6) , cFlag C(1), cordtype C(1), Status C(1), Season C(6) ,;
    cDivision C(6) , Shipvia C(6) , CTermcode C(6), CNewOrder C(6))

  ZAP  
  INDEX ON cordtype + Order TAG &lcOrdTmp
  
ENDIF
*!*************************************************************
*! Name      : lpOpnFil
*! Developer : (MHM) Mohamed Shokry
*! Date      : 11/30/2003
*! Purpose   : Open needed Files
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpOpnFil
*!*************************************************************
PROCEDURE lpOpnFil

*- Abdou Open Order file with Store index.
*=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'Ordlinst','SH')
*- Abdou

=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDACCT','SH')
=gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')

*!*************************************************************
*! Name      : lfvColect
*! Developer : (MHM) Mohamed Shokry
*! Date      : 11/30/2003
*! Purpose   : Collect data for date
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvColect()
*!*************************************************************
FUNCTION lfvColect
PRIVATE lnAlias , lcStyle
lnAlias = SELECT()

IF EMPTY(cAccount) 
  WAIT WINDOW "Choose Account" NOWAIT
  RETURN
ENDIF
IF USED(lcOrdTmp)
  SELECT (lcOrdTmp)
  *--Zap temp file to empty and collect data 
  ZAP
ENDIF
SELECT OrdHdr

*--Orders with status active
*-- have no open or hold purchase order
*-- no open or hold orders ,Have no stock 
=SEEK(cAccount)
SCAN REST WHILE Account = cAccount
  WAIT WINDOW 'Collecting data for Account : '+Account NOWAIT
  *--checking all data to be correct " multi store and come from Bulk
  IF MULTI = "N"
  	LOOP
  ENDIF
  *--if not coming from Bulk Order then loop
  *ash1
  *IF EMPTY(cFromOrder)
  *	LOOP
  *ENDIF
  IF !(STATUS $ "OH")
  	LOOP
  ENDIF
  SCATTER MEMVAR MEMO
  INSERT INTO (lcOrdTmp) FROM MEMVAR
ENDSCAN
WAIT CLEAR

=lfVBrw()
*-- Enabled select in case of there are data
SELECT (lcOrdTmp)
IF !EOF()
  SHOW GET pbSelect ENABLED
  SHOW GET pbSplet ENABLED
ENDIF  
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwTrnBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : Browse the records
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfTrnBrow()
*!*************************************************************

FUNCTION lfwTrnBrow

lnCurAlias = SELECT(0)
SELECT (lcOrdTmp)
lnBrTrNo = RECNO()

SELECT (lnCurAlias)

SHOW WINDOW Soblksp2 REFRESH SAME

lcSelect = IIF(CFLAG ='»','Un\<select','\<Select')
SHOW GET pbSelect,1 PROMPT lcSelect


*!*************************************************************
*! Name      : lfvSplit
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : Validate Split
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSplit()
*!*************************************************************
*!
FUNCTION lfvSplit
lnAlias = SELECT()

SELECT (lcOrdTmp)
llCheck = .F.

SCAN
  IF CFLAG ='»'
    llCheck = .T.
    EXIT
  ENDIF
ENDSCAN
IF !llCheck
  WAIT WINDOW "No record selected" NOWAIT
  RETURN
ENDIF

lnChoice =gfModalGen('QRM00000B42002','F',' ',' ','Are you sure you want to Split these Order now?')

*- Abdou
lcOldStore = ""
*- Abdou

IF lnChoice = 1 
  SELECT ORDHDR
  =SEEK(cAccount+&lcOrdTmp..cOrdType + &lcOrdTmp..Order ,"ORDHDR")
  SCATTER MEMVAR MEMO
  SELECT OrdLine
  =SEEK(&lcOrdTmp..cOrdType + &lcOrdTmp..Order )
  
  IF EOF()
    RETURN
  ENDIF
  
  SCAN FOR  cOrdType + Order = &lcOrdTmp..cOrdType + &lcOrdTmp..Order
    *- Abdou
     llAddNew = .F.
    *m.Order     =  gfSequence('ORDER','','',m.cDivision)    
    IF !(lcOldStore = STORE)
      m.Order = gfSequence('ORDER','','',m.cDivision)
      lcOldStore = STORE
      llAddNew = .T.
    ENDIF
    *- Abdou

    m.Store     = Ordline.Store
    m.Multi     = "N"
    m.Book      = OrdLine.TotBook
    m.BookAmt   = OrdLine.TotBook*OrdLine.Price
    m.Open      = OrdLine.TotQty
    m.OpenAmt   = OrdLine.TotQty * OrdLine.Price
    *- Abdou
    *INSERT INTO ORDHDR FROM MEMVAR 
    *REPLACE ORDLINE.ORDER   WITH m.Order,;
    * 		ORDLINE.lineno  WITH 1
    IF llAddNew
      *ash1 as per tony in issue# 122971
      *M.Entered   = gdSysDate
      *M.Start     = gdSysDate
      *M.Complete  = gdSysDate + 10
      
      INSERT INTO ORDHDR FROM MEMVAR
      lnLineNo = 0
    *ash1
    ELSE
      REPLACE ORDHDR.Open    WITH ORDHDR.Open   + OrdLine.TotQty ,;
              ORDHDR.OpenAmt WITH ORDHDR.OpenAmt+ OrdLine.TotQty * OrdLine.Price,;
              ORDHDR.Book    WITH ORDHDR.Book   + OrdLine.TotBook ,;
              ORDHDR.BookAmt WITH ORDHDR.BookAmt+ OrdLine.TotBook*OrdLine.Price
    ENDIF
    lnLineNo = lnLineNo + 1
    *ash1 as per Tony in issue# 122971
    *REPLACE Ordline.Order     WITH m.Order   ,;
     		Ordline.lineno    WITH lnLineNo  ,;
            Ordline.Start     WITH gdSysDate ,;
            Ordline.Complete  WITH gdSysDate + 10
    REPLACE Ordline.Order     WITH m.Order   ,;
     		Ordline.lineno    WITH lnLineNo  
    *- Abdou
    
	*--some code to split order
    =SEEK(&lcOrdTmp..cOrdType + &lcOrdTmp..Order )
    IF EOF()
      GOTO BOTT
    ELSE
      SKIP-1
    ENDIF
    
  ENDSCAN
  SELECT ORDHDR
  =SEEK(cAccount+&lcOrdTmp..cOrdType + &lcOrdTmp..Order)
  REPLACE ORDHDR.STATUS   WITH "X"
  
  *- Abdou * Update the Order Header With cancel Qty.
  REPLACE Ordhdr.Cancel    WITH Ordhdr.book   ,;
          Ordhdr.CancelAmt WITH Ordhdr.bookAmt,;
          Ordhdr.Open      WITH 0            ,;
          Ordhdr.OpenAmt   WITH 0
          
          
  *- Abdou

  =gfModalGen('INM00000B40011','DIALOG','','','Order have been successfully Split.')
  *-- Refresh the screen.
  =lfvColect()
  lcSelect = '\<Select'
  SHOW GET pbSelect,1 PROMPT lcSelect
  
  *- Abdou
  IF USED(lcOrdTmp)
    SELECT (lcOrdTmp)
    ZAP
  ENDIF
  =lfVBrw()
  SHOW GET pbSelect DISABLE
  SHOW GET pbSplet  DISABLE
  STORE '' TO cAccount , lcAccName
  _CUROBJ = OBJNUM(cAccount)
  llSelected = .F.
  *- Abdou

  
ELSE
  RETURN
ENDIF  

*!*************************************************************
*! Name      : lfTrBrTab
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : Trap the Tab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrBrTab()
*!*************************************************************

FUNCTION lfTrBrTab
ON KEY LABEL TAB

IF WONTOP()  = lcBrTrttl
  ACTIVATE WINDOW SoBlkSp3
  _CUROBJ = OBJNUM(pbSelect)
ENDIF
IF EOF() 
  _CUROBJ = OBJNUM(pbAccBrow)
ELSE
  _CUROBJ = OBJNUM(pbSelect)
ENDIF

*!*************************************************************
*! Name      : lfTrBrBack
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrBrBack()
*!*************************************************************

FUNCTION lfTrBrBack
ON KEY LABEL BACKTAB
ACTIVATE WINDOW SoBlkSp1
_CUROBJ = OBJNUM(pbAccBrow)


*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : Validate Select 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvSelect()
*!*************************************************************

FUNCTION lfvSelect

lnCurAlias = SELECT(0)

SELECT (lcOrdTmp)
lcRecNo = RecNo()
IF EOF()
  RETURN
ENDIF
IF llSelected AND &lcOrdTmp..cFlag <> '»'
  =gfModalGen('INM00000B40011','DIALOG','','','you alrady selected Order , please unselect the order first.')
  RETURN
	
ENDIF

IF &lcOrdTmp..cFlag = '»'
  REPLACE &lcOrdTmp..cFlag WITH ''
  llSelected = .F.
ELSE
  REPLACE &lcOrdTmp..cFlag WITH '»'
  llSelected = .T.
ENDIF
GOTO lcRecNo
= lfwTrnBrow()

SELECT(lnCurAlias)
*!*************************************************************
*! Name      : lfTrap
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrnTrap()
*!*************************************************************

FUNCTION lfTrnTrap

*-- THIS is function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTrttl
  ON KEY LABEL TAB     DO lfTrBrTab
  ON KEY LABEL BACKTAB DO lfTrBrBack
  ON KEY LABEL ESC     DO lfTrBrEsc

  ON KEY LABEL SPACEBAR DO lfvSelRec

ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfClrTrnTrap
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : Clearing the previous trapping
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfClrTrnTrap()
*!*************************************************************

FUNCTION lfClrTrnTrap

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfTrBrBack
*! Developer : Mohamed Shokry (MHM)
*! Date      : 11/30/2003
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrBrBack()
*!*************************************************************

FUNCTION lfTrBrBack
ON KEY LABEL BACKTAB

ACTIVATE WINDOW SoblkSp2

_CUROBJ = OBJNUM(pbSelect)

*!*************************************************************
*! Name      : lfTrBrEsc
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
*! Purpose   : Trap the BackTab Key
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrBrEsc()
*!*************************************************************

FUNCTION lfTrBrEsc
ON KEY LABEL ESC
ACTIVATE WINDOW SoblkSp3
_CUROBJ = OBJNUM(pbClose)
KEYBOARD "{ENTER}" CLEAR

*!*************************************************************
*! Name      : lfClearKey
*! Developer : MHM (MOHAMED SHOKRY)
*! Date      : 29/03/2001
*! Purpose   : Clear Active Keys
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lfClearKey()
*!*************************************************************
*!
FUNCTION lfClearKey
ON KEY LABEL Alt+L
ON KEY LABEL Alt+A
ON KEY LABEL Alt+N
ON KEY LABEL Alt+I
ON KEY LABEL Alt+R
ON KEY LABEL Alt+O
ON KEY LABEL Alt+S
ON KEY LABEL Alt+C
ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+B
*-- end of lfClearKey.

*!*************************************************************
*! Name      : lfvSelRec
*! Developer : Abdou Elgendy (ABD)
*! Date      : 09/26/2001
*! Purpose   : Select Active Recored.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvSelRec()
*!*************************************************************
FUNCTION lfvSelRec
PRIVATE lnAlias


lnAlias = SELECT (0)
IF LastKey() =  32
  SELECT (lcOrdTmp)
  
  IF EOF()
    RETURN
  ELSE
    lcRecNo = RecNo()  
  ENDIF
  IF &lcOrdTmp..cFlag = '»'
    REPLACE &lcOrdTmp..cFlag WITH ''
  ELSE
    REPLACE &lcOrdTmp..cFlag WITH '»'
  ENDIF
  
  SKIP
  IF EOF()
    SKIP-1
  ENDIF
  = lfwTrnBrow()
  SHOW WINDOW SoblkSp2 REFRESH SAME
ENDIF
SELECT (lnAlias)

*-- End OF lfvSelRec.

*!*************************************************************
*! Name      : lfvData_1
*! Developer : Ahmed Amer (AHM)
*! Date      : 08/28/97
*! Purpose   : Validat the screen key (Account).
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvData_1()
*!*************************************************************

FUNCTION lfvData_1

PRIVATE lnCurAlias

lnCurAlias = SELECT(0)
XACCOUNT = cAccount
IF llBrowse OR (!EMPTY(cAccount) AND !SEEK('M'+XACCOUNT,'Customer'))
  IF llBrowse 
  	XACCOUNT = "?"
  ENDIF
  llBrowse = .F.
  DO CUSBROWM WITH XACCOUNT
  cAccount = XACCOUNT
  IF EMPTY(cAccount)
    _CUROBJ = _CUROBJ
  ENDIF  
ENDIF  
lcAccName = IIF(SEEK("M"+cAccount,'Customer'),Customer.btName," ")
SHOW GETS WINDOW Soblksp1
=lfvColect()


SELECT (lnCurAlias)

