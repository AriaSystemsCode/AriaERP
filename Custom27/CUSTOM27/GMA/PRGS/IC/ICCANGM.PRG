*:************************************************************************
*: Program file  : ICCANGM.PRG                  (102214,1)
*: Program desc. : Cancel Styles.
*: For screen    : ICCANGMA.SPR
*:         System: ARIA APPAREL SYSTEM 2.7
*:         Module: Inventory Control (IC).
*:      Developer: MHM - Mohamed Shokry
*:************************************************************************
*: Calls : 
*:         Functions  : gfTempName, gfSetup,gfOpenFile ,gfModalGen
*:                    : gfBrows 
*:*************************************************************
*: Passed Parameters  : .... 
*:*************************************************************
*: Modifications      :
*:C102312,1 05/22/2001 MHM Add  PO Comp. Invoice Date Style Date  to  filter criteria   
*:B604550,1 06/22/2001 WAB fix the bug of filter Add_date not work and diplay "*' on the left of 
*:B604550,1                any style have space on the left of the style code.
*:C102440,1 ABD 09/25/2001 Let the Program Delete Canceled styles , & delete this 
*:C102440,1 ABD            Styles from EDI & UPC.
*:B605121,1  11/25/2001 Delete the style upc from the EDI catalog if found.
*:B605392,1 BWA 02/03/2002 1) Modify the fields of the style file with the user , time and date.
*:B605392,1                2) Add a new message to let the user know that the transaction is finished.
*:B605924,1 ASH 05/20/2002 Fix the bug of not creating 832 EDI transaction for the deleted UPC's with status "Discontinued". (This fix done per Walid instructions)
*:****************************************************************

*--Variable Declaration
*--lcRpTmp      --------------------> Variable to get temp. file
*--lcBrTrttl    --------------------> Variable to get Tital
*--lcRpName     --------------------> Variable to Get Report Name
*--lnBrRecNo    --------------------> Variable to Get record Number
*:C102312,1 05/22/2001 MHM [start]
*--ldvDat       --------------------> Variable to save Date 
*--ldvPODat     --------------------> Variable to save PO Comp. Date 
*--ldvInvDat    --------------------> Variable to save Invoice Date 
*--ldvStyDat    --------------------> Variable to save Style Date 
*:C102312,1 05/22/2001 MHM [end]

STORE '' TO lcBrTrttl
STORE 0  TO lnBrRecNo

*:C102312,1 05/22/2001 MHM [start]
*ldvDat = {}
ldvPoDat = {}
ldvInvDat = {}
ldvStyDat = {}
*:C102312,1 05/22/2001 MHM [End]

lcBrTrttl  ='Style Cancelation'
lcRpName = "ICCANGMA.FRX"
lcCanTmp = gfTempName()


*--Control Show Of Browse and Print
laCtrStat[10] = "DISABLED"
laCtrStat[6]  = "ENABLED"

*-- for in Range Screen
lcStyBrow = 'Styles'
llBrowse = .F.

*C102440,1 ABD Ask the user if to Cancel styles or to Delete Canceled styles. [Begin]
*--
lnResp = gfModalGen('QRM00000B42020','DIALOG',.F.,.F.,;
                    'Do you want to Cancel styles with no activities OR ' +;
                    'to Delete cancelled styles/UPC form the EDI catalog?')
llCanSty = (lnResp=1) && Variable used to Hide screen contains dates fields
                      && in case of delete canceled styles 
lcBrTrttl  = IIF(llCanSty,'Style Cancelation','Style Deletion')

lcCanWnd = IIF(llCanSty,'Iccangm2','Iccangm4')

DO CASE
  CASE lnResp = 1
  *--Cancel styles
  *C102440,1 ABD [End]
  
  DIMENSION laPanelObj(1,3)    && Array to add new buttons to the control panel
  laPanelObj[1,1] = 'pbUpdate'
  laPanelObj[1,2] = gcBmpHome + 'Release2.BMP'
  laPanelObj[1,3] = 'VALID lfvUpdate()'

  *C102440,1 ABD The case of Delete Canceled styles. [Begin]
  *--Delete styles.
  CASE lnResp = 2

  *-- Create Table to Hold the rejected styles. [Begin]
  lcRejectmp = gfTempName()

  *--Delete canceled styles  
  laCtrStat[8]  =  "ENABLE"   && Enable Delete button 
  STORE {} TO ldPoDate , ldInvDat , ldStyDat

    
  llPackUse  = gfGetMemVar('M_pack')     ='Y'
  lcIType1   = gfGetMemVar('M_cIType1')
  lcIType2   = gfGetMemVar('M_cIType2')
  lcIType3   = gfGetMemVar('M_cIType3')
  lcIType4   = gfGetMemVar('M_cIType4')
  lcIType5   = gfGetMemVar('M_cIType5')      
  lcMType1   = gfGetMemVar('M_cMType1')
  lcMType2   = gfGetMemVar('M_cMType2')
  lcMType3   = gfGetMemVar('M_cMType3')
  lcMType4   = gfGetMemVar('M_cMType4')
  lcMType5   = gfGetMemVar('M_cMType5')
  
  llEDIInstl = (OCCURS('UP',gcCmpModules)<>0)
  llUpc      = gfGetMemVar('M_UPC_USE')  ='Y'
  llUpc      = ( llUpc AND llEDIInstl )
  llLink_Gl  = gfGetMemVar('M_Link_GL')  ='Y'
  llMultiwh  = gfGetMemVar('M_WareHouse')='Y'
    
ENDCASE
*C102440,1 ABD [End]

IF !gfSetup()
  RETURN
ENDIF

*--Open needed files
DO lpOpnFil
*--Create Temp File
DO lfCrtTmp

*C102440,1 ABD Save the key label before calling the screen. [Begin]
PUSH KEY
ON KEY
*C102440,1 ABD [End]

SELECT STYLE
DO (gcScrDir + gcWinAppl + '\ICCANGMA.SPR')

*C102440,1 ABD Return the status for key label to the status before call
*C102440,1 ABD The screen.  [Begin]
POP KEY
*C102440,1 ABD [End]

*!*************************************************************
*! Name      : lfVBrw
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/28/2001
*! Purpose   : Browse the Style records
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
*--MHM2000
*lcBrRpCom  ="cFlag      :1   :R :H=' ',"+;
            "cStyle     :19  :R :H='Style',"+;
            "cDesc      :30  :R :H='Description ',"+;
            "cOrder     :8   :R :H='Order',"+;
            "dOrdDate   :10  :R :H='Order Date',"+;
            "cInvoic    :8   :R :H='Invoic',"+;
            "dInvDat    :10  :R :H='Invoice Date',"+;
            "cPo        :8   :R :H='PO',"+;
            "dPoDate    :10  :R :H='PO Date'"
*B604550,1 WAB (START) add field ADD_DATE to the browser screen
*lcBrRpCom  ="cFlag      :1   :R :H=' ',"+;
            "cStyle     :19  :R :H='Style',"+;
            "cDesc      :30  :R :H='Description ',"+;
            "cInvoic    :8   :R :H='Invoic',"+;
            "dInvDat    :10  :R :H='Invoice Date',"+;
            "cPo        :8   :R :H='PO',"+;
            "dPoDate    :10  :R :H='PO Date'"

lcBrRpCom  ="cFlag      :1   :R :H=' ',"+; 
            "cStyle     :19  :R :H='Style',"+;
            "cDesc      :30  :R :H='Description ',"+;
            "dAdd_Date  :10  :R :H='Add Date',"+;
            "cInvoic    :8   :R :H='Invoic',"+;
            "dInvDat    :10  :R :H='Invoice Date',"+;
            "cPo        :8   :R :H='PO',"+;
            "dPoDate    :10  :R :H='PO Date'"

*B604550,1 WAB (END) 
*--MHM2000
SELECT (lcCanTmp)
GOTO TOP

*C102440,1 ABD Show the cancel browse or Delete browse. [Begin]
*BROWSE FIELDS &lcBrRpCom   ;
       WINDOW 'Iccangm2'   ;
       WHEN lfwTrnBrow()   ;
       IN WINDOW 'Iccangma';
       LOCK 0              ;
       NOAPPEND            ;
       NOEDIT              ;
       NOCLEAR             ;
       NODELETE            ;
       NOMENU              ;    
       SAVE                ;
       TITLE (lcBrTrttl)   ;
       NOWAIT

*B605392,1 BWA 02/03/2002 Browse the fields for the styles that not canceled.[START]
*BROWSE FIELDS &lcBrRpCom   ;
*       WINDOW &lcCanWnd    ;
*       WHEN lfwTrnBrow()   ;
*       IN WINDOW 'Iccangma';
*       LOCK 0              ;
*       NOAPPEND            ;
*       NOEDIT              ;
*       NOCLEAR             ;
*       NODELETE            ;
*       NOMENU              ;    
*       SAVE                ;
*       TITLE (lcBrTrttl)   ;
*       NOWAIT

BROWSE FIELDS &lcBrRpCom   ;
       FOR !(Ldelete)      ;
       WINDOW &lcCanWnd    ;
       WHEN lfwTrnBrow()   ;
       IN WINDOW 'Iccangma';
       LOCK 0              ;
       NOAPPEND            ;
       NOEDIT              ;
       NOCLEAR             ;
       NODELETE            ;
       NOMENU              ;    
       SAVE                ;
       TITLE (lcBrTrttl)   ;
       NOWAIT
*B605392,1 BWA 02/03/2002.[END]

*C102440,1 ABD [End]

ON KEY LABEL TAB        DO lfTrBrTab
SELECT (lnCurAlias)               

*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : (MHM) Mohamed Shokry
*! Date      : 03/27/2001
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
IF USED(lcCanTmp)
  SELECT (lcCanTmp)
  ZAP
ELSE
  *B604550,1 WAB (START) add field add_date to the temp file
  *CREATE CURSOR (lcCanTmp);
   ( cStyle C(19) ,cDesc C(30) , cOrder C(6)  , dOrdDate D,cInvoic C(6) , dInvDat D ,;
    cPo C(6) , dPoDate D  , cFlag C(1))
  *C102440,1 ABD Add New Fields (make , Major) to the Cursor to use when Delete the style. [Begin]
  *CREATE CURSOR (lcCanTmp);
   ( cStyle C(19) ,cDesc C(30) , cOrder C(6)  , dOrdDate D,cInvoic C(6) , dInvDat D ,;
    cPo C(6) , dPoDate D  , cFlag C(1) ,dAdd_Date D)

  *B605392,1 BWA 02/03/2002 Add a new field "Ldelete" to use it in delete the style from the screen.[START]
  *CREATE CURSOR (lcCanTmp);
  *  ( cStyle C(19) ,cDesc C(30) , cOrder C(6)  , dOrdDate D,cInvoic C(6) , dInvDat D ,;
  *   cPo C(6) , dPoDate D  , cFlag C(1) ,dAdd_Date D, make L, CSTYMAJOR C (19))

  CREATE CURSOR (lcCanTmp);
   ( cStyle C(19) , cDesc C(30) , cOrder C(6)  , dOrdDate D  , cInvoic C(6) , dInvDat D        ,;
    cPo C(6)      , dPoDate D   , cFlag C(1)   , dAdd_Date D , make L       , CSTYMAJOR C (19) ,;
    Ldelete L)
  *B605392,1 BWA 02/03/2002.[END]

  *C102440,1 ABD [End]
  
  *B604550,1 WAB (END) 
  ZAP  
  INDEX ON cStyle TAG &lcCanTmp


  *C102440,1 ABD Create new table in case we will delete style , this file will hold the 
  *C102440,1 ABD Rejected style and reasoun. [Begin]
  IF !llCanSty
    CREATE Table (lcRejectmp)  ( Style C(19) ,Comments C(75) )
    INDEX ON Style TAG &lcRejectmp
  ENDIF  
  *C102440,1 ABD [End]
  
ENDIF
*!*************************************************************
*! Name      : lpOpnFil
*! Developer : (MHM) Mohamed Shokry
*! Date      : 03/27/2001
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

=gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH')
*--C102214,4 MHM 04/05/2001 [start]
=gfOpenFile(gcDataDir+'Stydye',gcDataDir+'Stydye','SH')
*--C102214,4 MHM 04/05/2001 [End]
=gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
=gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLNS','SH')
=gfOpenFile(gcDataDir+'INVLINE',gcDataDir+'INVLINES','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINE','SH')
=gfOpenFile(gcDataDir+'ORDLINE',gcDataDir+'ORDLINES','SH')
=gfOpenFile(gcDataDir+'ORDHDR',gcDataDir+'ORDHDR','SH')


*!*************************************************************
*! Name      : lfvColect
*! Developer : (MHM) Mohamed Shokry
*! Date      : 03/27/2001
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

*:C102312,1 05/22/2001 MHM [start]
*IF EMPTY(ldDate)
IF EMPTY(ldPoDate) OR EMPTY(ldInvDat) OR EMPTY(ldStyDat)
*:C102312,1 05/22/2001 MHM [End]
  WAIT WINDOW "Enter a date" NOWAIT
  RETURN
ENDIF
IF (ldPoDate = ldvPoDat) AND (ldInvDat = ldvInvDat) AND (ldStyDat = ldvStyDat)
  RETURN
ENDIF
IF USED(lcCanTmp)
  SELECT (lcCanTmp)
  *--Zap temp file to empty and collect data 
  ZAP
ENDIF
SELECT STYLE

*--Styles with status active
*-- have no open or hold purchase order
*-- no open or hold orders ,Have no stock 

SCAN FOR (STATUS = 'A') AND (TOTORD = 0) AND (TOTSTK = 0) AND (TOTWIP = 0)
  *WAIT WINDOW 'Collecting data, please wait.....' NOWAIT
  WAIT WINDOW 'Collecting data for Style # : '+Style NOWAIT
  lcStyle = Style
  *--style start date must be before the date  entered by the user
  *--C102214,4 MHM 04/05/2001 [start]
  *IF !EMPTY(Start) AND Start < ldDate

  *:C102312,1 05/22/2001 MHM [start]
  *-to initialize variables
  m.cPo = ""
  m.dPoDate = {}
  m.cInvoic = ""
  m.dInvDat = {}
  *IF Start < ldDate
  *B604550,1 WAB (START) change the filter from start_date to dADD_date
  *IF Start < ldStyDat
  IF dAdd_Date < ldStyDat
  *B604550,1 WAB (END) 
  *:C102312,1 05/22/2001 MHM [End]
    *--C102214,4 MHM 04/05/2001 [End]
    SCATTER MEMVAR MEMO
    m.cStyle = Style
    m.cDesc  = Desc
  ELSE
    LOOP
  ENDIF
  *--C102214,4 MHM 04/05/2001 [start]
  *--Check for stydye file
  IF SEEK(lcStyle,'STYDYE')
    SELECT STYDYE
    llChckSty = .F.
    SCAN REST WHILE (lcStyle = STYLE) 
      IF (TOTORD <> 0) OR (TOTSTK <> 0) OR (TOTWIP <> 0)
        llChckSty = .T.
        EXIT
      ENDIF
    ENDSCAN
    IF llChckSty
      LOOP
    ENDIF
  ENDIF  
  *--C102214,4 MHM 04/05/2001 [End]
  *-- Have no recieved purchase order since the end of the date entered by the user
  IF SEEK(lcStyle,'POSLN')
    llPoRecvd = .F.
    SELECT POSLN
    SCAN REST WHILE (lcStyle = STYLE) 
      lnlstPo = PO
      =SEEK('P'+PO,'POSHDR')
      *:C102312,1 05/22/2001 MHM [start]
      *lnlPodate = POSHDR.Entered
      *IF (lnlPodate > ldDate) AND (TRANCD = '2')
      lnlPodate = POSHDR.Complete
      IF (lnlPodate > ldPoDate) AND (TRANCD = '2')
      *:C102312,1 05/22/2001 MHM [End]
        llPoRecvd = .T.
        EXIT
      ENDIF
    ENDSCAN
    IF llPoRecvd
      LOOP
    ENDIF
    m.cPo = lnlstPo
    m.dPoDate = lnlPodate
  ENDIF  

  *--Have no invoice since the end of the date  entered by the user
  *--search for invoice line
  IF SEEK(lcStyle,'INVLINE')
    llInvoic = .F.
    SELECT INVLINE
    SCAN REST WHILE (lcStyle = STYLE) 
      lnlstInv  = Invoice
      lnlinvdat = InvDate
       *:C102312,1 05/22/2001 MHM [start]
       *IF (InvDate > ldDate) 
       IF (InvDate > ldInvDat) 
       *:C102312,1 05/22/2001 MHM [End]
         llInvoic = .T.
         EXIT
       ENDIF
    ENDSCAN
    IF llInvoic 
      LOOP
    ENDIF
    m.cInvoic = lnlstInv
    m.dInvDat = lnlinvdat
  ENDIF  
  
  *-- No open or hold orders since the end of the date  entered by the user
  *--search for Order
  *:C102312,1 05/22/2001 MHM [Start]
  *IF SEEK(lcStyle,'ORDLINE')
  *  llOrdLn = .F.
  *  SELECT ORDLINE
  *  SCAN REST WHILE (lcStyle = STYLE) 
  *    lnlstOrd = Order
  *    =SEEK('O'+Order,'OrdHdr')
  *    lnlOrdDat = OrdHdr.ENTERED
  *     IF (lnlOrdDat > ldDate) 
  *       llOrdLn = .T.
  *       EXIT
  *     ENDIF
  *  ENDSCAN
  *  IF llOrdLn
  *    LOOP
  *  ENDIF
  *  m.cOrder = lnlstOrd
  *  m.dOrdDate = lnlOrdDat 
  *ENDIF  
 *:C102312,1 05/22/2001 MHM [End]
  *B604550,1 WAB (START) call functon to add "*" on the left of style field if optain space
  =lfChkFrSPC()
  *B604550,1 WAB (END) 
  INSERT INTO (lcCanTmp) FROM MEMVAR
ENDSCAN
WAIT CLEAR
*C102312,1 05/22/2001 MHM [Start]
*ldvDat = ldDate
ldvPoDat = ldPoDate
ldvInvDat = ldInvDat
ldvStyDat = ldStyDat
*C102312,1 05/22/2001 MHM [End]
=lfVBrw()
*-- Enabled select in case of there are data
SELECT (lcCanTmp)
IF !EOF()
  SHOW GET pbSelAll ENABLED
  SHOW GET pbSelect ENABLED
  SHOW GET pbSelnon ENABLED
  SHOW GET pbSelrng ENABLED
ENDIF  
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfwTrnBrow
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
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
SELECT (lcCanTmp)
lnBrTrNo = RECNO()

SELECT (lnCurAlias)

*C102440,1 ABD - Call the correct screen, call IcCanWnd2 in case 
*C102440,1 ABD - Cancel style & IcCanWnd4 in case delete style. [Begin]
*SHOW WINDOW IcCanGm2 REFRESH SAME
SHOW WINDOW &lcCanWnd REFRESH SAME
*C102440,1 ABD [End]

lcSelect = IIF(CFLAG ='»','Un\<select','\<Select')
SHOW GET pbSelect,1 PROMPT lcSelect


*!*************************************************************
*! Name      : lfvUpdate
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
*! Purpose   : Validate Update 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvUpdate()
*!*************************************************************
*!
FUNCTION lfvUpdate
lnAlias = SELECT()
*--C102214,4 MHM 04/05/2001 [start]
SELECT (lcCanTmp)
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
*--C102214,4 MHM 04/05/2001 [End]
lnChoice =gfModalGen('QRM00000B42002','F',' ',' ','Are you sure you want to cancel these styles now?')

IF lnChoice = 1 
  SELECT (lcCanTmp)
  LOCATE
  IF EOF()
    RETURN
  ENDIF
  SCAN
    *B604550,1 WAB (START) call function to remove "*" from the left of the style field
    *=SEEK(&lcCanTmp..cStyle,'STYLE')
    *IF CFLAG ='»'
    IF CFLAG ='»' .AND. SEEK(lfRmvAstrics(),'STYLE')
      *B604550,1 WAB (END) 
      *C102440,1 ABD - Fix bug that when delete the style , the status should be
      *C102440,1 ABD - 'X' Not 'C'. [Begin]
      *REPLACE Style.Status WITH 'C'
      REPLACE Style.Status WITH 'X'

      *B605392,1 BWA 02/03/2002 Update the fields in the style file with the data 
      *                         and rise the flage to delete the style from the brows.[START]
      REPLACE STYLE.COWNER    WITH 'ICCANGM' ,;
              STYLE.CLOK_USER WITH gcUser_ID ,;
              STYLE.DADD_DATE WITH gdSysDate ,;
              STYLE.CLOK_TIME WITH Time()

      REPLACE &lcCanTmp..Ldelete WITH .T.
      *B605392,1 BWA 02/03/2002.[END]

    *C102440,1 ABD - [End]
    ENDIF  
  ENDSCAN

  *B605392,1 BWA 02/03/2002 Add message to let the user know that the process is finished.[START]
  =gfModalGen('INM00000B40011','DIALOG','','','Style(s) have been successfully cancelled.')
  *-- Refresh the screen.
  =lfVBrw()
  SHOW WINDOW &lcCanWnd REFRESH SAME
  *B605392,1 BWA 02/03/2002.[END]

ELSE
  RETURN
ENDIF  

*!*************************************************************
*! Name      : lfTrBrTab
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
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

*C102440,1 ABD - Activate the select screen when push Tab. [Begin]
*ACTIVATE WINDOW IcCanGm2
IF WONTOP()  = lcBrTrttl
  ACTIVATE WINDOW IcCanGm3
  _CUROBJ = OBJNUM(pbSelect)
ENDIF
*C102440,1 ABD - [End]


*C102440,1 ABD - Dont select date field if we delete the style. [Begin]
*IF EOF() 
IF EOF() .And. llCanSty
*C102440,1 ABD - [End]
  _CUROBJ = OBJNUM(ldDate)
ELSE
  _CUROBJ = OBJNUM(pbSelect)
ENDIF

*!*************************************************************
*! Name      : lfTrBrBack
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
*! Example   : =lfTrBrBack()
*!*************************************************************

FUNCTION lfTrBrBack
ON KEY LABEL BACKTAB
ACTIVATE WINDOW IcCanGm1
_CUROBJ = OBJNUM(ldDate)

*!*************************************************************
*! Name      : gfCPPrint
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
*! Purpose   : Print Report for selected records
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =gfCPPrint()
*!*************************************************************
FUNCTION gfCPPrint
*--check for printed records
SELECT (lcCanTmp)
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

IF pSetup(.T.)
  lcCons = SET('CONS')
  SET CONS OFF
  DO (gcRepHome +  'ICREPORT.APP') WITH 'ICCANGMA', .T.
  SET CONS &lcCons
ENDIF

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
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

SELECT (lcCanTmp)
lcRecNo = RecNo()
IF EOF()
  RETURN
ENDIF
IF &lcCanTmp..cFlag = '»'
  REPLACE &lcCanTmp..cFlag WITH ''
ELSE
  REPLACE &lcCanTmp..cFlag WITH '»'
ENDIF
GOTO lcRecNo
= lfwTrnBrow()

SELECT(lnCurAlias)
*!*************************************************************
*! Name      : lfTrap
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
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

  *C102440,1 ABD - Let the user select any record by push space bar
  *C102440,1 ABD - This will let the user to select more easy.  [Begin]
  ON KEY LABEL SPACEBAR DO lfvSelRec
  *C102440,1 ABD - [End]

ENDIF
RETURN .F.

*!*************************************************************
*! Name      : lfClrTrnTrap
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
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
*! Name      : lfTrBrTab
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/27/2001
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
*-- *C102440,1 ABD : I found 2 function , I delete this one !!!!!
*FUNCTION lfTrBrTab
*ON KEY LABEL TAB
*ACTIVATE WINDOW &lcCanWnd
*_CUROBJ = OBJNUM(pbSelect)

*!*************************************************************
*! Name      : lfTrBrBack
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
*! Example   : =lfTrBrBack()
*!*************************************************************

FUNCTION lfTrBrBack
ON KEY LABEL BACKTAB

*C102440,1 ABD - Call the correct screen, call IcCanWnd2 in case 
*C102440,1 ABD - Cancel style & IcCanWnd4 in case delete style. [Begain]
*ACTIVATE WINDOW IcCanGm2
ACTIVATE WINDOW &lcCanWnd
*C102440,1 ABD - [End]

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
ACTIVATE WINDOW gwcContrl1
_CUROBJ = OBJNUM(pbCls)
KEYBOARD "{ENTER}" CLEAR

*!*************************************************************
*! Name      : lfvSelAllNo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/29/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : lcBtn
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvSelAllNo()
*!*************************************************************

FUNCTION lfvSelAllNo
PARAMETERS lcBtn
lcSelMark = ''
lnCurAlias = SELECT(0)

SELECT (lcCanTmp)
LOCATE
lcSelMark = IIF(lcBtn = "ALL",'»','')
IF lcBtn = "None"
  REPLACE ALL cFlag WITH lcSelMark
ELSE
  REPLACE ALL cFLag WITH lcSelMark 
ENDIF
GOTO TOP
=lfVBrw()

SELECT(lnCurAlias)

*!*************************************************************
*! Name      : lfvRange
*! Developer : MHM (MOHAMED SHOKRY)
*! Date      : 29/03/2001
*! Purpose   : To validate the Range button.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvRange()
*!*************************************************************
*!
FUNCTION lfvRange

=lfClearKey()
DO (gcScrDir+gcWinAppl+'\ICGETRNG.SPX')
*-- end of lfvRange.

*!*************************************************************
*! Name      : lfvSelAllR
*! Developer : MMH (MOHAMED SHOKRY)
*! Date      : 29/03/2001
*! Purpose   : To validate the Select All button in Range window.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvSelAllR()
*!*************************************************************
*!
FUNCTION lfvSelAllR

PRIVATE  lnAlias

IF EMPTY(lcFrom) AND EMPTY(lcTo)
  RETURN
ENDIF

lnAlias = SELECT(0)
SELECT (lcCanTmp)
IF !EMPTY(lcFrom)
  SEEK lcFrom
ELSE
  LOCATE
ENDIF
IF EMPTY(lcTo)
  lcScanExpr = "REST WHILE !EOF()"
ELSE  
  lcScanExpr = "REST WHILE BETWEEN(cStyle,lcFrom,lcTo) "
ENDIF  
SCAN &lcScanExpr
  REPLACE Cflag WITH '»'
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
SELECT (lnAlias)
CLEAR READ
=lfVBrw()
*-- end of lfvSelAllR.

*!*************************************************************
*! Name      : lfvSelNonR
*! Developer : MHM (MOHAMED SHOKRY)
*! Date      : 29/03/2001
*! Purpose   : To validate the Select None button in range window.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvSelNonR()
*!*************************************************************
*!
FUNCTION lfvSelNonR

PRIVATE  lnAlias

IF EMPTY(lcFrom) AND EMPTY(lcTo)
  RETURN
ENDIF

lnAlias = SELECT(0)
SELECT (lcCanTmp)
IF !EMPTY(lcFrom)
  SEEK lcFrom
ELSE
  LOCATE
ENDIF
IF EMPTY(lcTo)
  lcScanExpr = "REST WHILE !EOF() "
ELSE  
  lcScanExpr = "REST WHILE BETWEEN(cStyle,lcFrom,lcTo) "
ENDIF  
SCAN &lcScanExpr
  REPLACE cFlag WITH ' '  
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
SELECT (lnAlias)
CLEAR READ
=lfVBrw()
*-- end of lfvSelNonR.

*!*************************************************************
*! Name      : lfvInvertR
*! Developer : MMH (MOHAMED SHOKRY)
*! Date      : 29/03/2001
*! Purpose   : To validate the Invert button in range window.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfvInvertR()
*!*************************************************************
*!
FUNCTION lfvInvertR

PRIVATE lnAlias

IF EMPTY(lcFrom) AND EMPTY(lcTo)
  RETURN
ENDIF

lnAlias = SELECT(0)
SELECT (lcCanTmp)
IF !EMPTY(lcFrom)
  SEEK lcFrom
ELSE
  LOCATE
ENDIF
IF EMPTY(lcTo)
  lcScanExpr = "REST WHILE !EOF()"
ELSE  
  lcScanExpr = "REST WHILE BETWEEN(cStyle,lcFrom,lcTo)"
ENDIF  
SCAN &lcScanExpr
  REPLACE Cflag WITH IIF(EMPTY(cFlag),'»','')
ENDSCAN  
IF BETWEEN(lnBrRecNo,1,RECCOUNT())
  GO lnBrRecNo
ENDIF  
SELECT (lnAlias)
CLEAR READ
=lfVBrw()
*-- end of lfvInvertR.


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
*! Name      : lfvMajor
*! Developer : Mohamed Shokry (MMH)
*! Date      : 03/29/2001
*! Purpose   : Valid function to validate From/To fields.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : lcField (lcFrom or lcTo)
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
FUNCTION lfvMajor
PARAMETER lcField
PRIVATE lcVar , lcObj , laTemp
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK(lcObj , lcCanTmp))
  SELECT (lcCanTmp)
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "cStyle     :R :H= 'Style' , "   +;
               "cDesc      :R :H= 'Description' ,"    +;
               "cPo        :R :H= 'PO' ," +;
               "Corder     :R :H= 'Order' ,"   +;
               "cInvoic    :R :H= 'Invoice' "
  
  lcFile_Ttl = "Style ..."
  = gfBrows('','cStyle','laTemp')
    
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = ''
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
IF lcField = 'FROM'
  lcFrom = lcObj
ELSE
  lcTo = lcObj
ENDIF

*-- end of lfMajor.
*!*************************************************************
*! Name      : lfvCancel
*! Developer : MMH (MOHAMED SHOKRY)
*! Date      : 29/03/2001
*! Purpose   : To validate the Cancel button in Range window.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ...........
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*!
FUNCTION lfvCancel

CLEAR READ
=lfVBrw()


*!*************************************************************
*! Name      : lfChkFrSPC
*! Developer : WAB  - WAlid A. Wahab
*! Date      : 06/22/2001
*! Purpose   : add '*' on the left of the style field if optain space on the left
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ...........
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*!B604550,1 WAB
*!*************************************************************
FUNCTION lfChkFrSPC
PRIVATE lcStyle 
IF LEFT(m.cStyle,1) = ' '
  lnStLen = LEN(m.cStyle)
  FOR lnCount = 1 TO lnStLen
    IF SUBSTR(m.cStyle,lnCount,1) <> ' '
      EXIT
    ENDIF
  ENDFOR
  lcStyle = SPACE(0)
  FOR lnNcnt = 1 TO lnCount -1 
    lcStyle = lcStyle + '*'
  ENDFOR
  lcStyle = lcStyle + SUBSTR(m.cStyle,lnCount)
  m.cStyle = lcStyle
ENDIF

*!*************************************************************
*! Name      : lfRmvAstrics
*! Developer : WAB  - WAlid A. Wahab
*! Date      : 06/22/2001
*! Purpose   : remove any '*' from the left of the style field 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ...........
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*!B604550,1 WAB
*!*************************************************************
FUNCTION lfRmvAstrics
PRIVATE lcStyle 
lcStyle = &lcCanTmp..cStyle
IF LEFT(lcStyle,1) = '*'
  lnStLen = LEN(&lcCanTmp..cStyle)
  FOR lnCount = 1 TO lnStLen
    IF SUBSTR(&lcCanTmp..cStyle,lnCount,1) <> '*'
      EXIT
    ENDIF
  ENDFOR
  lcStyle = SPACE(0)
  FOR lnNcnt = 1 TO lnCount -1 
    lcStyle = lcStyle + ' '
  ENDFOR
  lcStyle = lcStyle + SUBSTR(&lcCanTmp..cStyle,lnCount)
ENDIF
RETURN lcStyle
*!*************************************************************
*! Name      : lfColDelst
*! Developer : Abdou Elgendy (ABD)
*! Date      : 09/25/2001
*! Purpose   : Collect style with status Cancel .
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfColDelst()
*!*************************************************************
*C102440,1 ABD .
FUNCTION lfColDelst
PRIVATE lnAlias 

IF !llCanSty
  lnAlias = SELECT()
  *-- Collect styles with Status Canncel.

  IF USED(lcCanTmp)
    SELECT (lcCanTmp)
    *--Zap temp file to empty and collect data 
    ZAP
  ENDIF
  SELECT STYLE

  *--Styles with status active
  *-- have no open or hold purchase order

  SCAN FOR (STATUS = 'X')
    WAIT WINDOW 'Collecting data for Style # : '+Style NOWAIT
    lcStyle = Style
    *-to initialize variables
    SCATTER MEMVAR MEMO
    m.cPo = ""
    m.dPoDate = {}
    m.cInvoic = ""
    m.dInvDat = {}
    m.cStyle = Style
    m.cDesc  = Desc

    *--Check for stydye file
    =lfChkFrSPC()
    INSERT INTO (lcCanTmp) FROM MEMVAR
  ENDSCAN
  ldvPoDat = ldPoDate
  ldvInvDat = ldInvDat
  ldvStyDat = ldStyDat

  *-- Enabled select in case of there are data
  SELECT (lcCanTmp)
  IF EOF()
    WAIT WINDOW 'No recored selected.'  NOWAIT
  ELSE
    SHOW GET pbSelAll ENABLED
    SHOW GET pbSelect ENABLED
    SHOW GET pbSelnon ENABLED
    SHOW GET pbSelrng ENABLED
  ENDIF  
  SELECT (lnAlias)
ENDIF

WAIT CLEAR

*-- End Of lfColDelst.
*!*************************************************************
*! Name      : gfCPDelete
*! Developer : Abdou Elgendy (ABD)
*! Date      : 09/26/2001
*! Purpose   : Delete selected styles.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =gfCPDelete()
*!*************************************************************
*C102440,1 ABD .
FUNCTION gfCPDelete
Private llCheck
*--check for Deletig  records


*--check for printed records
SELECT (lcCanTmp)
llCheck = .F.
SCAN
  IF CFLAG ='»'
    llCheck = .T.
    EXIT
  ENDIF
ENDSCAN
IF llCheck
  *- Message Text   :- Are you sure you want to delete XXX ?
  *- Message Number :- 42124
  *- button message :-  - Yes - No.
  *- button Number  :- 42002
  IF  gfModalGen("QRM42124B42002","DIALOG",'selected style(s)') = 1
    *-- Call Function to Delete style.
    = lfacptdel ()  
  ENDIF
ELSE
  *- Message Text   :- Nothing to delete.
  *- Message Number :- 42006.
  *- button message :- OK.
  *- button Number  :- 00000
  = gfModalGen("QRM42006B00000","DIALOG")
ENDIF
*-- End OF gfCPDelete.
*!*************************************************************
*! Name      : lfAcptDel
*! Developer : Abdou Elgendy (ABD)
*! Date      : 09/26/2001
*! Purpose   : Accept deletion of the style colors.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Returns            : ............
*!*************************************************************
*! Example   : =lf..()
*!*************************************************************
*C102440,1 ABD .
FUNCTION lfacptdel
Private lctype , llOpnShst , lcMajor , lccolor , lcSepart

STORE '' TO lcMajor , lccolor , lcSepart
STORE .F. TO llOpnShst , llOpnPcks , llOpnBom , llOpnUpc
STORE 0   TO lnstylewid
STORE .T. TO llDelCort  && Flage hold .F. in case we have rejected lines to print.

*-- Open need fiels if use Upc and EDI module instul.
IF llUpc
  = gfOpenFile(gcDataDir+'STYLEUPC','STYLEUPC','SH')
  = gfOpenFile(gcDataDir+'EDICATGH','Account','SH')
  = gfOpenFile(gcDataDir+'EDICATGD','Account','SH')
   *B605121,1 HIA 11/25/2001 TO DELETE STYLE EVEN IT HAS UPC(S) 11/27/2001 [BEGIN]
  = gfOpenFile(gcDataDir+'EDITRANS','TYPEKEY','SH')
  = gfOpenFile(gcDataDir+'EDIACPRT','ACCFACT','SH')
   *B605121,1 HIA 11/25/2001 TO DELETE STYLE EVEN IT HAS UPC(S) 11/27/2001 [END]
  = gfOpenFile(gcDataDir+'RCYLUPC','Style','SH')
ENDIF

lcIMjrPt   = gfItemMask('PI')
lcMjrPct   = gfItemMask('PM')
lnstylewid = LEN(lcMjrPct)
lnColorLn  = (19-lnstylewid-1)
lcSepart   =SUBSTR(lcIMjrPt,lnstylewid+1,1)

SELECT (lcRejectmp)
ZAP

*-- Open Needed file.
llOpnShst = !USED('ICSTYHST')
IF llOpnShst
  =gfOpenFile(gcDataDir+'ICSTYHST','Styhst','SH')
ENDIF

llOpnBom = !USED('BOM')
IF llOpnBom
  =gfOpenFile(gcDataDir+'BOM','Bomitem','SH')
ENDIF

llSTYPRICE = !USED('STYPRICE')
IF llSTYPRICE
    =gfOpenFile(gcDataDir+'STYPRICE','STYPRICE','SH')
ENDIF

SELECT (lcCanTmp)


SCAN FOR CFLAG ='»'
  WAIT WINDOW 'Deleting Data For Style ..' NOWAIT
  *-- Check before delete the style.
  *A) Check For total Order before deleting.
  =SEEK(cStyle,'STYLE')
  IF style.totord <> 0
    *-- Flage hold .F. in case we have rejected lines to print.
    llDelCort = IIF(llDelCort,.F.,llDelCort)
    *--Quantity on order. Cannot delete.
    *-- Insert the rejected style into rejected file to print.
    SELECT (lcRejectmp)
    APPEND BLANK
    REPLACE STYLE   WITH &lcCanTmp..cStyle ,;
            Comments WITH 'has Quantity on order.'
    SELECT (lcCanTmp)
  ENDIF    
  
  *B) Check For total Stk and link to GL before deleting.
  IF llLink_Gl .AND. style.totstk > 0
    *-- Flage hold .F. in case we have rejected lines to print.
    llDelCort = IIF(llDelCort,.F.,llDelCort)
    *-- Quantity in inventory. Please adjust the inventory before delete.
    *-- Insert the rejected style into rejected file to print.

    SELECT (lcRejectmp)
    APPEND BLANK
    REPLACE STYLE   WITH &lcCanTmp..cStyle ,;
            Comments WITH 'has quantity in inventory. Please adjust the inventory before delete.'
    SELECT (lcCanTmp)
  ENDIF
  
  *C) Check For Quantity in WIP. Please cancel all WIP first.
  IF style.totwip <> 0
    *-- Flage hold .F. in case we have rejected lines to print.
    llDelCort = IIF(llDelCort,.F.,llDelCort)
    *--Quantity in WIP. Please cancel all WIP first.
    *-- Insert the rejected style into rejected file to print.
    SELECT (lcRejectmp)
    APPEND BLANK
    REPLACE STYLE   WITH &lcCanTmp..cStyle ,;
           Comments WITH 'has Quantity in WIP. Please cancel all WIP first.'
    SELECT (lcCanTmp)
  ENDIF

  *D) Check WIP at warehouse level (case of inter location).
  IF llMultiwh AND STYLE.TotWip = 0 
    SELECT STYDYE
    lnSTDYRec = IIF(EOF(),0,RECNO())
    =SEEK(&lcCanTmp..cStyle) 
    LOCATE REST WHILE Style = &lcCanTmp..cStyle FOR TotWip <> 0
    llSDWipFnd = FOUND()
    IF lnSTDYRec <> 0
      GOTO lnSTDYRec
    ENDIF
    IF llSDWipFnd
      *-- Flage hold .F. in case we have rejected lines to print.
      llDelCort = IIF(llDelCort,.F.,llDelCort)
      *--Quantity in WIP. Please cancel all WIP first.
      SELECT (lcRejectmp)
      APPEND BLANK
      REPLACE STYLE   WITH &lcCanTmp..cStyle ,;
             Comments WITH 'has Quantity in WIP. Please cancel all WIP first at warehouse level.'
    ENDIF
    SELECT (lcCanTmp)
  ENDIF


   *-- Check if the current style\color hase UPCs.
   *-- Make sure UPC is installed before other checckings.
   *B605121,1 HIA 11/25/2001 TO DELETE STYLE EVEN IT HAS UPC(S) 11/25/2001 [BEGIN]
*!*	   IF llUpc AND !lfUPCCheck()
*!*	     llDelCort = IIF(llDelCort,.F.,llDelCort)
*!*	     SELECT (lcRejectmp)
*!*	     APPEND BLANK
*!*	     REPLACE STYLE   WITH &lcCanTmp..cStyle ,;
*!*	              Comments WITH 'has UPCs. Cannot delete.'
*!*	     SELECT (lcCanTmp)
*!*	   ENDIF
   *B605121,1 HIA 11/25/2001 [End]
 
  lcMajor = PADR(CSTYMAJOR,lnstylewid)
  lccolor = RIGHT(cStyle,lnColorLn)
  
  *- Check the style component item type.
  lctype=' '
  FOR lnx = 1 TO 5
    z=STR(lnx,1)
    IF (Make .AND. lcmtype&z = 'S') .OR. (!Make .AND. lcitype&z = 'S')
      lctype = z
      EXIT
    ENDIF
  ENDFOR

  WAIT WINDOW "Deleting Data For The Selected Style Colors..." NOWAIT

  *-- Go to the style+color in the main style file to get some
  *-- information that can be used in deleteing in other files.

   
  *1)--Delete style cost sheet for this style
  SELECT BOM
  SET ORDER TO TAG BomItem
  IF SEEK(lctype+lcMajor)
    LOCATE REST WHILE Typ+Item=lctype+lcMajor ;
           FOR LIKE(STRTRAN(ITEM,'*','?'),ALLTRIM(lcMajor+lcSepart+lccolor))
    IF FOUND()
      *-- Flage hold .F. in case we have rejected lines to print.
      llDelCort = IIF(llDelCort,.F.,llDelCort)
      *--Check if the color used in another style.
      IF SEEK(SUBSTR(CITMMAJOR,1,lnstylewid)+lcSepart+lccolor,'STYLE')
        *-- Insert the rejected style into rejected file to print.
        SELECT (lcRejectmp)
        APPEND BLANK
        REPLACE STYLE   WITH &lcCanTmp..cStyle ,;
                Comments WITH 'is a component in a another style Cost Sheet.'
      ENDIF
    ENDIF
  ENDIF

  *-- Flage hold .F. in case we have rejected lines to print.
  IF llDelCort
    SELECT BOM
    SET ORDER TO TAG Bom
    IF SEEK(lcMajor)
      IF lmaterial
        LOCATE REST WHILE cItmMajor=lcMajor FOR !lmaterial
      ENDIF
      DELETE REST WHILE cItmMajor=lcMajor FOR !lmaterial .AND. ;
                        SUBSTR(CITMMASK,1,LEN(ALLTRIM(lcMajor+lcSepart+lccolor)))=ALLTRIM(lcMajor+lcSepart+lccolor)
    ENDIF 

    *2)--If the syetem is setup to use style packs then delete the
    *-)---style information from the style packs line file.
    IF llpackuse
      IF !llOpnPcks
        llOpnPcks = gfOpenFile(gcDataDir+'SPCK_LIN','SPCKLNST','SH')
      ENDIF
      
      SELECT spck_lin
      IF SEEK('P'+lcMajor+lcSepart+lccolor)
        BLANK REST FOR (Type + Style = 'P'+lcMajor+lcSepart+lccolor)
     	  = SEEK(SPACE(1))
     	  DELETE REST WHILE  TYPE = SPACE(1)
      ENDIF
      IF SEEK('S'+lcMajor+lcSepart+lccolor)
        BLANK REST FOR Type + Style = 'S'+lcMajor+lcSepart+lccolor
        SEEK(SPACE(1))
        DELETE REST WHILE  TYPE = SPACE(1)
      ENDIF
    ENDIF
    
    *3)-- Delete all style UPC nummber for the current style color.
    IF llUpc AND USED('STYLEUPC') AND SEEK(lcMajor+lcSepart+lccolor,'StyleUPC')
      *:B605121,1 HIA/25/2001 TO DELETE STYLE from the catalogs if it has 11/25/2001 [BEGIN]
      *Delete this style form Catalogs if it has.
      =lfUPCDel()
      *:B605121,1 HIA 11/25/2001 [End]
      SELECT styleupc
      SCAN WHILE style+size = lcMajor+lcSepart+lccolor
        SCATTER MEMVAR MEMO
        INSERT INTO RCYLUPC FROM MEMVAR
        *:B605121,1 HIA 11/25/2001 [Begin]
        Select RCYLUPC
        REPLACE RCYLUPC.Desc       WITH STYLE.Desc       ,;
                RCYLUPC.Scale      WITH STYLE.Scale      ,;
                RCYLUPC.PriceA     WITH STYLE.PriceA     ,;
                RCYLUPC.Start      WITH STYLE.Start      ,;
                RCYLUPC.nSugRetPri WITH STYLE.nSugRetPri ,;
                RCYLUPC.Fabric     WITH STYLE.Fabric     ,;
                RCYLUPC.Content1   WITH STYLE.Content1   ,;
                RCYLUPC.TotCost    WITH STYLE.TotCost    


        SELECT styleupc
        *:B605121,1 HIA 11/25/2001 [End]
        
        DELETE
      ENDSCAN
    ENDIF

    *4)--Delete all records in the style dyelot file all dyelots
    *----and all warehouse records for the current style color.
    IF SEEK(lcMajor+lcSepart+lccolor,'StyDye')
      SELECT stydye
      BLANK REST FOR (style+cwarecode+dyelot=lcMajor+lcSepart+lccolor) 
	  =SEEK(SPACE(19))
      DELETE REST WHILE style+cwarecode+dyelot = SPACE(19)
    ENDIF

    *5)--Delete the style foreign prices. 
    IF SEEK(lcMajor+lcSepart+lccolor,'STYPRICE')
      SELECT STYPRICE
      BLANK REST FOR (Style=lcMajor+lcSepart+lccolor) 
      =SEEK(SPACE(19))
      DELETE REST WHILE STYLE = SPACE(19)
    ENDIF

    *6)--Delete Style History.
    SELECT ICSTYHST
    IF SEEK(lcMajor+lcSepart+lccolor)
      DELETE REST WHILE Style=lcMajor+lcSepart+lccolor 
    ENDIF

    *7)--Delete the style record in the main style file.
    SELECT STYLE
    IF SEEK(lcMajor)
      lnFrstRec  = RECNO()
      lnLastRec  = RECNO() 
  	  SCAN REST WHILE Style = lcMajor
        lnFrstRec  = MIN(RECNO(),lnFrstRec)
        lnLastRec  = MAX(RECNO(),lnLastRec)
      ENDSCAN
      IF SEEK(lcMajor+lcSepart+lccolor)
        lnRecToDel = RECNO()
        IF lnFrstRec = lnRecToDel AND lnLastRec <> lnFrstRec
          GO lnLastRec
          SCATTER MEMVAR MEMO
          GO lnFrstRec
          GATHER MEMVAR MEMO
          GO lnLastRec
        ENDIF
        =RLOCK()
	    BLANK
        DELETE
        UNLOCK
      ENDIF
    ENDIF

    *--If all colors was deleted for this style so delete the notepad also.
    IF !SEEK(lcMajor,'STYLE')
      *--Delete Bom.
      SELECT BOM
      SET ORDER TO TAG Bom
      =SEEK(lcMajor)
      LOCATE REST WHILE cItmMajor=lcMajor FOR !lmaterial
      IF FOUND()
        DELETE REST WHILE cItmMajor=lcMajor FOR !lmaterial
      ENDIF  
      *--Delete style notepad.
      =gfOpenFile(gcDataDir+'NotePad','NotePad','SH')

      IF SEEK('F'+lcMajor,'NotePad')
	    SELECT notepad
        BLANK
        DELETE
      ENDIF
    ENDIF
  
    *-- Delete Style From the Browse
    Select (lcCanTmp)
    DELETE 
  ENDIF
  SELECT (lcCanTmp)
ENDSCAN

IF llOpnShst
  USE IN ICSTYHST
ENDIF

IF llOpnBom
  USE IN BOM
ENDIF

IF llpackuse .AND. llOpnPcks
  USE IN SPCK_LIN
ENDIF

IF llSTYPRICE
  USE IN STYPRICE
ENDIF

IF llUpc 
  USE IN STYLEUPC
  USE IN EDICATGH
  USE IN EDICATGD
  USE IN RCYLUPC
ENDIF

*-- REfresh the screen.
=lfVBrw()


*-- Give user message in case delete scuccfule or not.

IF llDelCort
  *- Message Text   :- process completed successfully.
  *- Message Number :- 42219
  *- button message :- OK
  *- button Number  :- 00000
  = gfModalGen("QRM42219B00000","DIALOG")
  
ELSE

  IF !EOF(lcRejectmp) AND gfModalGen ('TRM00000B3200',.F.,.F.,.F.,'Would you like to print a summary of delete process?')=1  
    DO WHILE .T.
     *-- Message : M42208 => 'Print the style-rejected report'
     *-- Button  : B36010 => '<Preview>  <Print>  <Cancel>'
      lnPrintRp  = gfModalGen('QRM42208B42017','DIALOG','the style-rejected')
      R_WIDTH    = 'N'
      lcRpName   = 'ICDELGMA'
      lcSetConsl = SET('CONSOLE')    && save console setting
      DO CASE
        CASE  lnPrintRp =  1
          OGPlatForm   = 'WINDOW'
          lcOGPlatForm = OGPlatForm
          gcDevice     = "SCREEN"
        CASE  lnPrintRp =  2
          lcRpName     = 'ICDELGMA'
          OGPlatForm   = 'DOS'
          lcOGPlatForm = OGPlatForm
          gcDevice     = "PRINTER"
          SET CONSOLE OFF
          IF !pSetup(.T.)
            SET DEVICE TO SCREEN
            RETURN
          ENDIF
        CASE  lnPrintRp =  3
          SET DEVICE TO SCREEN
          RETURN
      ENDCASE
      SELECT (lcRejectmp)
      GOTO TOP
      DO gfDispRe WITH lcRpName
      SET DEVICE TO SCREEN
      IF lnPrintRp != 1  
        EXIT
      ENDIF
      SET CONSOLE &lcSetConsl
    ENDDO
  ENDIF
  SET DEVICE TO SCREEN
ENDIF


WAIT CLEAR
RETURN

*-- End OF lfacptdel.
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
*C102440,1 ABD .
FUNCTION lfvSelRec
PRIVATE lnAlias


lnAlias = SELECT (0)
IF LastKey() =  32
  SELECT (lcCanTmp)
  
  IF EOF()
    RETURN
  ELSE
    lcRecNo = RecNo()  
  ENDIF
  IF &lcCanTmp..cFlag = '»'
    REPLACE &lcCanTmp..cFlag WITH ''
  ELSE
    REPLACE &lcCanTmp..cFlag WITH '»'
  ENDIF
  
  SKIP
  IF EOF()
    SKIP-1
  ENDIF
  = lfwTrnBrow()
  SHOW WINDOW &lcCanWnd REFRESH SAME
ENDIF
SELECT (lnAlias)

*-- End OF lfvSelRec.
*!*************************************************************
*! Name      : lfUPCCheck
*! Developer : Abdou Elgendy (ABD)
*! Date      : 09/27/2001
*! Purpose   : This function checks if there's any UPC records
*! Purpose   : For the current Style/Color
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUPCCheck()
*!*************************************************************
*C102440,1 ABD .
FUNCTION lfUPCCheck

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT EDICATGH
SCAN
  lcScanExp = type+cpartner+cselccode+&lcCanTmp..cStyle
  IF SEEK(lcScanExp,'EDICATGD')
    SELECT EDICATGD 
    SCAN REST WHILE type+cpartner+cselccode+style+size = lcScanExp FOR cEdidStat <> 'D'
      *--Don't delete it.
      SELECT (lnAlias)
      RETURN .F.
    ENDSCAN
  ENDIF
ENDSCAN
SELECT (lnAlias)

*-- End Of lfUPCCheck.

*!*************************************************************
*! Name      : lfUPCDel
*! Developer : HASSAN IBRAHIM ALI [HIA]
*! Date      : 11/25/2001
*! Purpose   : This function checks if this style in catalogs , and del it.
*! Purpose   : For the current Style/Color
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : NONE
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfUPCDel()
*!*************************************************************
FUNCTION lfUPCDel
PRIVATE lnAlias , llSend , llTransmit
lnAlias = SELECT(0)
SELECT EDICATGH
llTransmit = .F.
SCAN
  lcScanExp = Type+cPartner+cSelcCode+&lcCanTmp..cStyle
  IF SEEK(lcScanExp , 'EDICATGD') AND !INLIST(EDICATGD.cEdiDStat,"D")
     SELECT EDICATGD
     llSend = .F. 
     SCAN REST WHILE Type+cPartner+cSelcCode+Style+Size = lcScanExp 
      *Delete This Style/Size
      *B605924,1 ASH 05/20/2002 (Begin) Remove the delete to be out of the scan loop.
      *IF EMPTY(EDICATGD.SENDDATE)
        *DELETE 
        *llSend = .F.
      *ELSE
      IF !EMPTY(EDICATGD.SENDDATE)
      *B605924,1 ASH 05/20/2002 (End)
        REPLACE cEdiDStat WITH 'D',;
                lTransmit WITH .T. ,;
                DelDate   WITH gdSysDate
                
        REPLACE EDICATGH.lTransmit WITH .T.
        llSend = .T.
     ENDIF  
    ENDSCAN
    *B605924,1 ASH 05/20/2002 (Begin) Delete all unneeded records.
    IF SEEK(lcScanExp)
      DELETE REST WHILE Type+cPartner+cSelcCode+Style+Size = lcScanExp FOR EMPTY(SENDDATE)
    ENDIF
    *B605924,1 ASH 05/20/2002 (End)
    IF llSend = .T.
      llTransmit = .T.
      = SEEK(EDICATGH.TYPE+EDICATGH.CPARTNER,'EDIACPRT','ACCFACT')
      SELECT EdiTrans
      IF !SEEK('832'+PADR(EDICATGH.CSELCCODE,20)+EDICATGH.TYPE+EDICATGH.CPARTNER)
        APPEND BLANK
      ENDIF
      REPLACE ceditrntyp WITH '832',;
              KEY        WITH EDICATGH.CSELCCODE,;
              CPARTNER   WITH EDICATGH.CPARTNER ,;
              TYPE       WITH EDICATGH.TYPE     ,;
              lInterComp WITH EdiAcPrt.lInterComp ,;
              CSTATUS    WITH 'N'
              
      SELECT (lcRejectmp)
      APPEND BLANK
      REPLACE STYLE    WITH &lcCanTmp..cStyle ,;
              Comments WITH 'has 832 Transaction Of Catalog "+ cSelcCode +" Which Is Not Sned, Please Send First. Cannot delete.'

    ENDIF        
  ENDIF
ENDSCAN        

IF llTransmit = .T.
  *llTransmit = .F.
  SELECT EDICATGH
  SCAN
    lcScanExp = Type+cPartner+cSelcCode+&lcCanTmp..cStyle
    IF SEEK(lcScanExp , 'EDICATGD') AND EDICATGD.lTransmit
   *  llTransmit = .T.
      SELECT (lcRejectmp)
      APPEND BLANK
      REPLACE STYLE    WITH &lcCanTmp..cStyle ,;
              Comments WITH 'has 832 Transaction Of Catalog "+ cSelcCode +" Which Is Not Sned, Please Send First. Cannot delete.'
     Exit
    ENDIF
  ENDSCAN
ENDIF


SELECT (lnAlias)
*-- End Of lfUPCCheck.
*!*************************************************************

