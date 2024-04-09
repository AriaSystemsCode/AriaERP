*!**************************************************************************
*! Program file        : POADORD.PRG
*! Program description : Adornment Production Order Planning Tool .
*! For screen          : POADORD.SPR
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : Style Purchase Order - (PO)
*! Developer Name      : Ahmed Maher (AMH) Due to C#102244,1
*!**************************************************************************
*! Calls               : MFADORD.SPX
*!**************************************************************************
*! Passed Parameters   : None.
*!**************************************************************************
*! Example             : 
*!**************************************************************************
*! Modification        :
*! B604656,1 AMH 07/12/2001 Fix the bug of not replacing the ware house
*! B604656,1                address with the correct format.
*! B804480,1 AMH 10/17/2001 Fix the bug of not getting the order line
*! B804480,1 AMH            that has been partially picked.
*! B605467,1 AMH 02/04/2002 Change the validation of last order start ship date.
*! B605713,1 AMH 03/20/2002 Fix the bug of collecting Hold order lines.
*! B605879,1 AMH 04/18/2002 Use custom field cPikTkt instead of 
*! B605879,1                piktkt field in cutpick file.
*!**************************************************************************

EXTERNAL ARRAY laScrMode,laDefProc
DIMENSION laType[1,2]
DIMENSION laWare[1]

*-- Inetialize all logic variables
*--laDefProc[9]  = .F.   : Force the local save   proc.  (lpSavScr)
*--laDefProc[10] = .F.   : Force the local cancel proc. (lpClsScr)

STORE .F. TO llNoShow,laScrMode,laDefProc[9],laScrMode,laDefProc[10],;
             llLinkToGl,llbrowse,glFromBrow,llAlowNew
STORE .T. TO laScrMode[1]
lcPassType = 'A'


*-- Inetialize all string variables

STORE ''  TO lcWinCh1,lcWinCh2,lcWinCh3,lcOldVal,lcBrwFlds,;
             laType,laWare,lcTmpOrder,lcTmpBom,lcTrgAudit,lcGlLink
STORE SPACE(6)  TO lcHdrOrder , lcPosHdrNo 
laType[1,1]='Generate New Adornment Order'
laType[1,2]='G'
lcBrowTitl = 'Order Lines'
lcDbSelPrm = 'Se\<lect'
lcForExp   = [STATUS='O' .AND. CPICKTYPE='A']

*-- Inetialize all State control variables

STORE 'DISABLE' TO lcSelAllSt,lcSelNonSt,lcStatus,lcOSelAllSt,lcOSelNonSt

*-- Inetialize all numeric variables             

*--lnSelRec: Variable to hold the number of selected records of order tmp file

STORE 0 TO lnSelRec  , lnSware    , lnTware
STORE 1 TO lnBrRecNo , lnMarker

*-- Inetialize all date variables
*-- ldStartDate : variable that hold the smallest date from all orders by which
*--               we will update all files
*-- ldStrtDate  : variable that hold the start from which we will include or
*--               Exclude the order ( on screen get field)
STORE {} TO ldVisWind , ldLOSSDate , ldStartDate
STORE gdSysDate TO ldStrtDate

IF !gfSetup()
  RETURN
ENDIF

*-- Get all neede memory variables [START]
DIMENSION laSetups[6,2]
laSetups[1,1]  = 'M_WareHouse'
laSetups[2,1]  = 'M_BOMVAR'       
laSetups[3,1]  = 'M_AOVW'
laSetups[4,1]  = 'M_Link_GL'


*-- Save Setup Parameters in memvar [Begin]
laSetups[5,1] = "M_MAAP"
laSetups[6,1] = "M_CLIR"


=gfGetMemVar(@laSetups,gcAct_Comp)

llWareHous  = (laSetups[1,2] ='Y')
ldVisWind   = 366
llLinkToGl  = (laSetups[4,2] ='Y')

lnCmpMnAlo = laSetups[5,2]  && Company Min. Allowable allocation Percentage.
llCmpRatio = laSetups[6,2]  && Company Line item ratio. 
*-- Save Setup Parameters in memvar [End  ]

*C102244,4 AMH Get the defaulte cpurcode from codes file [Start]
llOpenCode = .F.
IF !USED('CODES')
  =gfOpenFile(gcDataDir+'CODES','CCODE_NO','SH')
  llOpenCode = .T.
ENDIF
IF SEEK('DCPURCODE  ','CODES')
  lcPurCode = CODES.CCODE_NO
ELSE
  lcPurCode = SPACE(6)
ENDIF
IF llOpenCode
  USE IN CODES
ENDIF
*C102244,4 AMH [End]

*-- Get all neede memory variables [END..]

IF !llWareHous
  *-- Can't generate adornment order for single warehous company.
  =gfModalGen('TRM00000B42000','ALERT','','',;
              "Can't generate adornment order for single warehous company.")
  glQuitting = .T.
  RETURN
ENDIF   

IF !laSetups[2,2]
  *-- System hasn't been setup to use variant cost sheet, Cannot proceed.
  =gfModalGen('TRM00353B00000','DIALOG')
  glQuitting = .T.
  RETURN
ENDIF

IF !WEXIST(gcBaseWind)

  lcBaseFile = ''
  lcWinCh1   = gfTempName()
  lcWinCh2   = gfTempName()
  lcWinCh3   = gfTempName()
  lcTmpOrder = gfTempName()
  lcTmPikOrd = gfTempName()
  lcTmpBom   = gfTempName()              
  
  *-- Creat the temp files .
  =lfCrtTmp()

  =gfOpenFile(gcDataDir+'POSLN'   ,'POSLN'   ,'SH')
  =gfOpenFile(gcDataDir+'CUTPICK'  ,'CUTORD' ,'SH')
  =gfOpenFile(gcDataDir+"SCALE","SCALE",'SH')
  
  SELECT Piktkt
  SET RELATION TO
  SET RELATION TO 'O'+ Piktkt.order INTO Ordhdr ADDITIVE
  SET RELATION TO 'M'+ Piktkt.account INTO Customer ADDITIVE
  SET RELATION TO 'O'+ Piktkt.order INTO Ordline ADDITIVE
  
  SELECT Ordline
  SET RELATION TO
  SET RELATION TO 'SO'+ Ordline.order+STR(Ordline.lineno,6) INTO Bomvar ADDITIVE

  SELECT Bomvar
  SET RELATION TO
  SET RELATION TO Bomvar.cdsgncode INTO Icdesign ADDITIVE

  SELECT WAREHOUS

  SELECT cWareCode+'-'+cDesc FROM Warehous INTO ARRAY laWare 

ENDIF
PUSH KEY

*--Activate Browse window
*-- Define a menu bar with a short-cut key for browse window 
*-- in the program just to be able to activate the browse window
*-- using its menu bar short-cut key.
DEFINE BAR 099 OF P01PU01 PROMPT "\-" SKIP FOR .T.
DEFINE BAR 100 OF P01PU01 PROMPT lcBrowTitl KEY ALT+B
ON SELECTION BAR 100 OF P01PU01 ACTIVATE WINDOW (lcBrowTitl)

**-------------------------- Call the screen

DO (gcScrDir+gcWinAppl+"\POADORD.SPX")

POP KEY
RELEASE WINDOW (lcBrowTitl)
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01
**-------------------------- Normal Quitting
IF glQuitting
  *-- Deactivate the filters
  *-- Erase tmp file if it was created
  IF USED(lcTmpOrder)
    USE IN (lcTmpOrder)
    ERASE (gcWorkDir+lcTmpOrder+'.DBF')
    ERASE (gcWorkDir+lcTmpOrder+'.CDX')
  ENDIF  
  IF USED(lcTmpBom)
    USE IN (lcTmpBom)
    ERASE (gcWorkDir+lcTmpBom+'.DBF')
    ERASE (gcWorkDir+lcTmpBom+'.CDX')
  ENDIF  
ENDIF  

*!*************************************************************
*! Name               : lpShow
*! Developer          : Ahmed Maher (AMH)
*! Date               : 05/23/2001
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpShow
*!************************************************************
*!
PROCEDURE lpShow
SELECT(lcTmpOrder)
DO CASE
  *-- S E L E C T  M O D E.
  CASE laScrMode[1]
  
    STORE gdSysDate+ldVisWind TO ldLOSSDate 
    STORE gdSysDate TO ldStrtDate
    STORE 'DISABLE' TO lcSelAllSt,lcSelNonSt,lcStatus,lcOSelAllSt,lcOSelNonSt
    STORE 0 TO lnSWare,lnTWare
    STORE 1 TO lnType
    STORE '' TO lcGlLink,lcTrgAudit
    SHOW GET lnType DISABLE
    SHOW GET ibGlLink ENABLE       
    SHOW GET lcGlLink ENABLE       
    SHOW GET lnSware DISABLE
    SHOW GET lnTware DISABLE
    SHOW GET ldLOSSDate ENABLE
    SHOW GET ldStrtDate ENABLE              
    _CUROBJ=OBJNUM(ldStrtDate)      
    
  *-- A D D  M O D E. 
  CASE laScrMode[4]  
    llCUpDate  = .T.
    SHOW GET ldLOSSDate DISABLE
    SHOW GET ldStrtDate DISABLE
    SHOW GET lnType DISABLE    
    SHOW GET lnSware  ENABLE
    SHOW GET lnTware  ENABLE
    SHOW GET ibGlLink ENABLE
    SHOW GET lcGlLink ENABLE
    lcStatus='ENABLE'
ENDCASE
SHOW GET pbDSelect &lcStatus
SHOW GET pbDInvert &lcStatus
SHOW GET pbOrdDet  &lcStatus

SHOW GET pbDSelAll &lcSelAllSt
SHOW GET pbDSelNon &lcSelNonSt

=lfBrow() AND lfwBrow()
*:*************************************************************
*! Name      : lfBrow
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : Activate screen browse.
*:*************************************************************
FUNCTION lfBrow

SELECT (lcTmpOrder)
LOCATE

IF !EMPTY(lcBrowTitl) AND WEXIST(lcBrowTitl)
  RELEASE WINDOW (lcBrowTitl)
ENDIF

lcBrwFlds =  [cMarker = IIF(lnMarker = RECNO(lcTmpOrder),'>',' ') :H=' ',]+;
             [lcSelect=IIF(LLSEL , '»' , ' ') :R :H= '»' ,]+;
             [Order :10 :H='Order #',]+;
             [Piktkt :10 :H='Piktkt #',]+;
             [Season  :9 :H='Season',]+;
             [Account    :11 :H='Account',]+;
             [stname    :25 :H='Account Name',]+;             
             [Start     :10 :H='Start',]+;
             [complete      :10 :H='Complete',]+;
             [custpo      :15 :H='Customer Po#',]+;
             [totpik      :7 :H='Totqty']

BROWSE FIELDS &lcBrwFlds      ; 
      	   SAVE               ;
       	   NOWAIT             ;
           NOCLEAR            ;
           NOEDIT             ;
           NOMENU             ;  
           WHEN lfwBrow()     ;
           VALID :F lfvBrow() ;         
    WINDOW (lcWinCh2)         ;
    IN WINDOW (gcBaseWind)    ; 
    TITLE  (lcBrowTitl)
*!*************************************************************
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : When valid function for browse.
*!*************************************************************
FUNCTION lfwBrow
lnMarker = RECNO(lcTmpOrder)
glFromBrow = .T.
=lfPrmCtrl()
SHOW WINDOW (lcBrowTitl) REFRESH SAME

*!*************************************************************
*! Name      : lfvBrow
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : TO CHECK IF comming from browse to call gfStopBrow() 
*!             function
*!*************************************************************
FUNCTION lfVBrow
IF WONTOP() # (lcBrowTitl)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
FUNCTION lfOldValue
lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfRcvDate
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************

FUNCTION lfRcvDate

*B605467,1 AMH Change the validation of last order start ship date [Start]
*IF ldLossDate < gdSysDate
IF ldLossDate < ldStrtDate
  *-- New 
  *-- Date must be greater than XXX .
  *=gfModalGen('INM00000B42000','DIALOG','','','Date must be greater than '+DTOC(gdSysDate)+' .')
  =gfModalGen('INM00000B42000','DIALOG','','','Date must be greater than '+DTOC(ldStrtDate)+' .')
*B605467,1 AMH [End]

  ldLossDate=lcOldVal
  _CUROBJ = _CUROBJ
  RETURN
ENDIF

*!*************************************************************
*! Name      : lfvLinkCod
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : Valid function to validate gl link code field.
*!*************************************************************
*! Calls     : gfGLBrowse 6chr 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvlinkcod()
*!*************************************************************
FUNCTION lfvlinkcod

lnAlias=SELECT()

IF llbrowse OR !EMPTY(lcGlLink)
  lcGllnk6   = lcGlLink
  =gfGLbrowse('05',@lcGllnk6,'',0)
  lcGlLink = lcGllnk6
ENDIF

lcGlLink = IIF(EMPTY(lcGlLink),'DEFDEF',lcGlLink)
SHOW GET lcGlLink
llbrowse = .F.
SELECT(lnalias)
*!*************************************************************
*! Name      : lfvCollect
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : Valid function to Collect push butt.
*!*************************************************************
*! Calls     :  
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Passed Parameters  : ............
*!*************************************************************
*! Example   : =lfvCollect()
*!*************************************************************

FUNCTION lfvCollect

SHOW GET lnSware ENABLE
SHOW GET lnTware ENABLE
IF !lfGetInfo()
  *-- No records to display
  =gfModalGen('TRM00000B42000','ALERT','','','No records to display.')
  _CUROBJ=OBJNUM(ldLOSSDate)
  RETURN
ENDIF

*- Code to Automatic detection of Select All , Select None , Restore Defaults
*- Must be here befor mode change .

lcSelAllSt = IIF(lnSelRec = RECCOUNT() , 'DISABLE' , 'ENABLE')
lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')

laScrMode=.F.
laScrMode[4]=.T.
SHOW GETS

*!*************************************************************
*! Name      : lfvControl
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*!
FUNCTION lfvControl
PARAMETERS lcEvent
PRIVATE lcScanExpr
*-- Select / UnSelect Record.

IF lcEvent = "SEL_UNSEL"
  =lfSelUnSel()

*-- any event but not select or unselect

ELSE  && Sel All, Sel None, Or Invert
  
  DO CASE

    CASE lcEvent = "INVERT"  && Invert Case
      lcScanExpr = []
    
    CASE lcEvent = "SEL_ALL"  && Select All Case
      lcScanExpr = [FOR !llSel]
    
    CASE lcEvent = "SEL_NON"  && Select None Case
      lcScanExpr = [FOR llSel]
    

  ENDCASE
  
  SCAN &lcScanExpr
    =lfSelUnSel()
  ENDSCAN  


ENDIF

GO lnMarker
=lfPrmCtrl()
lcSelAllSt = IIF(lnSelRec = RECCOUNT() , 'DISABLE' , 'ENABLE')
lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')
SHOW GET pbDSelAll &lcSelAllSt
SHOW GET pbDSelNon &lcSelNonSt
SHOW WINDOW (lcBrowTitl) REFRESH SAME

*!*************************************************************
*! Name      : lfSelUnSel
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*!
FUNCTION lfSelUnSel

SELECT (lcTmpOrder)
REPLACE llSel WITH !llSel

*-- if it is now selected record.
IF llSel
  lnSelRec = lnSelRec + 1
  =lfDsgnToBom(.T.)

ELSE  && else if it is now unselected record.
  lnSelRec = lnSelRec - 1 
  =lfDsgnToBom(.F.)
ENDIF
*!*************************************************************
*! Name      : lfDsgnToBom
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : Reflect the changes in Tmp Dsgn To Tmp Bomvar 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : llAdd = .T. if called to select records to tmpbom
*!                           = .F. if called to unselect records to tmpbom
*!*************************************************************
*! Return      : .T.
*!*************************************************************
*
FUNCTION lfDsgnToBom
PARA llAdd
PRIVATE lcForExp,llZeroLine,lnQty
lcForExp = IIF(llAdd,[!llSEL],[llSEL])
SELECT (lcTmpBom)
=SEEK('SO'+EVALUATE(lcTmpOrder+'.ORDER')+STR(EVALUATE(lcTmpOrder+'.lineno'),6))
SCAN REST WHILE cidtype+ccost_id+STR(lineno,6) = ;
                'SO'+EVALUATE(lcTmpOrder+'.ORDER')+STR(EVALUATE(lcTmpOrder+'.lineno'),6);
          FOR &lcForExp
  IF llAdd
    REPLACE llSel WITH .T.
  ELSE
    REPLACE llSel WITH .F.
  ENDIF  
ENDSCAN 
SELECT (lcTmpOrder)    

*!*************************************************************
*! Name      : lfPrmCtrl
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : Function to control the push button Select prompt
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
*
FUNCTION lfPrmCtrl

IF laScrMode[4]

  IF !LLSEL
    lcDbSelPrm = 'Se\<lect'
    SHOW GET pbDSelect ,1 PROMPT lcDbSelPrm &lcStatus
  ELSE    
    lcDbSelPrm = 'UnSe\<lect'
    SHOW GET pbDSelect ,1 PROMPT lcDbSelPrm &lcStatus
  ENDIF   
ENDIF

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*!
FUNCTION lfGetInfo
PRIVATE llFirst
llFirst=.T.
*--  SELECT Piktkt
*--  SET RELATION TO 'O'+ Piktkt.order   INTO Ordhdr   ADDITIVE
*--  SET RELATION TO 'M'+ Piktkt.account INTO Customer ADDITIVE
*--  SET RELATION TO 'O'+ Piktkt.order   INTO Ordline  ADDITIVE
  
*--  SELECT Ordline
*--  SET RELATION TO 'SO'+ Ordline.order+STR(Ordline.lineno,6) INTO Bomvar ADDITIVE

*--  SELECT Bomvar
*--  SET RELATION TO Bomvar.cdsgncode INTO Icdesign ADDITIVE


SELECT Piktkt
*-- lcForExp  : [STATUS='O']

SCAN FOR EVALUATE(lcForExp)
  
  *--  The 1st selection condition .
  
  *B605713,1 AMH Don't collecting the hold order lines [Start]
  *IF !BETWEEN(Ordhdr.Start,ldStrtDate,ldLOSSDate)
  IF !BETWEEN(Ordhdr.Start,ldStrtDate,ldLOSSDate) .OR. ORDHDR.STATUS = 'H'
  *B605713,1 AMH [End]
  
    LOOP
  ENDIF
  
  *-- Save the smallest start date order number , we will use it to 
  *-- get information in saving
  IF llFirst
    ldStartDate = Ordhdr.Start
    lcHdrOrder  = Ordhdr.Order 
    llFirst     = .F.
  ENDIF
  IF Ordhdr.Start < ldStartDate
    ldStartDate = Ordhdr.Start
    lcHdrOrder  = Ordhdr.Order 
  ENDIF
 
  lcScanExp = 'O'+Piktkt.order

  SELECT ORDLINE
  SCAN REST WHILE Cordtype+order = lcScanExp FOR PIKTKT=PIKTKT.PIKTKT
    **-- Code to fill the order detail tmp file [START]
    **-- This file will carry the final order lines from which we will make the order
    IF !EOF('BOMVAR') 
      
      IF lfLoop()
        LOOP
      ENDIF
      
      SCATTER MEMVAR MEMO
      m.STNAME = CUSTOMER.STNAME
      m.llSel  = .F.

      INSERT INTO (lcTmpOrder) FROM MEMVAR

      **-- this file will carry the sublines from bomvar related with the record
      **-- of orderline file .
      SELECT Piktkt        &&-- to update the record pointers
      SELECT BOMVAR
      
      SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)='SO'+ORDLINE.ORDER+STR(ORDLINE.lineno,6)
        
        SCATTER MEMVAR MEMO
        m.llSel      = .F.
        m.TOTPIK     = ORDLINE.TotPik
        m.Gros_price = ORDLINE.Gros_price  
        
        INSERT INTO (lcTmpBom) FROM MEMVAR 

      ENDSCAN  &&-- ENDSCAN of bomvar
      SELECT ORDLINE
    ENDIF  &&-- endif !EOF('BOMVAR') 
  ENDSCAN  &&-- ENDSCAN of ORDLINE
  SELECT Piktkt
ENDSCAN &&-- ENDSCAN of Piktkt

SELECT (lcTmpOrder)
GO TOP
IF EOF() OR BOF()
  RETURN(.F.)
ENDIF

*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*!

FUNCTION lfCrtTmp
*------------------ CREATE ORDER TEMP. FILE -> [BEGIN]

SELECT ordline
DIMENSION laFileStru[1,4]
STORE SPACE(1) TO laFileStru
=AFIELDS(laFileStru)
lnFStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFStru+5,4]

laFileStru[lnFStru+1,1] = 'STNAME'
laFileStru[lnFStru+1,2] = 'C'
laFileStru[lnFStru+1,3] =  30


laFileStru[lnFStru+2,1] = 'LLSEL'
laFileStru[lnFStru+2,2] = 'L'
laFileStru[lnFStru+2,3] =  1

laFileStru[lnFStru+3,1] = 'NCOST2'
laFileStru[lnFStru+3,2] = 'N'
laFileStru[lnFStru+3,3] =  13
laFileStru[lnFStru+3,4] =  3

laFileStru[lnFStru+4,1] = 'POSLNPOS'
laFileStru[lnFStru+4,2] = 'N'
laFileStru[lnFStru+4,3] =  6
STORE 0 TO laFileStru[lnFStru+1,4]  , laFileStru[lnFStru+2,4] , laFileStru[lnFStru+4,4]   

laFileStru[lnFStru+5,1] = 'LEXIST'
laFileStru[lnFStru+5,2] = 'L'
laFileStru[lnFStru+5,3] =  1
laFileStru[lnFStru+5,4] =  0


CREATE TABLE &gcWorkDir.&lcTmpOrder FROM ARRAY laFileStru
INDEX ON cordtype+order+STR(lineno,6) TAG (lcTmpOrder)
INDEX ON PIKTKT+cordtype+order+STR(lineno,6) TAG (lcTmPikOrd) OF (lcTmpOrder)
SET ORDER TO TAG (lcTmpOrder)
*------------------ CREATE ORDER TEMP. FILE -> [END..]

*------------------ CREATE BOMVAR TEMP. FILE -> [BEGIN]
SELECT BOMVAR
STORE SPACE(1) TO laFileStru
=AFIELDS(laFileStru)
lnFStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFStru+4,4]


laFileStru[lnFStru+1,1] = 'LLSEL'
laFileStru[lnFStru+1,2] = 'L'
laFileStru[lnFStru+1,3] =  1
laFileStru[lnFStru+1,4] = 0

laFileStru[lnFStru+2,1] = 'STNAME'
laFileStru[lnFStru+2,2] = 'C'
laFileStru[lnFStru+2,3] =  30
laFileStru[lnFStru+2,4] = 0

laFileStru[lnFStru+3,1] = 'TOTPIK'
laFileStru[lnFStru+3,2] = 'N'
laFileStru[lnFStru+3,3] =  6
laFileStru[lnFStru+3,4] =  0

laFileStru[lnFStru+4,1] = 'Gros_price'
laFileStru[lnFStru+4,2] = 'N'
laFileStru[lnFStru+4,3] =  8
laFileStru[lnFStru+4,4] =  2

CREATE TABLE &gcWorkDir.&lcTmpBom FROM ARRAY laFileStru
INDEX ON cidtype+ccost_id+STR(lineno,6) TAG (lcTmpBom)
*------------------ CREATE BOMVAR TEMP. FILE -> [END..]
SELECT (lcTmpOrder) 


**--------------------------------------------------------------------------
**------------------------((Trapping Section )) ----------------------------
*!*************************************************************
*! Name               : lpTrap
*! Developer          : Ahmed Maher (AMH)
*! Date               : 05/23/2001
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  DO lpTrap
*!*************************************************************
*!
PROCEDURE lpTrap
PARAMETERS Win_Name,UNTRAP_KEY
ON KEY LABEL &UNTRAP_KEY        &&-- Avoiding infinit loop by untrap
ACTIVATE WINDOW &Win_Name

*!*************************************************************
*! Name               : lfDeact
*! Developer          : Ahmed Maher (AMH)
*! Date               : 05/23/2001
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDeact()
*!*************************************************************
*!
FUNCTION lfDeact
*-- Trap keys in this function
IF WONTOP()=(lcBrowTitl)
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpTrap WITH (lcWinCh3),'TAB'
  ON KEY LABEL BACKTAB    DO lpTrap WITH (lcWinCh1),'BACKTAB'
  ON KEY LABEL ESCAPE     LLDUMMY1=.T.
ENDIF

*!*************************************************************
*! Name               : lfAct
*! Developer          : Ahmed Maher (AMH)
*! Date               : 05/23/2001
*! Purpose            : 
*!*************************************************************
*! Calls              : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfAct()
*!*************************************************************
*!
FUNCTION lfAct
*-- Untrap keys in this function
IF WONTOP() # (lcBrowTitl)  
  IF glFromBrow
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    ON KEY LABEL ESC DO gfEscap
    =gfStopBrow()
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lpClsScr
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : Cancel/Close entery.
*!*************************************************************
PROCEDURE lpClsScr
STORE 'DISABLE' TO lcSelAllSt,lcSelNonSt
STORE 'DISABLE' TO lcOSelAllSt,lcOSelNonSt
SELECT (lcTmpBom)
ZAP
SELECT (lcTmpOrder)
ZAP
SELECT (lcTmpOrder)
ZAP

*-------------------------------------------------------------------------
*-------------------------- ( Save section ) -----------------------------
*!************************************************************************
*! Name      : lpSavScr
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : 
*!************************************************************************
*! Calls     : None
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            :  
*!************************************************************************
PROCEDURE lpSavScr

PRIVATE lnCrRec , lnLastLine , lcAdorNO , lnLineNo , lnTotQty , lnNicost1 ,;
        lcOrdKey,llFilesUpdt,llBomUpdt,llIgnorBom,llIgnorFil

STORE .F. TO llFilesUpdt,llBomUpdt,llIgnorBom,llIgnorFil
*-- Check for any selected lines & Source location = target location [begin]

SELECT (lcTmpOrder)
lnCrRec =  IIF(EOF(),0,RECNO())
LOCATE FOR llSEL

IF !FOUND()
  *--No [Receiving Lines] was done,Cannot update.
    = gfModalGen('INM00000B42000','DIALOG','','','No piktkt selection was done. Cannot update.')
  STORE .F. TO llShow,llCSave
  
  *-- Check of phisical record before go to it.
  IF BETWEEN(lnCrRec,1,RECCOUNT())
    GOTO lnCrRec
  ENDIF
  RETURN .F.
ENDIF

IF lnSware = 0
  *--You have to select a XXX  
  = gfModalGen('TRM00000B42000','DIALOG','','','You have to select a source location. ')
  STORE .F. TO llShow,llCSave
  RETURN .F.
  _CUROBJ=OBJNUM(lnSware)        
ENDIF	

IF lnTware =0
  *--You have to select a XXX 
  = gfModalGen('TRM00000B42000','DIALOG','','','You have to select a target location. ')
  STORE .F. TO llShow,llCSave
  RETURN .F.  
  _CUROBJ=OBJNUM(lnTware)          
ENDIF	

IF lnSware = lnTware
  *--Source location is the same as target location , Cannot update
  = gfModalGen('TRM00000B42000','DIALOG','','',;
               'Source location is the same as target location , Cannot update')
  STORE .F. TO llShow,llCSave
  RETURN .F.  
ENDIF	

IF llLinkToGl AND EMPTY(lcGlLink) 
  *--You have to select a XXX 
  = gfModalGen('TRM00000B42000','DIALOG','','','You have to select a GL Link Code.')
  STORE .F. TO llShow,llCSave
  RETURN .F.  
  _CUROBJ=OBJNUM(lcGlLink)          
ENDIF
*-- Check for selected lines & Source location = target location [end..]

=gfOpenFile(gcDataDir+'STYLE'   ,'STYLE'   ,'SH')
=gfOpenFile(gcDataDir+'STYDYE'  ,'STYDYE'  ,'SH')
=gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','SH')

=SEEK('O'+lcHdrOrder,'ORDHDR')
*AHMED
*=SEEK(PADR(laware[lnSware],6),'WAREHOUS')
=SEEK(PADR(laware[lnTware],6),'WAREHOUS')
*AHMED
*-- The main purpose of the next loop is to make a selected subset from (lcTmpOrder)

SELECT PIKTKT
SET ORDER TO TAG Ordpik
SELECT (lcTmpOrder)
SET ORDER TO TAG (lcTmPikOrd)
SET RELATION TO "SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6) INTO (lcTmpBom)
*-- The next relation makes the tmp order lines file see all related records in all files
SET RELATION TO order + piktkt INTO Piktkt ADDITIVE
LOCATE
*-- Vriables that will be used in this loop

****-- Accomulators

*-- lnTotQty    
*--             : This variable will be used to update POSHDR.NSTYORDER & POSHDR.OPEN
*--             : This variable will be initialized with zero if new poshdr record
*--             : is created otherwise will be initialized with POSHDR.NSTYORDER
*--             : This variable will not be reset inside the main loop
*--             : its equation : [lnTotQty = lnTotQty+&lcTmpOrder..TOTPIK]

*-- lnNicost1   : This variable will be used to update POSHDR.Nicost1 & POSHDR.NFcost1
*--             : This variable will be initialized with zero and will be reset 
*--             : inside the main loop per record , POSHDR will be accomulated with this 
*--             : through the main loop
*--             : Its equation : [lnNicost1=NCOST1*&lcTmpOrder..TOTPIK] in the main loop 

*-- lnLineNo    : This variable will be used to update POSLN.Lineno and POSHDR.Lastline
*--             : It will be initialized to zero if new poshdr record
*--             : is created otherwise will be initialized with POSHDR.Lastline
*--             : It will be incremented through the main loop only if this line
*--             : is a new for this adornment order

****-- Logic and indicators

*-- POSLNPOS    : It is a field in lcTmpOrder will be used to save the recordno of the posln record 
*--             : if this line is an old one with different piktkt number
*--             : I did so since I will need to go to this record later
*--             : so there is no need to SEEK() it again .
*--             : Its initial value is zero , it means that this is a new line for this order
*--             : This value may be changed in lpChkLine .

*-----------------------    Main Loop [start]    ----------------------
STORE SPACE(6) TO lcPikTkt, lcFirst, lcLast
*--Standard process.
SCAN       &&-- (lcTmpOrder)

  IF llSEL
    IF !(PIKTKT == lcPikTkt)
      SELECT POSHDR
      IF !EMPTY(lcPikTkt) .AND. gfObj_Lock(.T.)
        REPLACE NSTYORDER WITH lnTotQty , OPEN WITH lnTotQty 
        =gfObj_Lock(.F.)      
      ENDIF
      lcPikTkt = EVALUATE(lcTmpOrder+'.PIKTKT')
      *-- Generate a new sequence number if you are sure that you will update.
      lcAdorNO=gfSequence('POAD')
      IF EMPTY(lcFirst)
        lcFirst = lcAdorNo
      ENDIF
      lcLast = lcAdorNo
      APPEND BLANK
      REPLACE CSTYTYPE   WITH 'A'                     ,;
              PO         WITH lcAdorNO                ,;
              VENDOR     WITH PADR(laware[lnSware],6) ,;
              CWARECODE  WITH PADR(laware[lnTware],6) ,;     
              STATUS     WITH 'O'                     ,;
              CDIVISION  WITH ORDHDR.CDIVISION        ,;
              ENTERED    WITH gdSysDate               ,;
              COMPLETE   WITH ORDHDR.COMPLETE         ,;
              AVAILABLE  WITH ORDHDR.COMPLETE         ,;
              CTERMCODE  WITH ORDHDR.CTERMCODE        ,;
              SHIPVIA    WITH ORDHDR.SHIPVIA          ,;
              LASTLINE   WITH 0                       ,;
              NICOST1    WITH 0                       ,;
              NSTYORDER  WITH 0                       ,;
              OPEN       WITH 0                       ,;
              LINK_CODE  WITH lcGlLink
      *B604656,1 AMH 07/12/2001 (Begin) Using the gfGetAdd to get the
      *B604656,1                address with the correct format.
      *REPLACE SHPNAME    WITH WAREHOUS.cdesc          ,;
              COUTADDR1  WITH WAREHOUS.CADDRESS1      ,;
              COUTADDR2  WITH WAREHOUS.CADDRESS2      ,;
              COUTADDR3  WITH WAREHOUS.CADDRESS3      ,;
              COUTADDR4  WITH WAREHOUS.CADDRESS4      ,;
              COUTADDR5  WITH WAREHOUS.CADDRESS5      ,;          
              CPRICECUR  WITH gcBaseCurr              ,;
              CDUTYCUR   WITH gcBaseCurr              ,;            
              NPRICERAT  WITH 1                       ,;
              NFCOST1    WITH 0                       ,;
              NCURRUNIT  WITH 1                       ,;
              NDCURUNIT  WITH 1                       ,;
              CPURCODE   WITH lcPurCode
      REPLACE SHPNAME    WITH WAREHOUS.cdesc                         ,;
              COUTADDR1  WITH gfGetAdr('WAREHOUS' , '' , '' , '' , 1),;
              COUTADDR2  WITH gfGetAdr('WAREHOUS' , '' , '' , '' , 2),;
              COUTADDR3  WITH gfGetAdr('WAREHOUS' , '' , '' , '' , 3),;
              COUTADDR4  WITH gfGetAdr('WAREHOUS' , '' , '' , '' , 4),;
              COUTADDR5  WITH gfGetAdr('WAREHOUS' , '' , '' , '' , 5),;
              CPRICECUR  WITH gcBaseCurr                             ,;
              CDUTYCUR   WITH gcBaseCurr                             ,;
              NPRICERAT  WITH 1                                      ,;
              NFCOST1    WITH 0                                      ,;
              NCURRUNIT  WITH 1                                      ,;
              NDCURUNIT  WITH 1                                      ,;
              CPURCODE   WITH lcPurCode
      *B604656,1 AMH 07/12/2001 (End)
      =gfAdd_Info('POSHDR')
      lnTotQty=0
    ENDIF

    lnLineNo=POSHDR.LastLine+1
    
    *---------------------------- BOMVAR LOOP [start]------------------
    SELECT (lcTmpBom)
    DO lpBomLoop  
    SELECT (lcTmpOrder)
    *---------------------------- BOMVAR LOOP [end..]-----------------

    llFilesUpdt=.T.
    
    *-- Update Stydye file [Begain]
    =lfUpdStydye()
    *-- Update Stydye file [End..]  
  
    =SEEK(STYLE,'STYLE')
    
    =SEEK('O'+order+STR(lineno,6),'ORDLINE')
    
    SELECT POSLN  
    APPEND BLANK
    REPLACE CSTYTYPE  WITH 'A'                     ,;
            PO        WITH lcAdorNO                ,;
            TRANCD    WITH '1'                     ,;
            LineNo    WITH lnLineNo                ,;
            VENDOR    WITH PADR(laware[lnSware],6) ,;
            STYLE     WITH &lcTmpOrder..STYLE      ,;
            SCALE     WITH &lcTmpOrder..SCALE      ,;
            PREPAK    WITH &lcTmpOrder..PREPAK     ,;
            PPQTY     WITH &lcTmpOrder..PPQTY
    FOR lnX= 1 TO 8
      Z=STR(lnX,1)
      REPLACE QTY&Z   WITH &lcTmpOrder..PIK&Z      ,;
              ORD&Z   WITH &lcTmpOrder..PIK&Z
    ENDFOR

    REPLACE TOTQTY    WITH &lcTmpOrder..TOTPIK     ,;
            TOTORD    WITH &lcTmpOrder..TOTPIK     ,;
            SHIPNO    WITH &lcTmpOrder..PIKTKT     ,;  
            NCOST1    WITH STYLE.Ave_Cost          ,;
            STORE     WITH ORDHDR.STORE            ,;
            CWARECODE WITH PADR(laware[lnTware],6) ,;
            NECOST1   WITH STYLE.Ave_Cost
    =gfAdd_Info('POSLN')
    lnTotQty=lnTotQty+&lcTmpOrder..TOTPIK
    lnNicost1=NCOST1*&lcTmpOrder..TOTPIK
    REPLACE POSHDR.NICOST1  WITH POSHDR.NICOST1+lnNicost1 ,;
            POSHDR.NFCOST1  WITH POSHDR.NFCOST1+lnNicost1 ,;  
            POSHDR.LastLine WITH lnLineNo 
    lnNicost1= 0 
    
    *--------------- UPDATE ORDLINE & ORDHDR [start]
    
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      REPLACE ORDLINE.CUT&Z WITH ORDLINE.CUT&Z+&lcTmpOrder..PIK&Z
      *C102244,4 AMH no wip in creating adornment order [Start]
      *REPLACE STYLE.WIP&Z   WITH STYLE.WIP&Z+&lcTmpOrder..PIK&Z
      *C102244,4 AMH [End]
      REPLACE STYLE.NWO&Z   WITH STYLE.NWO&Z+&lcTmpOrder..PIK&Z
    ENDFOR 
    REPLACE ORDLINE.TOTCUT WITH ORDLINE.TOTCUT+&lcTmpOrder..TOTPIK
    REPLACE ORDHDR.TOTCUT  WITH ORDHDR.TOTCUT+&lcTmpOrder..TOTPIK
    *C102244,4 AMH no wip in creating adornment order [Start]
    *REPLACE STYLE.TOTWIP   WITH STYLE.TOTWIP+&lcTmpOrder..TOTPIK
    *C102244,4 AMH [End]
    REPLACE STYLE.NTOTWO   WITH STYLE.NTOTWO+&lcTmpOrder..TOTPIK

    *--------------- UPDATE ORDLINE & ORDHDR [end..]
  
    *------------------ UPDATE CUTPICK [start]
    SELECT (lcTmpOrder)
    SELECT CUTPICK
    APPEND BLANK
           
    REPLACE CTKTNO WITH lcAdorNO ,;
            TRANCD WITH '2'      ,;
            ORDER  WITH &lcTmpOrder..ORDER      ,; 
            CORDLINE WITH STR(&lcTmpOrder..LINENO,6)      ,; 
            CTKTLINENO WITH STR(lnLineNo,6) ,;
            STYLE WITH &lcTmpOrder..STYLE      
    
    *B804480,1 AMH save the piktkt in cutpick file [Start]
    
    *B605879,1 AMH Use custom field cPikTkt instead of piktkt 
    *B605879,1     field in cutpick file [Start]
    *REPLACE PIKTKT WITH EVALUATE(lcTmpOrder+'.PIKTKT')
    REPLACE CPIKTKT WITH EVALUATE(lcTmpOrder+'.PIKTKT')
    *B605879,1 AMH [End]
    
    *B804480,1 AMH [End]
    
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      REPLACE QTY&Z WITH &lcTmpOrder..PIK&Z
    ENDFOR
    REPLACE TOTQTY WITH &lcTmpOrder..TOTPIK
    =gfAdd_Info('CUTPICK')
    *--------------------- UPDATE CUTPICK [end..]    
  ENDIF
  SELECT (lcTmpOrder)
ENDSCAN

*-----------------------    Main Loop [end..]    ----------------------

SELECT POSHDR
IF !EMPTY(lcPikTkt) .AND. gfObj_Lock(.T.)
  REPLACE NSTYORDER WITH lnTotQty , OPEN WITH lnTotQty 
  =gfObj_Lock(.F.)      
ENDIF

*-- XXX is saved as XXX
IF llBomUpdt OR llFilesUpdt
  =gfModalGen("TRM000000B00000","DIALOG",'','','Generate adornment orders from '+;
                                               lcFirst+' to '+lcLast+'.')
ENDIF

SELECT (lcTmpOrder)        
ZAP
SET RELATION OFF INTO (lcTmpBom)
SET RELATION OFF INTO Piktkt 
SELECT (lcTmpBom)
ZAP
SELECT (lcTmpOrder) 
ZAP

*!************************************************************************
*! Name      : lpBomLoop
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   : 
*!************************************************************************
*! Calls     : None
*!************************************************************************
*! Passed Parameters  :  None
*!************************************************************************
*! Returns            :  None
*!************************************************************************
*! Example            : DO lpBomLoop  
*!************************************************************************
*
PROCEDURE lpBomLoop
SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)="SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6)
  IF LLSEL
    llBomUpdt=IIF(llBomUpdt,llBomUpdt,.T.)      
    **-- Update BOMVAR file [start]
    SCATTER MEMVAR MEMO
    m.Cidtype  = 'AD'
    m.Ccost_id = lcAdorNO
    m.Lineno   = lnLineNo
    INSERT INTO BOMVAR FROM MEMVAR
    =gfAdd_Info('BOMVAR')
    **-- Update BOMVAR file [end..]    
  ENDIF
ENDSCAN
SELECT THREADCL
SEEK 'SO' + EVALUATE(lcTmpOrder+'.order') + STR(EVALUATE(lcTmpOrder+'.lineno'),6)
SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6)+cDsgnCode+cStylePos+cThreadCl =;
                "SO" + &lcTmpOrder..order + STR(&lcTmpOrder..lineno,6)
  SCATTER MEMVAR MEMO
  lnRecNo = RECNO()
  m.Cidtype  = 'AD'
  m.Ccost_id = lcAdorNO
  m.Lineno   = lnLineNo
  INSERT INTO THREADCL FROM MEMVAR
  *=gfAdd_Info('THREADCL')
  GOTO lnRecNo
ENDSCAN

*!***************************************************************************
*! Name      : lfUpdStydye                                                  
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   :                                                               
*!***************************************************************************
*! Calls     : None                                                         
*!***************************************************************************
*! Passed Parameters  :  None                                               
*!***************************************************************************
*! Returns            :  None                                               
*!***************************************************************************
*! Example            :                                                     
*!***************************************************************************
*
FUNCTION lfUpdStydye
PRIVATE lcAlias
lcAlias=SELECT()
IF !SEEK(&lcTmpOrder..STYLE+PADR(laware[lnSware],6)+SPACE(10),'STYDYE')
  WAIT "Assigning Style"+ALLTRIM(&lcTmpOrder..STYLE)+" to warehouse "+PADR(laware[lnSware],6) WINDOW NOWAIT
  DO gpAdStyWar WITH &lcTmpOrder..STYLE,SPACE(10),PADR(laware[lnSware],6)
  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnSware],6)+SPACE(10),'STYDYE')
  WAIT CLEAR
ENDIF

IF !SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')
  WAIT "Assigning Style"+ALLTRIM(&lcTmpOrder..STYLE)+" to warehouse "+PADR(laware[lnTware],6) WINDOW NOWAIT
  DO gpAdStyWar WITH &lcTmpOrder..STYLE,SPACE(10),PADR(laware[lnTware],6)
  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')
  WAIT CLEAR
ENDIF
*C102244,4 AMH no wip in creating adornment order & don't change allocated Qty [Start]
FOR lnX= 1 TO 8
  Z=STR(lnX,1)
*  REPLACE STYDYE.WIP&Z WITH STYDYE.WIP&Z + PIK&Z
  REPLACE STYDYE.NWO&Z WITH STYDYE.NWO&Z + PIK&Z
ENDFOR
*REPLACE STYDYE.TOTWIP WITH STYDYE.TOTWIP + TOTPIK
REPLACE STYDYE.NTOTWO WITH STYDYE.NTOTWO + TOTPIK

*IF ORDLINE.CWARECODE <> PADR(laware[lnTware],6)
*  SELECT STYDYE
*  =SEEK(&lcTmpOrder..STYLE+ORDLINE.CWARECODE+SPACE(10),'STYDYE')  
*  =RLOCK()
*  FOR lnX= 1 TO 8
*    Z=STR(lnX,1)
*    REPLACE ALO&Z WITH ALO&Z - &lcTmpOrder..PIK&Z
*  ENDFOR
*  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
*  UNLOCK
*  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')  
*  =RLOCK()
*  FOR lnX= 1 TO 8
*    Z=STR(lnX,1)
*    REPLACE ALO&Z WITH ALO&Z + &lcTmpOrder..PIK&Z
*  ENDFOR
*  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
*  UNLOCK
*ENDIF
*C102244,4 AMH [End]
SELECT (lcAlias)

*!***************************************************************************
*! Name      : lfLoop
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/23/2001
*! Purpose   :                                                               
*!***************************************************************************
*! Calls     : None                                                         
*!***************************************************************************
*! Passed Parameters  :  None                                               
*!***************************************************************************
*! Returns            :  None                                               
*!***************************************************************************
*! Example            :                                                     
*!***************************************************************************
*
FUNCTION lfLoop

PRIVATE lnAlias
lnAlias = SELECT(0)
IF SEEK('2'+ORDER+STR(LINENO,6),'CUTPICK')
  SELECT CUTPICK
  *B804480,1 AMH Scan only for the current piktkt [Start]
  *SCAN REST WHILE TranCd+Order+cOrdLine = '2'+ORDLINE.ORDER+STR(ORDLINE.LINENO,6)
  
  *B605879,1 AMH Use custom field cPikTkt instead of piktkt field in 
  *B605879,1     cutpick file [Start]
  *SCAN REST WHILE TranCd+Order+cOrdLine = '2'+ORDLINE.ORDER+STR(ORDLINE.LINENO,6);
            FOR !EMPTY(PIKTKT) AND PIKTKT = ORDLINE.PIKTKT
  SCAN REST WHILE TranCd+Order+cOrdLine = '2'+ORDLINE.ORDER+STR(ORDLINE.LINENO,6);
            FOR !EMPTY(CPIKTKT) AND CPIKTKT = ORDLINE.PIKTKT
  *B605879,1 AMH [End]
  
  *B804480,1 AMH [End]
  
    IF SEEK('A'+CTKTNO,'POSHDR')
      RETURN .T.
    ENDIF
  ENDSCAN
ENDIF
SELECT (lnAlias)
RETURN .F.