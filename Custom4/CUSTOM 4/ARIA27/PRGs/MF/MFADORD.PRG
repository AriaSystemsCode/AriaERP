*!**************************************************************************
*! Program file        : MFADORD.PRG
*! Program description : Adornment Production Order Planning Tool .
*! For screen          : MFADORD.SPR
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : Manufacturing - (MF)
*! Developer Name      : Walid Abou El-Magd (WAM) Due to E#301290,1
*!**************************************************************************
*! Calls               : MFADORD.SPX & MFADOR.SPX
*!**************************************************************************
*! Passed Parameters   : None.
*!**************************************************************************
*! Example             : 
*!**************************************************************************
*! Modification        :
*!**************************************************************************
*!E#301290,1
*!B603205,1 Walid Abou El-Magd (WAM) 10/12/1999 Fix the bug Alias not Found.
*!B603237,1 TAK 11/01/99 Added a custom process for Fresh Produce to generate
*!B603237,1              pick tickets with an adornment order.
*B603318,1 TAK 12/04/99  Don't reduce the stock from target location, and clear releation.
*C101929,1 SSH 06/09/00  Generate separate sequance no for adornment order.
*C000000,1 TAK 10/15/00 (TTTT) Added season to adornments detail browse.
*C102797,1 AMH 02/05/2003 Change the cDsgnCode field to be 8 Chr.
*!**************************************************************************

EXTERNAL ARRAY laScrMode,laDefProc
DIMENSION laType[2,2]
DIMENSION laWare[1]
*B603205,1 add new variables[Start]
*-- llSchedul : To control the appearance Scheduled in the browes .T. if lnExist > 0
*-- lnExist   : the position of the event "SCHEDUL" in the array laEvntTrig
*B603205,1 add two new variables[End..]
*-- Inetialize all logic variables

 
*--laDefProc[9]  = .F.   : Force the local save   proc.  (lpSavScr)
*--laDefProc[10] = .F.   : Force the local cancel proc. (lpClsScr)

STORE .F. TO llNoShow,laScrMode,laDefProc[9],laScrMode,laDefProc[10],;
             llLinkToGl,llbrowse,glFromBrow,llAlowNew,llSchedul
STORE .T. TO laScrMode[1]
lcPassType = 'A'


*-- Inetialize all string variables

STORE ''  TO lcWinCh1,lcWinCh2,lcWinCh3,lcOldVal,lcTmpDsgn,lcBrwFlds,;
             laType,laWare,lcTmpOrder,lcTmpBom,lcTmpBom2,lcTrgAudit,lcGlLink
STORE SPACE(6)  TO lcHdrOrder , lcPosHdrNo 
lcBrowTitl = 'Design Lines'
lcOrdTitel = 'Order Lines'
lcDbSelPrm = 'Se\<lect'
lcObSelPrm = 'Se\<lect'
lcForExp   = [STATUS='O']

*-- Inetialize all State control variables

STORE 'DISABLE' TO lcSelAllSt,lcSelNonSt,lcStatus,lcOSelAllSt,lcOSelNonSt

*-- Inetialize all numeric variables             

*--lnSelRec: Variable to hold the number of selected records of design tmp file

STORE 0 TO lnSelRec  , lnSware    , lnTware,lnExist
STORE 1 TO lnBrRecNo , lnOrMarker , lnMarker 

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
*B603205,1 Initialize the  two new variables[Start]
*B603237,1 Check for existing of a custom process.
lnExist = ASCAN(laEvntTrig,PADR('SCHEDUL',10))
llSchedul=IIF(lnExist > 0 ,.T.,llSchedul)
*B603237,1 End.
*B603205,1 Initialize the  two new variables[End..]

*-- Get all neede memory variables [START]
DIMENSION laSetups[12,2]
laSetups[1,1]  = 'M_WareHouse'
laSetups[2,1]  = 'M_BOMVAR'       
laSetups[3,1]  = 'M_AOVW'
laSetups[4,1]  = 'M_Link_GL'


*-- Save Setup Parameters in memvar [Begin]
laSetups[7,1] = "M_MAAP"
laSetups[8,1] = "M_CLIR"
laSetups[9,1] = "M_BLANKS"
laSetups[10,1] = "M_DESIGNS"
laSetups[11,1] = "M_NMDROPS"
laSetups[12,1] = "M_DESDROPS"


=gfGetMemVar(@laSetups,gcAct_Comp)

llWareHous  = (laSetups[1,2] ='Y')
ldVisWind   = laSetups[3,2]  
llLinkToGl  = (laSetups[4,2] ='Y')


lnCmpMnAlo = laSetups[7,2]  && Company Min. Allowable allocation Percentage.
llCmpRatio = laSetups[8,2]  && Company Line item ratio. 
lnBlankBk  = laSetups[9,2]  && Blanks Min. Back Order Quantity.
lnDesignBk = laSetups[10,2]  && Designs only Min. Back Order Quantity.
lnNmDropBk = laSetups[11,2]  && Name Drops only Min. Back Order Quantity.
lnDesDrpBk = laSetups[12,2]  && Designs and Name Drops Min. Back Order Quantity.
*-- Save Setup Parameters in memvar [End  ]

*-- Get all neede memory variables [END..]

IF !llWareHous
  *-- Can't generate adornment order for single warehous company.
  =gfModalGen('TRM38178B42000','ALERT')  
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
  lcTmpDsgn  = gfTempName()
  lcTmpOrder = gfTempName()              
  lcTmpBom   = gfTempName()              
  lcTmpBom2  = gfTempName()                
  lcOrdCh0   = gfTempName()
  lcOrdCh1   = gfTempName()
  lcOrdCh2   = gfTempName()  
  *B603237,1 Added piktkt temp files name.
  lcTmpGenPk = gfTempName()  && Temporary File Name.
  lcOrdUniqu = gfTempName()  && Unique Tag for Orders.
  lcTmpPkTk  = gfTempName()  && piktkts generated in this session.
  *B603237,1 End.
  
  *-- Creat the temp files .
  =lfCrtTmp()  

  =gfOpenFile(gcDataDir+'POSLN'   ,'POSLN'   ,'sh')
  =gfOpenFile(gcDataDir+"CUTPICK","CUTPICK",'SH')
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

DO (gcScrDir+gcWinAppl+"\MFADORD.SPX")

POP KEY
RELEASE WINDOW (lcBrowTitl)
RELEASE BAR 099 OF P01PU01
RELEASE BAR 100 OF P01PU01
**-------------------------- Normal Quitting
IF glQuitting
  *-- Deactivate the filters
  *-- Erase tmp file if it was created
  IF USED(lcTmpDsgn)
    USE IN (lcTmpDsgn)
    ERASE (gcWorkDir+lcTmpDsgn+'.DBF')
    ERASE (gcWorkDir+lcTmpDsgn+'.CDX')
  ENDIF  
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
  *B603237,1 Close temp files if opend.
  IF USED(lcTmpGenPk)
    USE IN (lcTmpGenPk)
    ERASE &gcWorkdir.&lcTmpGenPk..DBF          && Erase the Temp file.
    ERASE &gcWorkdir.&lcTmpGenPk..CDX          && Erase the Temp file.
    ERASE &gcWorkdir.&lcTmpGenPk..FPT          && Erase the Temp file.
  ENDIF
  IF USED(lcTmpPkTk)
    USE IN (lcTmpPkTk)
    ERASE &gcWorkdir.&lcTmpPkTk..DBF          && Erase the Temp file.
    ERASE &gcWorkdir.&lcTmpPkTk..CDX          && Erase the Temp file.
  ENDIF
  *B603237,1 End.
ENDIF  


*!*************************************************************
*! Name               : lpShow
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
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
SELECT(lcTmpDsgn)
DO CASE
  *-- S E L E C T  M O D E.
  CASE laScrMode[1]
  
    STORE gdSysDate+ldVisWind TO ldLOSSDate 
    STORE gdSysDate TO ldStrtDate
    STORE 'DISABLE' TO lcSelAllSt,lcSelNonSt,lcStatus,lcOSelAllSt,lcOSelNonSt
    STORE 0 TO lnType,lnSWare,lnTWare
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
    IF laType[lnType,2]='A'
      SHOW GET lnSware  DISABLE
      SHOW GET lnTware  DISABLE
      SHOW GET ibGlLink DISABLE
      SHOW GET lcGlLink DISABLE
    ELSE
      SHOW GET lnSware  ENABLE
      SHOW GET lnTware  ENABLE
      SHOW GET ibGlLink ENABLE
      SHOW GET lcGlLink ENABLE
    ENDIF
    lcStatus='ENABLE'
ENDCASE
SHOW GET pbDSelect &lcStatus
SHOW GET pbDInvert &lcStatus
SHOW GET pbOrdDet  &lcStatus

SHOW GET pbDSelAll &lcSelAllSt
SHOW GET pbDSelNon &lcSelNonSt


=lfBrow() AND lfwBrow()
*:*************************************************************
*! Name     : lfBrow
*! Developer: Walid Abou El-Magd 
*! Date     : 07/01/1999               
*! Purpose  : Activate screen browse.
*:*************************************************************
FUNCTION lfBrow

SELECT (lcTmpDsgn)

IF !EMPTY(lcBrowTitl) AND WEXIST(lcBrowTitl)
  RELEASE WINDOW (lcBrowTitl)
ENDIF

lcBrwFlds =  [cMarker = IIF(lnMarker = RECNO(lcTmpDsgn),'>',' ') :H=' ',]+;
             [lcSelect=IIF(LLSEL , '»' , ' ') :R :H= '»' ,]+;
             [cdsgncode :15 :H='Design Code',]+;
             [cdsgnname :20 :H='Design Name',]+;
             IIF(llSchedul,[cadsch    :12  :H='Scheduled',],"") +;
             [totqty    :10 :H='Total Qty',]+;
             [totval    :11 :H='Total Value',]+;             
             [Dstrt     :11 :H='Start Date',]+;
             [Dend      :17 :H='Completion Date']

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
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : When valid function for browse.
*!*************************************************************
FUNCTION lfwBrow
lnMarker = RECNO(lcTmpDsgn)
glFromBrow = .T.
=lfPrmCtrl()
SHOW WINDOW (lcBrowTitl) REFRESH SAME
*!*************************************************************
*! Name      : lfvBrow
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999               
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
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
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
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************

FUNCTION lfRcvDate

IF ldLossDate < gdSysDate
  *-- New 
  *-- Date must be greater than XXX .
  =gfModalGen('INM38172B42000','DIALOG',DTOC(gdSysDate))
  ldLossDate=lcOldVal
  _CUROBJ = _CUROBJ
  RETURN
ENDIF  

*!*************************************************************
*! Name      : lfvType
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*!
FUNCTION lfvType
PRIVATE lnAlias     
IF laType[lnType,2]='A'
  lnAlias=SELECT()
  SELECT POSHDR
  SET ORDER TO TAG POSHDR DESCENDING
  IF SEEK('A')
    llFound=.F.
    SCAN REST WHILE CSTYTYPE='A' FOR STATUS $ 'OH'
      llFound=.T.
      lcPosHdrNo=POSHDR.PO
      lnSWare   = ASCAN(laWare,PADR(POSHDR.VENDOR,6),1)
      lnTWare   = ASCAN(laWare,POSHDR.CWARECODE,1)
      IF llLinkTOGl
        lcGlLink  = POSHDR.Link_Code
        SHOW GET lcGlLink
      ENDIF
      SHOW GET lnSWare
      SHOW GET lnTWare        
        
      EXIT
    ENDSCAN
    
    IF !llFound
      *-No adornment order to add to.You have to generate new one.
      =gfModalGen('TRM38179B42000','ALERT')  
      lnType=1
      SHOW GET lnType
    ELSE
      SELECT POSLN
      =SEEK('A'+lcPosHdrNo)
      LOCATE REST WHILE CSTYTYPE+PO = 'A'+lcPosHdrNo FOR TranCd ='6'
      IF FOUND()
        *-Last open adornment order has been issued, You have to generate new one.
        =gfModalGen('TRM38193B42000','ALERT')  
        lnType=1
        SHOW GET lnType
      ENDIF
    ENDIF

  ELSE
    *-No adornment order to add to.You have to generate new one.
    =gfModalGen('TRM38179B42000','ALERT')  
    lnType=1
    SHOW GET lnType
  
  ENDIF
  SELECT (lnAlias)
ENDIF

*B603237,1 Custom collect function.
IF llSchedul
  llAcepSel = .F.
  =gfDoTriger('MFADORD',PADR('FPCOLECT',10))
  IF !llAcepSel
    *-- No records to display
    =gfModalGen('TRM38180B42000','ALERT')  
    _CUROBJ=OBJNUM(ldLOSSDate)
    RETURN
  ENDIF
*B603237,1 End.
ELSE
  IF !lfGetInfo()
    *-- No records to display
    =gfModalGen('TRM38180B42000','ALERT')  
    _CUROBJ=OBJNUM(ldLOSSDate)
    RETURN
  ENDIF

ENDIF
  

*- Code to Automatic detection of Select All , Select None , Restore Defaults
*- Must be here befor mode change .

lcSelAllSt = IIF(lnSelRec = RECCOUNT() , 'DISABLE' , 'ENABLE')
lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')

laScrMode=.F.
laScrMode[4]=.T.
SHOW GETS
*!*************************************************************
*! Name      : lfvLinkCod
*! Developer : Walid Abou El-Magd
*! Date      : 
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
*! Developer : Walid Abou El-Magd
*! Date      : 
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

laType[1,1]='Generate New Adornment Order'
laType[1,2]='G'
laType[2,1]='Add To Existing Adornment Order'
laType[2,2]='A'
lnType=1
SHOW GET lnType ENABLE
SHOW GET lnSware ENABLE
SHOW GET lnTware ENABLE
_CUROBJ=OBJNUM(lnType)      

lnOldTyp = lnType
lnType   = 0  
KEYBOARD '{SPACEBAR}'
lnType   = lnOldTyp

*!*************************************************************
*! Name      : lfvControl
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
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
PARAMETERS lcEvent,llFromBom
PRIVATE llRestore,lcScanExpr
llRestore = IIF(lcEvent = "RESTOR" , .T. , .F. )
*-- Select / UnSelect Record.

IF lcEvent = "SEL_UNSEL"
  =lfSelUnSel(llFromBom)

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
    =lfSelUnSel(llFromBom)
  ENDSCAN  


ENDIF

IF llFromBom
  GO lnOrMarker
  =lfPrmCtrl(.T.)
  lcOSelAllSt =  'ENABLE'
  lcOSelNonSt =  'ENABLE'
  SHOW GET pbOSelAll &lcOSelAllSt
  SHOW GET pbOSelNon &lcOSelNonSt

  SHOW WINDOW (lcOrdTitel) REFRESH SAME

ELSE
  GO lnMarker
  =lfPrmCtrl(.F.)
  lcSelAllSt = IIF(lnSelRec = RECCOUNT() , 'DISABLE' , 'ENABLE')
  lcSelNonSt = IIF(lnSelRec = 0 , 'DISABLE' , 'ENABLE')
  SHOW GET pbDSelAll &lcSelAllSt
  SHOW GET pbDSelNon &lcSelNonSt

  SHOW WINDOW (lcBrowTitl) REFRESH SAME
ENDIF
*!*************************************************************
*! Name      : lfSelUnSel
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
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
PARA llFromBom

REPLACE llSel WITH !llSel


*-- if it is now selected record.
IF llSel
  IF llFromBom
    =lfBomToDsgn()
    
  ELSE
    lnSelRec = lnSelRec + 1
    =lfDsgnToBom(.T.)
  ENDIF

ELSE  && else if it is now unselected record.
  IF llFromBom
    =lfBomToDsgn()
  ELSE
    lnSelRec = lnSelRec - 1 
    =lfDsgnToBom(.F.)
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfDsgnToBom
*! Developer : Walid Abou El-Magd
*! Date      : 09/07/1997
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
PRIVATE lcForExp,lcOrder,llZeroLine,lnQty
lcForExp = IIF(llAdd,[!llSEL],[llSEL])
SELECT (lcTmpBom)
lcOrder=ORDER(lcTmpBom)
SET ORDER TO TAG (lcTmpBom2)
=SEEK(&lcTmpDsgn..CDSGNCODE)
*-- if in any time through the program cycle this design become with zero qty
*-- due to unselection of its all Bomlines , In this case we have to restore
*-- its qty from bomvar temp if we make it selected record again.
llZeroLine=IIF(&lcTmpDsgn..TOTQTY = 0 ,.T.,.F.)

SCAN REST WHILE CDSGNCODE = &lcTmpDsgn..CDSGNCODE FOR &lcForExp
  IF llAdd
    REPLACE llSel WITH .T.

    *-- if we call this function to make a zero qty line selected .
    
    IF llZeroLine
      REPLACE &lcTmpDsgn..TOTQTY WITH (&lcTmpDsgn..TOTQTY + TOTPIK )
      REPLACE &lcTmpDsgn..TOTVAL WITH (&lcTmpDsgn..TOTQTY * Gros_price )    
    ENDIF
  ELSE
    REPLACE llSel WITH .F.

  ENDIF  
ENDSCAN 
SET ORDER TO TAG &lcOrder
SELECT (lcTmpDsgn)    

*!*************************************************************
*! Name      : lfBomToDsgn
*! Developer : Walid Abou El-Magd
*! Date      : 09/07/1997
*! Purpose   : Reflect the changes in Tmp Bomvar To Tmp Dsgn 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 
*!                     
*!*************************************************************
*! Return      : .T.
*!*************************************************************
*
FUNCTION lfBomToDsgn

IF !&lcTmpDsgn..LLSEL
  REPLACE &lcTmpDsgn..TOTQTY WITH 0
  REPLACE &lcTmpDsgn..TOTVAL WITH 0
ENDIF

lcCalOper  = [IIF(llSEl, 1,-1) *]  && Transaction sign.
lnQuantity = 0
lnValue    = 0
lnQuantity = lnQuantity + (&lcCalOper. TOTPIK )
lnValue    =(lnQuantity * Gros_price)

*-- Send the changes to lcTmpDsgn .
REPLACE &lcTmpDsgn..TOTQTY WITH (&lcTmpDsgn..TOTQTY + lnQuantity )
REPLACE &lcTmpDsgn..TOTVAL WITH (&lcTmpDsgn..TOTVAL + lnValue )    

IF &lcTmpDsgn..TOTQTY > 0 AND !&lcTmpDsgn..LLSEL
  REPLACE &lcTmpDsgn..LLSEL WITH .T.
ENDIF
IF &lcTmpDsgn..TOTQTY = 0
  REPLACE &lcTmpDsgn..LLSEL WITH .F.
ENDIF
*!*************************************************************
*! Name      : lfPrmCtrl
*! Developer : Walid Abou El-Magd
*! Date      : 09/07/1997
*! Purpose   : Function to control the push button Select prompt
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : llFromBom = .T. if called from order screen
*!                               = .F. if called from design screen
*!*************************************************************
*! Return      : .T.
*!*************************************************************
*
FUNCTION lfPrmCtrl
PARA llFromBom
IF laScrMode[4]

  IF !LLSEL
    IF llFromBom 
      lcDbSelPrm = 'Se\<lect'
      SHOW GET pbOSelect ,1 PROMPT lcDbSelPrm &lcStatus
    
    ELSE
      lcDbSelPrm = 'Se\<lect'
      SHOW GET pbDSelect ,1 PROMPT lcDbSelPrm &lcStatus
    ENDIF
  ELSE    
    IF llFromBom
      lcDbSelPrm = 'UnSe\<lect'
      SHOW GET pbOSelect ,1 PROMPT lcDbSelPrm &lcStatus
    
    ELSE
      lcDbSelPrm = 'UnSe\<lect'
      SHOW GET pbDSelect ,1 PROMPT lcDbSelPrm &lcStatus
    ENDIF
  ENDIF   
ENDIF


**--------------------------------------------------------------------------
**----------------- (( Order Lines Detail Section )) -----------------------
**-------------------  Function without traping  ---------------------------
*!*************************************************************
*! Name      : lfvOrdDet
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*!
FUNCTION lfvOrdDet
PRIVATE llFrist
lnQuantity = 0
lnValue = 0
SELECT (lcTmpBom)
SET FILTER TO CDSGNCODE = &lcTmpDsgn..CDSGNCODE
LOCATE                     &&-- To activate the filter
SET RELATION TO 'O'+&lcTmpBom..Ccost_id+STR(&lcTmpBom..lineno,6) INTO (lcTmpOrder)
llFrist = .T. &&-- this variable to control the invisible butt in the screen

ON KEY LABEL ALT+Z ACTIVATE WINDOW (lcOrdTitel)

lcHldTab = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcHldEnt = ON('KEY','ENTER')
lcOldPrm=lcDbSelPrm
ON KEY LABEL TAB    
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER

*--Call Oredr line quantity screen.
DO (gcScrDir+gcWinAppl+"\MFADOR.SPX")
ON KEY LABEL ALT+Z
ON KEY LABEL TAB     &lcHldTab
ON KEY LABEL BACKTAB &lcHldBtb
ON KEY LABEL ENTER   &lcHldEnt
SELECT (lcTmpBom)
SET FILTER TO
SET RELATION TO
=lfBrow() AND lfwBrow()

*!*************************************************************
*! Name      : lfvClose
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Parameters: 
*!*************************************************************
*! Returns   : 
*!*************************************************************
*!
FUNCTION lfvClose
CLEAR READ 
*!*************************************************************
*! Name      : lfDispBrow
*! Developer : Walid Abou El-Magd
*! Date      : 07/20/1999
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfOrdBrow
PRIVATE lcBrwOFlds

SELECT (lcTmpBom)
lnBrRecNo  = RECNO()

lcBrwOFlds = [cMarker = IIF(lnOrMarker = RECNO(lcTmpBom),'>',' ') :H=' ',]+;
             [lcSelect=IIF(LLSEL , '»' , ' ') :R :H= '»' ,]+;
             [Ccost_id :10 :H='Order #',]
*B603237,1 Don't Show the piktkt if custom process was exist
*B603237,1 since piktkt not generated yet and show style insted.
IF llSchedul
  lcBrwOFlds = lcBrwOFlds + [&lcTmpOrder..Style  :20 :H='Style',]
ELSE
*B603237,1 End.
  lcBrwOFlds = lcBrwOFlds + [&lcTmpOrder..Piktkt :10 :H='Piktkt #',]
ENDIF
*TTTT ADD SEASON.
lcBrwOFlds = lcBrwOFlds + [&lcTmpOrder..Season  :9 :H='Season',]+;
             [&lcTmpOrder..Account    :11 :H='Account',]+;
             [stname    :25 :H='Account Name',]+;             
             [&lcTmpOrder..Start     :10 :H='Start',]+;
             [&lcTmpOrder..complete      :10 :H='Complete',]+;
             [&lcTmpOrder..custpo      :15 :H='Customer Po#',]+;
             [&lcTmpOrder..totpik      :7 :H='Totqty']

BROWSE FIELDS &lcBrwOFlds        ;
       WINDOW (lcOrdCh1)         ;
       WHEN lfwOrdBrows()        ;
       VALID :F lfvOrdBrow()     ;
       IN WINDOW Mfador          ;
       LOCK 0                    ;
       NOAPPEND                  ;
       NOCLEAR                   ;
       NODELETE                  ;
       NOWAIT                    ;
       NOEDIT                    ;
       NOMENU                    ;
       SAVE                      ;
       TITLE (lcOrdTitel)
GO TOP
*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwOrdBrows
*! Developer : Walid Abou El-Magd
*! Date      : 07/20/1999
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwOrdBrows
glFromBrow = .T.
lnOrMarker = RECNO(lcTmpBom)
glFromBrow = .T.
=lfPrmCtrl(.T.)
SHOW WINDOW (lcOrdTitel) REFRESH SAME

*!*************************************************************
*! Name      : lfvOrdBrow
*! Developer : Walid Abou El-Magd
*! Date      : 07/20/1999
*! Purpose   : Valid Browse function
*!*************************************************************
*
FUNCTION lfvOrdBrow
IF WONTOP() # (lcOrdTitel)
  glFromBrow = .T.
  = gfStopBrow()
ENDIF

*!*************************************************************
*! Name      : lfOrdShow
*! Developer : Walid Abou El-Magd
*! Date      : 07/20/1999
*! Purpose   : Valid Browse function
*!*************************************************************
*
FUNCTION lfOrdShow
SHOW GET pbOSelect &lcStatus
SHOW GET pbOInvert &lcStatus
SHOW GET pbOSelAll ENABLE
SHOW GET pbOSelNon ENABLE &&-- all selected by default
=lfOrdBrow() 

*!*************************************************************
*! Name      : lfGetInfo
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
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
  IF !BETWEEN(Ordhdr.Start,ldStrtDate,ldLOSSDate) 
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

        IF SEEK(cdsgncode,(lcTmpDsgn))
          *-- Accomulate
          REPLACE &lcTmpDsgn..TOTQTY WITH &lcTmpDsgn..TOTQTY + Ordline.TOTPIK
          REPLACE &lcTmpDsgn..TOTVAL WITH &lcTmpDsgn..TOTVAL + (Ordline.TOTPIK * Ordline.Gros_price)      
          IF &lcTmpDsgn..TOTQTY > ICDESIGN.Nvenadmin
            REPLACE &lcTmpDsgn..llSEL WITH .T.
            lnSelRec = lnSelRec + 1
          ENDIF
          
          IF Ordhdr.Start < &lcTmpDsgn..DSTRT
            REPLACE &lcTmpDsgn..DSTRT WITH Ordhdr.Start
          ENDIF
          IF Ordhdr.Complete < &lcTmpDsgn..DEND
            REPLACE &lcTmpDsgn..DEND WITH Ordhdr.Complete
          ENDIF
      
        ELSE
          *-- Add
          DIME laTemp[10]
          laTemp[1]  = CDSGNCODE
          laTemp[2]  = ICDESIGN.CDSGNNAME
          laTemp[3]  = Ordline.TOTPIK
          laTemp[4]  = laTemp[3] * Ordline.Gros_price
          laTemp[5]  = Ordhdr.Start
          laTemp[6]  = Ordhdr.Complete
          laTemp[7]  = IIF(laTemp[3] > ICDESIGN.Nvenadmin,.T.,.F.)
          laTemp[8]  = laTemp[7]
          laTemp[9]  = ICDESIGN.Cdsgnlink
          *B603205,1 Don't seek in maybe not exist (Customer) file directly [Start]
          *laTemp[10] = IIF(SEEK(CDSGNCODE,'ICADSCH'),'Y','N')
          laTemp[10] = 'N'
          *B603205,1 Don't seek in maybe not exist (Customer) file directly[End..]          
          INSERT INTO (lcTmpDsgn) FROM ARRAY laTemp
          IF laTemp[7]
            lnSelRec = lnSelRec + 1
          ENDIF
          laTemp=SPACE(1)
        ENDIF

      ENDSCAN  &&-- ENDSCAN of bomvar
      SELECT ORDLINE
    ENDIF  &&-- endif !EOF('BOMVAR') 
  ENDSCAN  &&-- ENDSCAN of ORDLINE
  SELECT Piktkt
ENDSCAN &&-- ENDSCAN of Piktkt

SELECT (lcTmpDsgn)
GO TOP
IF EOF() OR BOF()
  RETURN(.F.)
ELSE  
  SCAN FOR llSEL  &&-- scan lcTmpDsgn for selected by default only .
    =lfDsgnToBom(.T.)
  ENDSCAN
  GO TOP
  RETURN(.T.)
ENDIF


*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
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
*------------------ CREATE DESIGN TEMP. FILE -> [BEGIN]

DIMENSION laFileStru[10,4]

laFileStru[1,1] = 'CDSGNCODE'
laFileStru[1,2] = 'C'

*C102797,1 AMH Change the cDsgnCode field to be 8 Chr. [Start]
*laFileStru[1,3] =  6
laFileStru[1,3] =  8
*C102797,1 AMH [End]


laFileStru[2,1] = 'CDSGNNAME'
laFileStru[2,2] = 'C'
laFileStru[2,3] = 30


laFileStru[3,1] = 'TOTQTY'
laFileStru[3,2] = 'N'
laFileStru[3,3] =  6


laFileStru[4,1] = 'TOTVAL'
laFileStru[4,2] = 'N'
laFileStru[4,3] =  6


laFileStru[5,1] = 'DSTRT'
laFileStru[5,2] = 'D'
laFileStru[5,3] =  8


laFileStru[6,1] = 'DEND'
laFileStru[6,2] = 'D'
laFileStru[6,3] =  8


laFileStru[7,1] = 'LLSEL'
laFileStru[7,2] = 'L'
laFileStru[7,3] =  1

laFileStru[8,1] = 'LLDEF'
laFileStru[8,2] = 'L'
laFileStru[8,3] =  1

laFileStru[9,1] = 'CDSGNLINK'
laFileStru[9,2] = 'C'
laFileStru[9,3] =  3

laFileStru[10,1] = 'CADSCH'
laFileStru[10,2] = 'C'
laFileStru[10,3] =  1

STORE 0 TO laFileStru[1,4] , laFileStru[2,4] , laFileStru[3,4] ,;
           laFileStru[4,4] , laFileStru[5,4] , laFileStru[6,4] ,;
           laFileStru[7,4] , laFileStru[8,4] , laFileStru[9,4] ,;
           laFileStru[10,4] 
CREATE TABLE &gcWorkDir.&lcTmpDsgn FROM ARRAY laFileStru
INDEX ON CDSGNCODE TAG (lcTmpDsgn)
*-- CREATE DESIGN TEMP. FILE ->  [END..]

*------------------ CREATE ORDER TEMP. FILE -> [BEGIN]

SELECT ordline
STORE SPACE(1) TO laFileStru
=AFIELDS(laFileStru)
lnFStru = ALEN(laFileStru,1)
*B603237,1 Add a new field , increment array by one.
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

*B603237,1 Add a new field for existing of the line in adornment.
laFileStru[lnFStru+5,1] = 'LEXIST'
laFileStru[lnFStru+5,2] = 'L'
laFileStru[lnFStru+5,3] =  1
laFileStru[lnFStru+5,4] =  0
*B603237,1 End.


CREATE TABLE &gcWorkDir.&lcTmpOrder FROM ARRAY laFileStru
INDEX ON cordtype+order+STR(lineno,6) TAG (lcTmpOrder)
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
INDEX ON CDSGNCODE TAG (lcTmpBom2)
INDEX ON cidtype+ccost_id+STR(lineno,6) TAG (lcTmpBom)


*************
CREATE CURSOR (lcTmpPkTk) (Order  C(6), Store C(8), cPickType C(1),PikTkt C(6))
ZAP
INDEX ON Order + Store + cPickType TAG (lcTmpPkTk) OF ;
         (gcWorkDir+lcTmpPkTk+'.CDX')

*-- Create Temporary File...
DIMENSION laTempStru[1,4]

SELECT ORDLINE
lnFldsCnt =AFIELDS(laTempStru)
*B603237,1 Add a new field , increment array by one.
DIMENSION laTempStru[lnFldsCnt+8,4]

*-- Select / UnSelect Flag Field.
laTempStru[lnFldsCnt+1 , 1] = 'lSelRec'
laTempStru[lnFldsCnt+1 , 2] = 'L'
laTempStru[lnFldsCnt+1 , 3] = 1
laTempStru[lnFldsCnt+1 , 4] = 0

*-- Field Detect if at least one size has 40% or greater Allocated .
laTempStru[lnFldsCnt+2 , 1] = 'lOneHas40'
laTempStru[lnFldsCnt+2 , 2] = 'L'
laTempStru[lnFldsCnt+2 , 3] = 1
laTempStru[lnFldsCnt+2 , 4] = 0

*-- Field Detect number of sizes have 40% or greater Allocated.
laTempStru[lnFldsCnt+3 , 1] = 'nTot40'
laTempStru[lnFldsCnt+3 , 2] = 'N'
laTempStru[lnFldsCnt+3 , 3] = 3
laTempStru[lnFldsCnt+3 , 4] = 0

*-- Field Detect if 3 sizes have 40% is in Consecutive.
laTempStru[lnFldsCnt+4 , 1] = 'mConSecTv3'
laTempStru[lnFldsCnt+4 , 2] = 'M'
laTempStru[lnFldsCnt+4 , 3] = 10
laTempStru[lnFldsCnt+4 , 4] = 0

*-- Field Hold Still Allocated quantity.
laTempStru[lnFldsCnt+5 , 1] = 'nStillAlo'
laTempStru[lnFldsCnt+5 , 2] = 'N'
laTempStru[lnFldsCnt+5 , 3] = 7
laTempStru[lnFldsCnt+5 , 4] = 0

*-- Field Hold Adorned / Blank .
laTempStru[lnFldsCnt+6 , 1] = 'cPickType'
laTempStru[lnFldsCnt+6 , 2] = 'C'
laTempStru[lnFldsCnt+6 , 3] = 1
laTempStru[lnFldsCnt+6 , 4] = 0

*-- Field Hold Net Open Quantity.
laTempStru[lnFldsCnt+7 , 1] = 'nNetOpen'
laTempStru[lnFldsCnt+7 , 2] = 'N'
laTempStru[lnFldsCnt+7 , 3] = 7
laTempStru[lnFldsCnt+7 , 4] = 0


*B603237,1 Add a new field for existing of the line in adornment.
laTempStru[lnFldsCnt+8 , 1] = 'LEXIST'
laTempStru[lnFldsCnt+8 , 2] = 'L'
laTempStru[lnFldsCnt+8 , 3] = 1
laTempStru[lnFldsCnt+8 , 4] = 0
*B603237,1 End.

CREATE TABLE (gcWorkDir+lcTmpGenPk) FROM ARRAY laTempStru
ZAP

INDEX ON Order + cPickType FOR cPickType = lcPassType TAG (lcOrdUniqu) OF ;
                                       (gcWorkDir+lcTmpGenPk+'.CDX') UNIQUE
INDEX ON Order+Store+cPickType+STR(LineNo,6) FOR cPickType = lcPassType TAG (lcTmpGenPk) OF ;
         (gcWorkDir+lcTmpGenPk+'.CDX')
*-- MAB 10/16/1999 Filter on Specific pick type only. [End  ]

*------------------ CREATE BOMVAR TEMP. FILE -> [END..]
SELECT (lcTmpDsgn) 


**--------------------------------------------------------------------------
**------------------------((Trapping Section )) ----------------------------
**------------------------ For the two browses -----------------------------
*!*************************************************************
*! Name               : lpTrap
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
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
**--------------------------------------------------------------------------
**-----------------------    1- Design Brow     ----------------------------
*!*************************************************************
*! Name               : lfDeact
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
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
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
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

**--------------------------------------------------------------------------
**----------------------- 2- Order detail brow -----------------------------

*!*************************************************************
*! Name               : lfDeactOrd
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
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
FUNCTION lfDeactOrd
*-- Trap keys in this function
IF WONTOP()=(lcOrdTitel)
  glFromBrow = .T.
  ON KEY LABEL TAB        DO lpTrap WITH (lcOrdCh2),'TAB'
  ON KEY LABEL ESCAPE     LLDUMMY1=.T.
ENDIF

*!*************************************************************
*! Name               : lfActOrd
*! Developer          : Walid Abou El-Magd
*! Date               : 07/01/1999
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
FUNCTION lfActOrd
*-- Untrap keys in this function
IF WONTOP() # (lcOrdTitel)  
  IF glFromBrow
    ON KEY LABEL TAB
    ON KEY LABEL BACKTAB
    ON KEY LABEL ESC DO gfEscap
    =gfStopBrow()
  ENDIF
ENDIF  



*!*************************************************************
*! Name    : lpClsScr
*! Developer: Timour A. K. 
*! Date     : 10/10/97               
*! Purpose : Cancel/Close entery.
*!*************************************************************
PROCEDURE lpClsScr
STORE 'DISABLE' TO lcSelAllSt,lcSelNonSt
STORE 'DISABLE' TO lcOSelAllSt,lcOSelNonSt
lnType=0
SELECT (lcTmpBom)
ZAP
SELECT (lcTmpOrder)
ZAP
SELECT (lcTmpDsgn)
ZAP



*-------------------------------------------------------------------------
*-------------------------- ( Save section ) -----------------------------
*!************************************************************************
*! Name      : lpSavScr
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
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


PRIVATE lnCrRec , lnLastLine , lcAdorNO , lnLineNo , lnTotQty , lnNicost1 , lnNicost2 ,;
        lnCutpikPos,llSame,lcOrdKey,llFilesUpdt,llBomUpdt,lnLineAdj,llIgnorBom,llIgnorFil

STORE .F. TO llFilesUpdt,llBomUpdt,llIgnorBom,llIgnorFil
*-- Check for any selected lines & Source location = target location [begin]

SELECT (lcTmpDsgn)
lnCrRec =  IIF(EOF(),0,RECNO())
LOCATE FOR llSEL

IF !FOUND()
  *--No [Receiving Lines] was done,Cannot update.
    = gfModalGen('INM34061B42000','DIALOG','design selection')
  STORE .F. TO llShow,llCSave
  
  *-- Check of phisical record before go to it.
  IF BETWEEN(lnCrRec,1,RECCOUNT())
    GOTO lnCrRec
  ENDIF
  RETURN .F.
ENDIF

IF lnSware = 0
  *--You have to select a XXX  
  = gfModalGen('TRM38183B42000','DIALOG','source location.')
  STORE .F. TO llShow,llCSave
  RETURN .F.
  _CUROBJ=OBJNUM(lnSware)        
ENDIF	

IF lnTware =0
  *--You have to select a XXX 
  = gfModalGen('TRM38183B42000','DIALOG','target location.')
  STORE .F. TO llShow,llCSave
  RETURN .F.  
  _CUROBJ=OBJNUM(lnTware)          
ENDIF	

IF lnSware = lnTware
  *--Source location is the same as target location , Cannot update
  = gfModalGen('TRM38181B42000','DIALOG')
  STORE .F. TO llShow,llCSave
  RETURN .F.  
ENDIF	

IF llLinkToGl AND EMPTY(lcGlLink) 
  *--You have to select a XXX 
  = gfModalGen('TRM38183B42000','DIALOG','a GL Link Code.')
  STORE .F. TO llShow,llCSave
  RETURN .F.  
  _CUROBJ=OBJNUM(lcGlLink)          
ENDIF
*-- Check for selected lines & Source location = target location [end..]



=gfOpenFile(gcDataDir+'STYLE'   ,'STYLE'   ,'sh')
=gfOpenFile(gcDataDir+'STYDYE'  ,'STYDYE'  ,'sh')
=gfOpenFile(gcDataDir+'CUTPICK'  ,'CUTORD' ,'sh')   &&--trancd+order+cordline
=gfOpenFile(gcDataDir+'WAREHOUS','WAREHOUS','sh')


*--Check selected records.
SELECT (lcTmpBom)
SCAN FOR llSEL
  lcOrdKey='O'+Ccost_id+STR(lineno,6)
  SELECT (lcTmpOrder)
  =SEEK(lcOrdKey)
  IF !llSEL
    REPLACE llSEL WITH .T.
  ENDIF
  SELECT (lcTmpBom)
ENDSCAN


*B603237,1 Custom process for generation of piktkt.
*--Genrate Pick tickets custom Process.
IF ASCAN(laEvntTrig,PADR('FPGENPIK',10)) > 0
  =gfDoTriger('MFADORD',PADR('FPGENPIK',10))
ENDIF
*B603237,1 End.


DO CASE

  CASE laType[lnType,2]='G'
    =SEEK('O'+lcHdrOrder,'ORDHDR')
    =SEEK(PADR(laware[lnSware],6),'WAREHOUS')
    lcAdorNO=SPACE(6)        
    SELECT POSHDR
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
            NICOST2    WITH 0                       ,;
            NSTYORDER  WITH 0                       ,;
            OPEN       WITH 0                       ,;
            LINK_CODE  WITH lcGlLink
    REPLACE SHPNAME    WITH WAREHOUS.cdesc          ,;
            COUTADDR1  WITH WAREHOUS.CADDRESS1      ,;
            COUTADDR2  WITH WAREHOUS.CADDRESS2      ,;
            COUTADDR3  WITH WAREHOUS.CADDRESS3      ,;
            COUTADDR4  WITH WAREHOUS.CADDRESS4      ,;
            COUTADDR5  WITH WAREHOUS.CADDRESS5      ,;          
            CPRICECUR  WITH gcBaseCurr              ,;
            CDUTYCUR   WITH gcBaseCurr              ,;            
            NPRICERAT  WITH 1                       ,;
            NFCOST1    WITH 0                       ,;
            NFCOST2    WITH 0                       ,;
            NCURRUNIT  WITH 1                       ,;
            NDCURUNIT  WITH 1            
    =gfAdd_Info('POSHDR')
    lnTotQty=0

  CASE laType[lnType,2]='A'
    *-- Select the poshdr record which you will add to .
    =SEEK('A'+lcPosHdrNo,'POSHDR')
    lcAdorNO=POSHDR.PO  &&-- this will be our adornment order no in this case
    lnTotQty=POSHDR.NSTYORDER
ENDCASE
*-- The main purpose of the next loop is to make a selected subset from (lcTmpOrder)


SELECT PIKTKT
SET ORDER TO TAG Ordpik
SELECT (lcTmpOrder)
SET RELATION TO "SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6) INTO (lcTmpBom)
*-- The next relation makes the tmp order lines file see all related records in all files
SET RELATION TO order + piktkt INTO Piktkt ADDITIVE
GO TOP
*-- Vriables that will be used in this loop

****-- Accomulators

*-- lnTotQty    
*--             : This variable will be used to update POSHDR.NSTYORDER & POSHDR.OPEN
*--             : This variable will be initialized with zero if new poshdr record
*--             : is created otherwise will be initialized with POSHDR.NSTYORDER
*--             : This variable will not be reset inside the main loop
*--             : its equation : [lnTotQty = lnTotQty+&lcTmpOrder..TOTPIK]

*-- lnNcost2    : This variable will be used to update &lcTmpOrder..Ncost2
*--             : which in turn will be used to update posln.Ncost2
*--             : It is the summation of bomvar.TOTCOST for all selected records
*--             : in bomvar related to this line in lcTmpOrder
*--             : This variable will be reset inside the main loop per record
*--             : Its equation : [lnNcost2=lnNcost2+TOTCOST ] in lpBomLoop  

*-- lnNicost1   : This variable will be used to update POSHDR.Nicost1 & POSHDR.NFcost1
*--             : This variable will be initialized with zero and will be reset 
*--             : inside the main loop per record , POSHDR will be accomulated with this 
*--             : through the main loop
*--             : Its equation : [lnNicost1=NCOST1*&lcTmpOrder..TOTPIK] in the main loop 

*-- lnNicost2   : Same as lnNicost2 but it will be used to update POSHDR.Nicost2 & POSHDR.NFcost2
*--             : Its equation : [lnNicost2=NCOST2*&lcTmpOrder..TOTPIK] in the main loop 

*-- lnLineNo    : This variable will be used to update POSLN.Lineno and POSHDR.Lastline
*--             : It will be initialized to zero if new poshdr record
*--             : is created otherwise will be initialized with POSHDR.Lastline
*--             : It will be incremented through the main loop only if this line
*--             : is a new for this adornment order

****-- Logic and indicators

*-- lnCutpikPos : This variable will be used as indicator for existance of this line
*--             : befor in cutpick file ,  which means it was added to the current odornment order
*--             : But till now , we still not sure if this line has a new piktkt number
*--             : or it carry the old one
*--             : Its equation : [lnCutpikPos=RECNO('CUTPICK') ]

*-- llSame      : If .T. it means this line has its old piktkt number

*-- lcTrgAudit  : This variable will be used to triger gfDoTriger
*--             : each time  ORDER+PIKTKT is changed , its initial value is SPACE(0)

*-- POSLNPOS    : It is a field in lcTmpOrder will be used to save the recordno of the posln record 
*--             : if this line is an old one with different piktkt number
*--             : I did so since I will need to go to this record later
*--             : so there is no need to SEEK() it again .
*--             : Its initial value is zero , it means that this is a new line for this order
*--             : This value may be changed in lpChkLine .

*-----------------------    Main Loop [start]    ----------------------


*B603237,1 Custom process for Updating an adornment order.
*--Custom Process to update adornment.
IF ASCAN(laEvntTrig,PADR('UPDADORN',10)) > 0
  =gfDoTriger('MFADORD',PADR('UPDADORN',10))
  RETURN
ENDIF
*B603237,1 End.



*--Standard process.
SCAN       &&-- (lcTmpOrder)

  STORE .F. TO llSame
  STORE 0 TO lnCutpikPos,lnLineAdj
  
  IF llSEL

    DO lpChkLine

    *-- Increment the line number if it is a new line in this adornment order 
    *-- Else use the line number of the line you add to .
    *-- the main idea in this process is that we replace POSHDR.LastLine
    *-- through the loop not in the end of loop .
    IF lnCutpikPos=0
      lnLineNo=POSHDR.LastLine+1
    ELSE
      lnLineNo=lnLineAdj
    ENDIF
    
    
    *---------------------------- BOMVAR LOOP [start]------------------
    lnNcost2=0
    SELECT (lcTmpBom)
    DO lpBomLoop  
    SELECT (lcTmpOrder)
    *---------------------------- BOMVAR LOOP [end..]-----------------

    *-- If this line is exist befor in this adornment order do nothing . [Add mode]
    *-- Or this line is exist befor in any adornment order with same piktktno [New mode]
    
    IF llSame
      llSame=.F.
      llIgnorFil=IIF(llIgnorFil,llIgnorFil,.T.)
      LOOP
    ENDIF      
    
    llFilesUpdt=.T.
    
    *-- Generate a new sequence number if you are sure that you will update.
    IF laType[lnType,2]='G' AND EMPTY(lcAdorNO)
      *C101929,1 SSH 06/09/00  [Start Generate separate sequance no for adornment order.
      *lcAdorNO=gfSequence('PO')
      lcAdorNO=gfSequence('POAD')
      *C101929,1 SSH 06/09/00  END]
      SELECT POSHDR
      REPLACE PO WITH lcAdorNO                
      SELECT (lcTmpOrder)
    ENDIF
    
    IF lcTrgAudit # ORDER+PIKTKT 
      lcTrgAudit=ORDER+PIKTKT
      *-- Audit Trial.
      IF ASCAN(laEvntTrig,PADR("ADRN_ORD",10)) <> 0
        =gfDoTriger('MFADORD',PADR("ADRN_ORD",10))
      ENDIF
    ENDIF
    
    REPLACE &lcTmpOrder..Ncost2 WITH lnNcost2


    *-- Update Stydye file [Begain]
    =lfUpdStydye()
    *-- Update Stydye file [End..]  
  
    =SEEK(STYLE,'STYLE')
    
    =SEEK('O'+order+STR(lineno,6),'ORDLINE')
    
    *-- it will see the right record in ORDHDR due to the relation 
    IF POSLNPOS=0  &&-- new line not exist in this order befor .
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
              NCOST1    WITH STYLE.TOTCOST           ,;
              NCOST2    WITH &lcTmpOrder..NCOST2     ,;
              STORE     WITH ORDHDR.STORE            ,;
              CWARECODE WITH PADR(laware[lnTware],6) ,;
              NECOST1   WITH STYLE.TOTCOST           ,;
              NECOST2   WITH &lcTmpOrder..NCOST2
      =gfAdd_Info('POSLN')
      lnTotQty=lnTotQty+&lcTmpOrder..TOTPIK
      lnNicost1=NCOST1*&lcTmpOrder..TOTPIK
      lnNicost2=NCOST2*&lcTmpOrder..TOTPIK
      REPLACE POSHDR.NICOST1  WITH POSHDR.NICOST1+lnNicost1 ,;
              POSHDR.NFCOST1  WITH POSHDR.NFCOST1+lnNicost1 ,;  
              POSHDR.NICOST2  WITH POSHDR.NICOST2+lnNicost2 ,;
              POSHDR.NFCOST2  WITH POSHDR.NFCOST2+lnNicost2 ,;
              POSHDR.LastLine WITH lnLineNo 
  
    ELSE
      SELECT POSLN
      GO &lcTmpOrder..POSLNPOS
      IF gfObj_Lock(.T.)
        FOR lnX= 1 TO 8
          Z=STR(lnX,1)
          REPLACE QTY&Z   WITH QTY&Z+&lcTmpOrder..PIK&Z      ,;
                  ORD&Z   WITH ORD&Z+&lcTmpOrder..PIK&Z
        ENDFOR  
        REPLACE TOTQTY    WITH TOTQTY+&lcTmpOrder..TOTPIK     ,;
                TOTORD    WITH TOTORD+&lcTmpOrder..TOTPIK     ,;
                NCOST2    WITH NCOST2+&lcTmpOrder..NCOST2            ,;
                NECOST2   WITH NECOST2+&lcTmpOrder..NCOST2 
        lnTotQty=lnTotQty+&lcTmpOrder..TOTPIK
        lnNicost2=NCOST2*&lcTmpOrder..TOTPIK
        REPLACE POSHDR.NICOST2  WITH POSHDR.NICOST2+lnNicost2 ,;
                POSHDR.NFCOST2  WITH POSHDR.NFCOST2+lnNicost2   
        =gfObj_Lock(.F.)      
      ENDIF
    ENDIF
    lnNicost1= 0 
    lnNicost2= 0
    
    *--------------- UPDATE ORDLINE & ORDHDR [start]
    
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      REPLACE ORDLINE.CUT&Z WITH ORDLINE.CUT&Z+&lcTmpOrder..PIK&Z
    ENDFOR 
    REPLACE ORDLINE.TOTCUT WITH ORDLINE.TOTCUT+&lcTmpOrder..TOTPIK
    REPLACE ORDHDR.TOTCUT  WITH ORDHDR .TOTCUT+&lcTmpOrder..TOTPIK


    *--------------- UPDATE ORDLINE & ORDHDR [end..]
  
    *------------------ UPDATE CUTPICK [start]
    SELECT (lcTmpOrder)
    IF lnCutpikPos=0
      SELECT CUTPICK
      APPEND BLANK
           
      REPLACE CTKTNO WITH lcAdorNO ,;
              TRANCD WITH '2'      ,;
              ORDER  WITH &lcTmpOrder..ORDER      ,; 
              CORDLINE WITH STR(&lcTmpOrder..LINENO,6)      ,; 
              CTKTLINENO WITH STR(lnLineNo,6) ,;
              STYLE WITH &lcTmpOrder..STYLE      
      FOR lnX=1 TO 8
        Z=STR(lnX,1)
        REPLACE QTY&Z WITH &lcTmpOrder..PIK&Z
      ENDFOR
      REPLACE TOTQTY WITH &lcTmpOrder..TOTPIK
      =gfAdd_Info('CUTPICK')
    ELSE
      SELECT CUTPICK
      GO lnCutpikPos
      FOR lnX=1 TO 8
        Z=STR(lnX,1)
        REPLACE QTY&Z WITH QTY&Z+&lcTmpOrder..PIK&Z
      ENDFOR
      REPLACE TOTQTY WITH TOTQTY+&lcTmpOrder..TOTPIK
    ENDIF
    *--------------------- UPDATE CUTPICK [end..]    
  ENDIF
  SELECT (lcTmpOrder)
ENDSCAN

*-----------------------    Main Loop [end..]    ----------------------

SELECT POSHDR
IF gfObj_Lock(.T.)
  REPLACE NSTYORDER WITH lnTotQty , OPEN WITH lnTotQty 
  =gfObj_Lock(.F.)      
ENDIF


IF llIgnorBom OR llIgnorFil
  *-- One or more line(s) will be ignored.
  =gfModalGen('TRM38185B42000','DIALOG')
ENDIF

*-- XXX is saved as XXX
IF llBomUpdt OR llFilesUpdt
  =gfModalGen('INM42085B42000','DIALOG','This adornment order'+'|'+lcAdorNO)
ELSE
  IF laType[lnType,2]='G'
    *-- 'This adornment order was generated before,Can't generate'
    =gfModalGen('TRM38184B42000','DIALOG')
    SELECT POSHDR
    BLANK
    DELETE
  ENDIF
ENDIF

SELECT (lcTmpOrder)        
ZAP
SET RELATION OFF INTO (lcTmpBom)
SET RELATION OFF INTO Piktkt 
SELECT (lcTmpBom)
ZAP
SELECT (lcTmpDsgn) 
ZAP



*!************************************************************************
*! Name      : lpBomLoop
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
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

PROCEDURE lpBomLoop
PRIVATE llLoop,lnRecNO,lcCdsgncode  
SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)="SO"+&lcTmpOrder..order+STR(&lcTmpOrder..lineno,6)  
  IF LLSEL
    llLoop=.F.    
    IF laType[lnType,2]='A'
      *-- Check if this design exist befor for this order or not
      *-- and if so don't update the BOMVAR
      lnRecNO=RECNO('BOMVAR')
      lcCdsgncode=Cdsgncode
      *--
      IF lnCutpikPos # 0
        SELECT CUTPICK
        GO lnCutpikPos
        *--  
        IF SEEK('AD'+lcAdorNO+PADL(ALLT(CUTPICK.CTKTLINENO),6),'BOMVAR')
          SELECT BOMVAR
          SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)='AD'+lcAdorNO+PADL(ALLT(CUTPICK.CTKTLINENO),6) FOR Cdsgncode=lcCdsgncode
            llLoop=.T.
            EXIT
          ENDSCAN

          IF llLoop
            GO lnRecNO
            *-- loop in the tmpbomvar scan to not Update BOMVAR file
            SELECT (lcTmpBom)
            llIgnorBom=IIF(llIgnorBom,llIgnorBom,.T.)
            LOOP
          ENDIF
          SELECT (lcTmpBom)
        ENDIF
      ENDIF
    ELSE
      IF llSame
        
        *-- Check if this Line/Desigen was included befor in any adornment order
        SELECT CUTPICK
        =SEEK('2'+&lcTmpOrder..ORDER+STR(&lcTmpOrder..LINENO,6))
        SCAN REST WHILE trancd+order+cordline='2'+&lcTmpOrder..ORDER+STR(&lcTmpOrder..LINENO,6) 
          
          IF SEEK('AD'+Ctktno+PADL(ALLT(Ctktlineno),6),'BOMVAR')
            SELECT BOMVAR
            SCAN REST WHILE cidtype+ccost_id+STR(lineno,6)='AD'+CUTPICK.Ctktno+PADL(ALLT(CUTPICK.Ctktlineno),6)
              IF Cdsgncode=&lcTmpBom..Cdsgncode
                llLoop=.T.
                EXIT
              ENDIF
            ENDSCAN
            IF llLoop  
              EXIT
            ENDIF
          ENDIF
        ENDSCAN
      ENDIF
    ENDIF
    IF llLoop
      SELECT (lcTmpBom)
      llIgnorBom=IIF(llIgnorBom,llIgnorBom,.T.)
      LOOP
    ENDIF

    *-- Generate a new sequence number if you are sure that you will update.
    IF laType[lnType,2]='G' AND EMPTY(lcAdorNO)
      *C101929,1 SSH 06/09/00  [Start Generate separate sequance no for adornment order.
      *lcAdorNO=gfSequence('PO')
      lcAdorNO=gfSequence('POAD') 
      *C101929,1 SSH 06/09/00  END]
      SELECT POSHDR
      REPLACE PO WITH lcAdorNO                
      SELECT (lcTmpOrder)
    ENDIF

    SELECT (lcTmpBom)
    lnNcost2=lnNcost2+TOTCOST   
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
*!************************************************************************
*! Name      : lpChkLine
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
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

PROCEDURE lpChkLine

PRIVATE lcKey
*-- if this line exist in CUTPICK [for the same adornment order number] .
*-- trancd+order+cordline  &&-- index of CUTPICK

IF SEEK('2'+ORDER+STR(LINENO,6),'CUTPICK') 
  *-- Is this order line belong to the adornment order we add to [start]
  *-- If so it must has a record in posln .
  SELECT CUTPICK
  
  IF laType[lnType,2]='A'
    SCAN REST WHILE trancd+order+cordline='2'+ORDER+STR(&lcTmpOrder..LINENO,6) FOR CUTPICK.CTKTNO=lcAdorNO 
      lnCutpikPos=RECNO('CUTPICK')
      EXIT  
    ENDSCAN
      
    *-- Is this order line belong to the adornment order we add to [end..]
            
    SELECT (lcTmpOrder)
    IF lnCutpikPos <> 0
      *--key of posln : cstytype+po+style+STR(lineno,6)+trancd+STR(RECNO(),7) 
      lcKey='A'+CUTPICK.CTKTNO+&lcTmpOrder..style+PADL(ALLT(CUTPICK.CTKTLINENO),6)+'1'
      =SEEK(lcKey,'posln')
      llSame=.F.
      lnLineAdj=POSLN.Lineno
      IF POSLN.SHIPNO=&lcTmpOrder..PIKTKT
        llSame=.T.
      ELSE &&-- the piktkt no is diff
        *-- Adjust the piktkt number in this posln so that when we accomulate 
        *-- it we don't check IF POSLN.SHIPNO=&lcTmpOrder..PIKTKT again .
        SELECT POSLN
        REPLACE SHIPNO WITH &lcTmpOrder..PIKTKT
        *-- Save the recordno of the posln record  to go it as fast as possible 
        SELECT (lcTmpOrder)
        REPLACE POSLNPOS WITH RECNO('posln')
      ENDIF
    ENDIF 
  ELSE
    llSame=.F.
    SCAN REST WHILE trancd+order+cordline='2'+&lcTmpOrder..ORDER+STR(&lcTmpOrder..LINENO,6)   
      lcKey='A'+CUTPICK.CTKTNO+&lcTmpOrder..style+PADL(ALLT(CUTPICK.CTKTLINENO),6)+'1'    
      =SEEK(lcKey,'posln')
      IF POSLN.SHIPNO=&lcTmpOrder..PIKTKT
        llSame=.T.
        EXIT     
      ENDIF  
    ENDSCAN
  ENDIF  
ENDIF

*!***************************************************************************
*! Name      : lfUpdStydye                                                  
*! Developer : Walid Abou El-Magd                                            
*! Date      : 07/01/1999                                                   
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
FUNCTION lfUpdStydye
PRIVATE lcAlias
lcAlias=SELECT()
IF !SEEK(&lcTmpOrder..STYLE+PADR(laware[lnSware],6)+SPACE(10),'STYDYE')
  WAIT "Assigning Style"+ALLTRIM(&lcTmpOrder..STYLE)+" to warehouse "+PADR(laware[lnSware],6) WINDOW NOWAIT
  DO gpAdStyWar WITH &lcTmpOrder..STYLE,SPACE(10),PADR(laware[lnSware],6)
  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnSware],6)+SPACE(10),'STYDYE')
  WAIT CLEAR
ENDIF

*B603318,1 Commnted.
*FOR lnX= 1 TO 8
*  Z=STR(lnX,1)
*  REPLACE STYDYE.WIP&Z WITH STYDYE.WIP&Z - PIK&Z
*ENDFOR
*REPLACE STYDYE.TOTWIP WITH STYDYE.TOTWIP - TOTPIK
*B603318,1 End.

IF !SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')
  WAIT "Assigning Style"+ALLTRIM(&lcTmpOrder..STYLE)+" to warehouse "+PADR(laware[lnTware],6) WINDOW NOWAIT
  DO gpAdStyWar WITH &lcTmpOrder..STYLE,SPACE(10),PADR(laware[lnTware],6)
  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')
  WAIT CLEAR
ENDIF
FOR lnX= 1 TO 8
  Z=STR(lnX,1)
  REPLACE STYDYE.WIP&Z WITH STYDYE.WIP&Z + PIK&Z
ENDFOR
REPLACE STYDYE.TOTWIP WITH STYDYE.TOTWIP + TOTPIK

IF ORDLINE.CWARECODE <> PADR(laware[lnTware],6)
  SELECT STYDYE
  =SEEK(&lcTmpOrder..STYLE+ORDLINE.CWARECODE+SPACE(10),'STYDYE')  
  =RLOCK()
  FOR lnX= 1 TO 8
    Z=STR(lnX,1)
    REPLACE ALO&Z WITH ALO&Z - &lcTmpOrder..PIK&Z
  ENDFOR
  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK
  =SEEK(&lcTmpOrder..STYLE+PADR(laware[lnTware],6)+SPACE(10),'STYDYE')  
  =RLOCK()
  FOR lnX= 1 TO 8
    Z=STR(lnX,1)
    REPLACE ALO&Z WITH ALO&Z + &lcTmpOrder..PIK&Z
  ENDFOR
  REPLACE TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
  UNLOCK
ENDIF
SELECT (lcAlias)


