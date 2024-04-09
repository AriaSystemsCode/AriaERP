*:***************************************************************************
*: Program file  : ALDAN11
*: Program desc. : Automatic Packing List For DAN
*: For screen    : ALDAN11.SCX
*:        System : Aria Advantage Series.
*:        Module : Allocation (AL)
*:        Date   : 06/08/2000
*:     Developer : AHMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO ALDAN11
*:***************************************************************************
*: Due to C101785
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*

IF !gfSetup()
  RETURN
ENDIF  

SELECT PIKTKT
LOCATE FOR PIKTKT <> "******"
IF !FOUND()
  *** Message : "There is no records in the ð file can not proceed."
  ***           "                     < Ok >                       "
  =gfModalGen("TRM44010B00000" , "DIALOG" , "Picking Tickets")
  RETURN
ENDIF  

*-- Define Screens Variables and Temporary file [Begin]
lcDan11Ch0 = gfTempName()  && Child window 0 (For Accounts).
lcDan11Ch1 = gfTempName()  && Child window 1 (For Browse).
lcDan11Ch2 = gfTempName()  && Child window 2 (For Controls).

lcAccount  = SPACE(05)
lcAccName  = SPACE(30)
lcPackNos  = ""
llBrowse   = .F.

lcCloseBtn = '\<Close'
lcAcc      = SPACE(5)
TmpPikTkt  = gfTempName()     && File to hold all the piktkts and to brows from it. 
TmpPikTkt2 = gfTempName()     && Second index for TmpPiktkt file.
TmpPackHdr = gfTempName()     && To hold the packing list lines.
TmpPackLin = gfTempName()     && To hold the packing list headers.
TmpMatrix  = gfTempName()
MatrixIndx = gfTempName()
MatrixInd2 = gfTempName()
TmpCartons = gfTempName()

lcPickBrow = 'PikTkts'
*-- Define Screens Variables and Temporary file [End  ]

*-- Create Temp. File [Begin]
CREATE CURSOR (TmpCartons) (nCarton N(6),nAvail N(5,2),nSize N(1))
INDEX ON STR(nSize,1)+IIF(nAvail=0,"C","O") TAG(TmpCartons) OF &TmpCartons

CREATE TABLE (TmpMatrix) (cStyle C(19),cSize C(5),nSize N(1),cType C(1),;
                             nMaxQty N(5),nPikQTy N(5),cPrePak C(1), nLineNo N(6))
INDEX ON cType+STR(nSize,1)+cSize TAG (MatrixInd2)
INDEX ON cType+cStyle+STR(nSize,1) TAG (MatrixIndx)
INDEX ON cStyle+STR(nSize,1) TAG (TmpMatrix) ADDITIVE

SELECT Pack_Hdr
COPY STRUCTURE TO (TmpPackHdr)
= gfOpenFile (TmpPackHdr,' ','EX')
INDEX ON Pack_No TAG(TmpPackHdr) OF (TmpPackHdr)

SELECT Pack_Lin
COPY STRUCTURE TO (TmpPackLin)
= gfOpenFile (TmpPackLin,' ','EX')
INDEX ON Style+STR(No_Cart,4) TAG(TmpPackLin) OF (TmpPackLin)

SELECT PikTkt
INDEX ON ACCOUNT + PIKTKT TAG ACCPIK OF (gcWorkDir+"PikTktAcc.CDX")
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'lSelRec'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] =  1
laFileStru[lnFileStru+1,4] =  0
CREATE TABLE (TmpPikTkt) FROM ARRAY laFileStru
INDEX ON lSelRec TAG (TmpPikTkt2) OF (TmpPikTkt)
INDEX ON PikTkt TAG (TmpPikTkt) OF (TmpPikTkt)
*-- Create Temp. File [End  ]

*-- Open files to check edi account [Begin]
IF 'EB' $ gcCmpModules
  =gfOpenFile(gcDataDir+'EdiAcPrt','AccFact','SH')
  =gfOpenFile(gcDataDir+'EdiPh','Partner','SH')
ENDIF
*-- Open files to check edi account [End  ]

*-- lcSelSt    : < Select >     Status
*-- lcSelAllSt : < Select All > Status
*-- lcSelNonSt : < Select Non > Status
*-- lcInvertSt : < Invert >     Status
*-- lcProcedSt : < Proceed >    Status
*-- lcAcountSt : < Account >    Status
*-- lcAccKeySt : < AccKey >     Status
STORE 'DISABLE' TO lcSelSt,lcSelAllSt,lcSelNonSt,lcInvertSt,lcProcedSt
STORE 'ENABLE'  TO lcAcountSt,lcAccKeySt
*-- lnBrRecNo  : Browse Record Number
*-- lnSelRecs  : No. of selected records (Zero initially)
*-- lnAllRecs  : All Orders to pick (Selected and Un-Selected)
STORE 0 TO lnBrRecNo , lnSelRecs  , lnAllRecs

PUSH KEY

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcPickBrow)
ON KEY LABEL Alt+L DO lfvSelect WITH "SEL_UNSEL"
ON KEY LABEL Alt+A DO lfvSelect WITH "SEL_ALL"
ON KEY LABEL Alt+N DO lfvSelect WITH "SEL_NON"
ON KEY LABEL Alt+I DO lfvSelect WITH "INVERT"
ON KEY LABEL Alt+P DO lfvProceed
ON KEY LABEL Alt+C DO lfvClose
ON KEY LABEL ESC   DO lfvClose

DO (gcScrDir+gcWinAppl+'\ALDAN11.SPX')

*-- Screen Cleanup Code.
glQuitting = .T.  && Rise quit flag because it's modal screen.

POP KEY
RELEASE WINDOW (lcPickBrow)
IF USED(TmpCartons)
  USE IN (TmpCartons)
ENDIF

IF FILE(gcWorkDir+TmpCartons+'.DBF')
  ERASE &gcWorkdir.&TmpCartons..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+TmpCartons+'.CDX')
  ERASE &gcWorkdir.&TmpCartons..CDX          && Erase the Temp file.
ENDIF

IF USED(TmpMatrix)
  USE IN (TmpMatrix)
ENDIF

IF FILE(gcWorkDir+TmpMatrix+'.DBF')
  ERASE &gcWorkdir.&TmpMatrix..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+TmpMatrix+'.CDX')
  ERASE &gcWorkdir.&TmpMatrix..CDX          && Erase the Temp file.
ENDIF

IF USED(TmpPackHdr)
  USE IN (TmpPackHdr)
ENDIF

IF FILE(gcWorkDir+TmpPackHdr+'.DBF')
  ERASE &gcWorkdir.&TmpPackHdr..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+TmpPackHdr+'.CDX')
  ERASE &gcWorkdir.&TmpPackHdr..CDX          && Erase the Temp file.
ENDIF

IF USED(TmpPackLin)
  USE IN (TmpPackLin)
ENDIF

IF FILE(gcWorkDir+TmpPackLin+'.DBF')
  ERASE &gcWorkdir.&TmpPackLin..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+TmpPackLin+'.CDX')
  ERASE &gcWorkdir.&TmpPackLin..CDX          && Erase the Temp file.
ENDIF

IF USED(TmpPikTkt)
  USE IN (TmpPikTkt)
ENDIF

IF FILE(gcWorkDir+TmpPikTkt+'.DBF')
  ERASE &gcWorkdir.&TmpPikTkt..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+TmpPikTkt+'.CDX')
  ERASE &gcWorkdir.&TmpPikTkt..CDX          && Erase the Temp file.
ENDIF

SELECT PIKTKT
SET INDEX TO
ERASE (gcWorkDir+"PikTktAcc.CDX")           && Erase the Index file.
*-- end of program code.

**********************************************************************
**************** Control Browse and trapping Functions ***************
**********************************************************************

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

*-- Release browse window if found.
IF WEXIST(lcPickBrow)
  RELEASE WINDOW (lcPickBrow)
ENDIF

SELECT (TmpPikTkt)
LOCATE
lnBrRecNo  = RECNO()

lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "lcSelect=IIF(lSelRec , '»' , ' ') :R :H= '»' ,"+;
             "PikTkt    :H = 'PikTkt'    :8:R,"+;
             "Order     :H = 'Order#'    :8:R,"+;
             "Store     :H = 'Store'     :8:R,"+;
             "Date      :H = 'PikDate'   :10:R,"+;
             "cWareCode :H = 'WareHouse' :13:R"

BROWSE FIELDS &lcBrowFild     ;
       WINDOW (lcDan11Ch1)    ;
       WHEN lfwBrows()        ;
       VALID :F lfvBrow()     ;
       IN WINDOW (gcBaseWind) ;
       LOCK 0                 ;
       NOAPPEND               ;
       NOCLEAR                ;
       NODELETE               ;
       NOWAIT                 ;
       NOEDIT                 ;
       NOMENU                 ;
       SAVE                   ;
       TITLE lcPickBrow

*-- end of lfDispBrow.

*!*************************************************************
*! Name      : lfwBrows
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows
lnBrRecNo  = RECNO(TmpPikTkt)
=lfvpbSel()
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Valid Browse function
*!*************************************************************
*
FUNCTION lfvBrow
IF !WONTOP(lcPickBrow)
  =gfStopBrow()
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcPickBrow)
ENDIF
*-- end of lfvBrow.

*!*************************************************************
*! Name      : lfwIbBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfwIbBrow
*-- IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF
RETURN .F.
*-- end of lfwIbBrow.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Function to trap the keys for the Browse , 
*!             and save the changes if the current record was edited
*!*************************************************************
*
FUNCTION lfBrowTrap
*-- IF The window on top is the Browse
IF WONTOP(lcPickBrow)
  glFromBrow = .T.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB DO lfTraps WITH "TAB"
  ON KEY LABEL BACKTAB DO lfTraps WITH "BACKTAB"
  ON KEY LABEL ALT+B 
ENDIF    && End of IF
*-- end of lfBrowTrap.

*!*************************************************************
*! Name      : lfBrwUnTrp
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*-- IF The window on top is not the Browse and coming from the Browse
IF !WONTOP(lcPickBrow) .AND. glFromBrow
  = gfStopBrow()
  glFromBrow = .F.    && Flag to hold .T. if we are coming from the Browse
  ON KEY LABEL TAB
  ON KEY LABEL BACKTAB
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcPickBrow)
ENDIF    && End of IF
*-- end of lfBrwUnTrp.

*!*************************************************************
*! Name      : lfTraps
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
*-- Tab Case
IF lcTrap = "TAB"
  ACTI WINDOW (lcDan11Ch2) TOP
  _CUROBJ = OBJNUM(pbSelect)

ELSE  && Backtab Case.
  IF "CLOSE"$UPPER(lcCloseBtn)
    ACTI WINDOW (lcDan11Ch0) TOP
    _CUROBJ = OBJNUM(lcAccount)
  ELSE
    ACTI WINDOW (lcDan11Ch2) TOP
    _CUROBJ = OBJNUM(pbClose)
  ENDIF
ENDIF
*-- end of lfTraps.

**********************************************************************
********************** Control Screen Functions **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvSelect
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Handle <Select>, <Select All>, <Select None> and <Invert> 
*!           : Buttons pressed.
*!*************************************************************
*! Passed Parameters  : Apply Method which will be one of the following
*!                    : 1- SEL_UNSEL :=> Select / UnSelect Line.
*!                    : 2- SEL_ALL   :=> Select All lines.
*!                    : 3- SEL_NON   :=> UnSelect All lines.
*!                    : 4- INVERT    :=> Invert Select / UnSelect Lines.
*!*************************************************************
*!
FUNCTION lfvSelect
PARAMETERS lcApplyMth
*-- if you have records and you are at specific record and 
*-- if your choice is <Select All> its button must be enable and
*-- if your choice is <Select None> its button must be enable.
*-- this is because you have hot keys.
IF (lnAllRecs<>0) AND !EOF(TmpPikTkt) AND (lnBrRecNo<>0) AND ;
   ((lcApplyMth <> "SEL_ALL") OR (lcSelAllSt = "ENABLE")) AND ;
   ((lcApplyMth <> "SEL_NON") OR (lcSelNonSt = "ENABLE")) 
  
  IF lcApplyMth = "SEL_UNSEL"
    =lfSelUnSel()

  ELSE  && Sel All, Sel None, Or Invert
    PRIVATE lcScanExpr
    DO CASE
      CASE lcApplyMth = "SEL_ALL"  && Select All Case
        lcScanExpr = [FOR !lSelRec]
      
      CASE lcApplyMth = "SEL_NON"  && Select None Case
        lcScanExpr = [FOR lSelRec]

      CASE lcApplyMth = "INVERT"  && Invert Case
        lcScanExpr = []
    ENDCASE
    SCAN &lcScanExpr
      =lfSelUnSel()
    ENDSCAN  
    GO lnBrRecNo
  ENDIF

  *-- Apply Select Function.
  =lfRefScr()
ENDIF
*-- end of lfvSelect.

*!*************************************************************
*! Name      : lfSelUnSel
*! Developer : AMH (AMHED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Select / Unselect Record.
*!*************************************************************
*!
FUNCTION lfSelUnSel
REPLACE lSelRec WITH !lSelRec
lnSelRecs = lnSelRecs + IIF(lSelRec,1,-1)
*-- end of lfSelUnSel.

*!*************************************************************
*! Name      : lfRefScr
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Refresh screen Controls
*!*************************************************************
*
FUNCTION lfRefScr
IF lnAllRecs = 0
  STORE 'DISABLE' TO lcSelSt,lcSelAllSt,lcSelNonSt,lcInvertSt,lcProcedSt
  STORE 'ENABLE'  TO lcAcountSt,lcAccKeySt
ELSE
  STORE "ENABLE" TO lcSelSt , lcInvertSt
  STORE IIF(lnSelRecs = 0, "DISABLE", "ENABLE") TO lcProcedSt , lcSelNonSt
  lcSelAllSt = IIF(lnSelRecs = lnAllRecs, "DISABLE", "ENABLE")
  STORE 'DISABLE' TO lcAcountSt,lcAccKeySt
ENDIF

ACTI WINDOW (lcDan11Ch2) TOP
SHOW GET pbSelAll  &lcSelAllSt
SHOW GET pbSelNon  &lcSelNonSt
SHOW GET pbInvert  &lcInvertSt
SHOW GET pbProceed &lcProcedSt
SHOW GET pbAccount &lcAcountSt
SHOW GET ibAccount &lcAccKeySt
SHOW GET lcAccount &lcAcountSt
=lfvpbSel()
*-- end of lfRefScr.

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : if Current status is Select prompt is Un-Select and vice versa
*!*************************************************************
*
FUNCTION lfvpbSel
ACTI WINDOW (lcDan11Ch2) TOP
IF lSelRec
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'  &lcSelSt
ELSE    && Else
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'    &lcSelSt
ENDIF

SHOW GET pbClose,1 PROMPT lcCloseBtn
SHOW WINDOW (lcPickBrow) REFRESH
*-- end of lfvpbSel.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Clear Active Keys
*!*************************************************************
*!
FUNCTION lfClearKey
ON KEY LABEL Alt+L
ON KEY LABEL Alt+A
ON KEY LABEL Alt+N
ON KEY LABEL Alt+I
ON KEY LABEL Alt+P
ON KEY LABEL Alt+S
ON KEY LABEL Alt+C
ON KEY LABEL ESC
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+B
*-- end of lfClearKey.

*!*************************************************************
*! Name      : lfvAccount
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Valed function of account field
*!*************************************************************
*!
FUNCTION lfvAccount
IF MDOWN()
  RETURN
ENDIF  

IF llBrowse .OR. (!EMPTY(lcAccount) .AND. !SEEK('M'+lcAccount,"Customer"))
  DO CusBrowM WITH lcAccount
ENDIF

llBrowse   = .F.

IF EMPTY(lcAccount)
  RETURN
ELSE
  lcCloseBtn = "\<Cancel"
  =lfGetPkTkt()
ENDIF
*-- end of lfvAccount.

*!*************************************************************
*! Name      : lfGetPkTkt
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : To get the related data of the order
*!*************************************************************
*! Example            :  lfGetPkTkt()
*!*************************************************************
FUNCTION lfGetPkTkt

lnAllRecs = 0

SHOW GET lcAccount
IF SEEK("M"+lcAccount,'Customer')
  lcAccName = Customer.StName  
  IF SEEK(lcAccount,"PikTkt")
    SELECT PikTkt
    SCAN REST WHILE Account+PikTkt = lcAccount
      IF !SEEK(PikTkt,'Pack_Hdr')
        SCATTER MEMVAR MEMO
        INSERT INTO (TmpPikTkt) FROM MEMVAR
        lnAllRecs = lnAllRecs + 1
      ENDIF
    ENDSCAN
  ENDIF    
  SELECT (TmpPiktkt)
  LOCATE
  IF EOF()
    = gfModalGen("TRM00000B00000",.F.," ",.F.,"No Pick tickets found for account "+lcAccount+".")
    lcAccount = SPACE(5)
    _CUROBJ = _CUROBJ
    lcCloseBtn = "\<Close"
    RETURN
  ENDIF
ENDIF  

=lfDispBrow()
=lfRefScr()
=lfRefresh()
*-- end of lfGetPkTkt.

*!*************************************************************
*! Name      : lfDelBrow
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : To Delete the data of the Browser
*!*************************************************************
*! Example            :  lfDelBrow()
*!*************************************************************
FUNCTION lfDelBrow

lnAllRecs = 0
lcCloseBtn = "\<Close"
lcAccount = SPACE(05)
lcAccName = SPACE(30)
  
IF USED(TmpPikTkt)
  SELECT(TmpPikTkt)
  ZAP
ENDIF
=lfDispBrow()
=lfRefScr()
ACTI WINDOW (lcDan11Ch0) TOP
_CUROBJ = OBJNUM(lcAccount)
=lfRefresh()
*-- end of lfDelBrow.
**********************************************************************
************************ Business Rules Section **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvProceed
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : To validate the proceed button and create the packing list.
*!*************************************************************
*! Example            :  lfvProceed()
*!*************************************************************
FUNCTION lfvProceed

llStandPk = ('EB' $ gcCmpModules) AND SEEK ('A'+OrdHdr.Account,'EdiAcPrt');
             AND SEEK (EdiAcPrt.cPartCode,'EdiPh') AND EdiPh.cCrtnType = "S"

SELECT (TmpPikTkt)
SET ORDER TO TAG(TmpPikTkt2)
IF !SEEK(.T.) && Not Selected Any Pick Ticket
  = gfModalGen("TRM00000B00000",.F.," ",.F.,'No Pick Ticket was selected to be packed.')
  SET ORDER TO TAG (TmpPikTkt) IN (TmpPikTkt)
  RETURN
ENDIF

SCAN REST WHILE lSelRec && Loop For Selected Pick Ticket
  *-- Intialize variables 
  STORE 0 TO lnCartNo,lnLineNo,lnTot_Wght,lnTot_Pcs,lnRemaining
  IF SEEK('O'+&TmpPikTkt..Order,'OrdHdr') AND SEEK('O'+&TmpPikTkt..Order,"OrdLine")
    IF !lfMakePack(&TmpPikTkt..Order,&TmpPikTkt..PikTKt) && Can't make Packing List
      = gfModalGen("TRM00000B00000",.F.," ",.F.,"All styles or sizes have not been entered"+;
                  " into the custom matrix program. This piktkt will be ignored.")
      SELECT(TmpMatrix)
      ZAP
      SELECT (TmpPackLin)
      ZAP
      LOOP
    ENDIF
  ENDIF  
  
  SELECT (TmpPackLin)
  LOCATE  
  IF !EOF() && Creat any line in TmpPackLin
    lcPackNos   = lcPackNos + &TmpPikTkt..PikTkt
    m.Pack_No   = &TmpPikTkt..PikTkt
    m.Order     = &TmpPikTkt..Order
    m.Account   = lcAccount            
    m.cWareCode = &TmpPikTkt..cWareCode
    m.Store     = &TmpPikTkt..Store    
    m.Tot_Wght  = lnTot_Wght           
    m.Tot_Cart  = lnCartNo             
    m.Tot_Pcs   = lnTot_Pcs                        
    m.ShipVia   = IIF(!EMPTY(OrdHdr.ShipVia),OrdHdr.ShipVia,;
                       Customer.ShipVia)          
    m.nLastCart = lnCartNo             
    m.lStandCtn = llStandPk            
    m.cToStOrCn = 'S'                  
    m.PikTkt    = &TmpPikTkt..PikTkt
    INSERT INTO Pack_Hdr FROM MEMVAR
    
    SCAN
      SCATTER MEMVAR MEMO
      INSERT INTO Pack_Lin FROM MEMVAR 
    ENDSCAN

  ENDIF

  SELECT(TmpMatrix)
  ZAP
  SELECT (TmpPackLin)
  ZAP

ENDSCAN

IF EMPTY(lcPackNos) && No Packing List Created
  = gfModalGen("TRM00000B00000",.F.," ",.F.,"No packs were generaed for the selected pick tickets")
ELSE
  IF LEN(lcPackNos) > 7 && More Than One Pack
    = gfModalGen("TRM00000B00000",.F.," ",.F.,'Packing list saved with # '+LEFT(lcPackNos,6);
                   + ' throught # ' + RIGHT(lcPackNos,6)+' .')
  ELSE
    = gfModalGen("TRM00000B00000",.F.," ",.F.,'Packing list saved with # '+lcPackNos+ '.')
  ENDIF
ENDIF
SELECT (TmpPikTkt)
SET ORDER TO TAG (TmpPikTkt)
DO WHILE LEN(lcPackNos) > 0
  lcPikTkt = LEFT(lcPackNos,6)
  lcPackNos = SUBSTR(lcPackNos,7)
  SEEK lcPikTkt
  DELETE
  lnAllRecs = lnAllRecs - 1
ENDDO
lcPackNos = ""
IF lnAllRecs = 0
  =lfDelBrow()
ELSE
  =lfDispBrow()
  =lfRefScr()
  =lfRefresh()
ENDIF
*-- end of lfvProceed.

*!*************************************************************
*! Name      : lfMakePack
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : To build the packing list lines and generate the cartons.
*!*************************************************************
*! Example            : =lfMakePack()
*!*************************************************************
FUNCTION lfMakePack
PARAMETERS lcOrder,lcPIkTkt

SET ORDER TO (TmpMatrix) IN (TmpMatrix)

SELECT ORDLINE
SET RELATION TO 'S'+Scale INTO Scale
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+lcOrder;
          FOR Picked AND PikTkt = lcPikTkt
  IF SEEK(lcAccount+SUBSTR(Style,1,12),'PackMtrx')
    lcAcc = lcAccount
  ELSE
    IF SEEK("*****"+SUBSTR(Style,1,12),'PackMtrx')
      lcAcc = "*****"
    ELSE
      RETURN(.F.)
    ENDIF        
  ENDIF

  FOR lnCount = 1 TO 8
    SELECT ORDLINE
    lcCount = STR(lnCount,1)
    IF Pik&lcCount <> 0
      IF !SEEK(Style+lcCount,TmpMatrix)
        SELECT PACKMTRX
        =SEEK(lcAcc+SUBSTR(OrdLine.Style,1,12))
        LOCATE REST WHILE cAccount+cStyle= lcAcc+SUBSTR(OrdLine.Style,1,12) ;
        FOR nMaxSize&lcCount <> 0
        IF FOUND()
          DO CASE
            CASE cMixStyle='N' AND cMixSize = 'N'
              lcType = '0'
            CASE cMixStyle='N' AND cMixSize = 'Y'
              lcType = '1'
            CASE cMixStyle='Y' AND cMixSize = 'N'
              lcType = '2'
            CASE cMixStyle='Y' AND cMixSize = 'Y'
              lcType = '3'
          ENDCASE
          INSERT INTO (TmpMatrix) (cStyle,cSize,nSize,cType,nMaxQty,nPikQTy,;
                                  cPrePak,nLineNo);
           VALUES (OrdLine.Style,Scale.Sz&lcCount,lnCount,lcType,;
           PACKMTRX.nMaxSize&lcCount,OrdLine.Pik&lcCount,OrdLine.PrePak,OrdLine.LineNo)
        ENDIF  
      ENDIF
    ENDIF
  ENDFOR
ENDSCAN

SELECT (TmpMatrix)
SET ORDER TO TAG (MatrixIndx)
LOCATE

DO WHILE !EOF()
  lcType = cType
  DO CASE
    CASE lcType = '0' && Mix Style No And Mix Size No
      SCAN REST WHILE cType = lcType
        WAIT WINDOW "Style/Color/Size :" + ALLTRIM(cStyle)+' '+cSize NOWAIT
        lnRemaining = nPikQTy
        =SEEK(cStyle,'Style')
        lcQtySize = STR(nSize,1)
        DO WHILE lnRemaining > 0
          lnCartNo = lnCartNo + 1
          lnLineNo = lnLineNo + 1
          SELECT (TmpPackLin)
          APPEND BLANK
          REPLACE Pack_No          WITH lcPikTkt                ,;
                  Line_No          WITH lnLineNo                ,;
                  Style            WITH &TmpMatrix..cStyle       ,;
                  No_Cart          WITH lnCartNo                 ,;
                  PrePak           WITH &TmpMatrix..cPrePak      ,;
                  Qty&lcQtySize    WITH MIN(lnRemaining,&TmpMatrix..nMaxQty),;
                  Weight           WITH Qty&lcQtySize * Style.nStyWeight,;
                  nOrdLineNo       WITH &TmpMatrix..nLineNo         ,; 
                  TotQty           WITH Qty&lcQtySize
          lnTot_Pcs  = lnTot_Pcs + Qty&lcQtySize
          lnTot_Wght = lnTot_Wght + Weight        
          lnRemaining = MAX(lnRemaining - &TmpMatrix..nMaxQty,0)
        ENDDO
        SELECT (TmpMatrix)
      ENDSCAN      
    CASE lcType = '1' && Mix Style No And Mix Size Yes
      lnAvailabl = 0
      lcCurStyle = ''
      SCAN REST WHILE cType = lcType
        WAIT WINDOW "Style/Color/Size :" + ALLTRIM(cStyle)+' '+cSize NOWAIT
        lnRemaining = nPikQTy 
        =SEEK(cStyle,'Style')
        lcQtySize  = STR(nSize,1)
        SELECT (TmpPackLin)
         DO WHILE lnRemaining > 0
          lnLineNo  = lnLineNo + 1
          IF lnAvailabl = 0 .OR. &TmpMatrix..cStyle <> lcCurStyle OR INT(&TmpMatrix..nMaxQty*lnAvailabl) <= 0
            lcCurStyle = &TmpMatrix..cStyle 
            lnCartNo   = lnCartNo + 1
            lnAvailabl = 1
            APPEND BLANK
          ENDIF  
          REPLACE Pack_No          WITH lcPikTkt                ,;
                  Line_No          WITH lnLineNo                ,;
                  Style            WITH &TmpMatrix..cStyle       ,;
                  No_Cart          WITH lnCartNo                 ,;
                  PrePak           WITH &TmpMatrix..cPrePak      ,;
                  Qty&lcQtySize    WITH MIN(lnRemaining,INT(&TmpMatrix..nMaxQty*lnAvailabl)),;
                  Weight           WITH Weight+Qty&lcQtySize * Style.nStyWeight,;
                  nOrdLineNo       WITH &TmpMatrix..nLineNo      ,; 
                  TotQty           WITH TotQty+Qty&lcQtySize
          lnTot_Pcs   = lnTot_Pcs + Qty&lcQtySize
          lnTot_Wght  = lnTot_Wght + Weight
          lnAvailabl  = (&TmpMatrix..nMaxQty*lnAvailabl - Qty&lcQtySize) / &TmpMatrix..nMaxQty 
          lnRemaining = MAX(lnRemaining - Qty&lcQtySize,0)
        ENDDO
        SELECT (TmpMatrix)
      ENDSCAN
    CASE lcType  = '2' && Mix Style Yes And Mix Size No
      lnAvailabl = 0
      lcCurSize  = ''
      SET ORDER TO (MatrixInd2)
      =SEEK(lcType)
      SCAN REST WHILE cType = lcType
        WAIT WINDOW "Style/Color/Size :" + ALLTRIM(cStyle)+' '+cSize NOWAIT
        lnRemaining = nPikQTy 
        =SEEK(cStyle,'Style')
        lcQtySize  = STR(nSize,1)

        DO WHILE lnRemaining > 0
          lnLineNo  = lnLineNo + 1        
          IF lnAvailabl = 0 .OR. &TmpMatrix..cSize <> lcCurSize OR INT(&TmpMatrix..nMaxQty*lnAvailabl) <= 0
            lcCurSize   = &TmpMatrix..cSize
            lnCartNo    = lnCartNo + 1
            lnAvailabl  = 1
            INSERT INTO (TmpCartons) (nCarton,nAvail,nSize);
                              VALUES (lnCartNo,lnAvailabl,&TmpMatrix..nSize)
          ENDIF            
          SELECT (TmpPackLin)
          APPEND BLANK
          REPLACE Pack_No          WITH lcPikTkt                ,;
                  Line_No          WITH lnLineNo                ,;
                  Style            WITH &TmpMatrix..cStyle       ,;
                  No_Cart          WITH lnCartNo                 ,;
                  PrePak           WITH &TmpMatrix..cPrePak      ,;
                  Qty&lcQtySize    WITH Qty&lcQtySize + MIN(lnRemaining,INT(&TmpMatrix..nMaxQty*lnAvailabl)),;
                  Weight           WITH Qty&lcQtySize * Style.nStyWeight,;
                  nOrdLineNo       WITH &TmpMatrix..nLineNo         ,; 
                  TotQty           WITH Qty&lcQtySize
          lnTot_Pcs   = lnTot_Pcs + Qty&lcQtySize
          lnTot_Wght  = lnTot_Wght + Weight
          lnRemaining = MAX(lnRemaining - MIN(lnRemaining,INT(&TmpMatrix..nMaxQty*lnAvailabl)),0)
          lnAvailabl  = (&TmpMatrix..nMaxQty*lnAvailabl - Qty&lcQtySize) / &TmpMatrix..nMaxQty           
          SELECT(TmpCartons)
          REPLACE nAvail WITH INT(lnAvailabl)
        ENDDO
        SELECT (TmpMatrix)
      ENDSCAN

    CASE lcType = '3' && Mix Style Yes And Mix Size Yes
      lnAvailabl = 0
      SCAN REST WHILE cType = lcType
        WAIT WINDOW "Style/Color/Size :" + ALLTRIM(cStyle)+' '+cSize NOWAIT
        lnRemaining = nPikQTy 
        =SEEK(cStyle,'Style')
        lcQtySize  = STR(nSize,1)
        SELECT (TmpMatrix)        
        DO WHILE lnRemaining > 0
          IF lnAvailabl = 0 OR INT(&TmpMatrix..nMaxQty*lnAvailabl) <= 0
            IF SEEK(lcQtySize+"O",TmpCartons) 
              lnCartNo   = &TmpCartons..nCarton
              lnAvailabl = &TmpCartons..nAvail
            ELSE
              lnCartNo   = lnCartNo + 1
              lnAvailabl = 1
            ENDIF  
          ENDIF  
          SELECT (TmpPackLin)

          IF !SEEK(&TmpMatrix..cStyle+STR(lnCartNo,4))
            APPEND BLANK
            lnLineNo  = lnLineNo + 1
          ENDIF 
          REPLACE Pack_No          WITH lcPikTkt                ,;
                  Line_No          WITH lnLineNo                ,;
                  Style            WITH &TmpMatrix..cStyle       ,;
                  No_Cart          WITH lnCartNo                 ,;
                  PrePak           WITH &TmpMatrix..cPrePak      ,;
                  Qty&lcQtySize    WITH MIN(lnRemaining,INT(&TmpMatrix..nMaxQty*lnAvailabl)),;
                  Weight           WITH Weight+Qty&lcQtySize * Style.nStyWeight,;
                  nOrdLineNo       WITH &TmpMatrix..nLineNo         ,; 
                  TotQty           WITH TotQty + Qty&lcQtySize
          lnTot_Pcs   = lnTot_Pcs + Qty&lcQtySize
          lnTot_Wght  = lnTot_Wght + (Qty&lcQtySize * Style.nStyWeight)
          lnAvailabl  = (&TmpMatrix..nMaxQty*lnAvailabl - Qty&lcQtySize) / &TmpMatrix..nMaxQty 
          lnRemaining = MAX(lnRemaining - Qty&lcQtySize,0)

          IF !EOF(TmpCartons) && In case we used an existing carton
            SELECT(TmpCartons)
            REPLACE nAvail WITH IIF(lnAvailabl<0,0,lnAvailabl)
         ENDIF  

        ENDDO
        SELECT (TmpMatrix)
      ENDSCAN 
  ENDCASE 
ENDDO
SET ORDER TO (TmpPackLin) IN (TmpPackLin)
WAIT CLEAR
*-- end of lfMakePack.

**********************************************************************
************************ Close screen Section ************************
**********************************************************************

*!*************************************************************
*! Name      : lfvClose
*! Developer : AMH (AHMED MAHER)
*! Date      : 06/08/2000
*! Purpose   : Clear Read Cycle
*!*************************************************************
*!
FUNCTION lfvClose
IF "CLOSE"$UPPER(lcCloseBtn)
  ACTI WINDOW (lcDan11Ch2) TOP
  CLEAR READ
ELSE
  =lfDelBrow()
ENDIF
*-- end of lfvClose.