*!**********************************************************************************************
*! Program file  : ARVInvG.PRG
*! Program desc. : Custom Voiding Invoice for GMA
*! Screen        : ArVInvGM.SPR
*! System        : Aria Advantage Series
*! Module        : Accounts Receivable (AR)
*! Developer     : Sameh Saiid Ezzat 
*! Date          : 01/18/2001
*! Reference     : C102085
*!**********************************************************************************************
*! Passed Parameters : None.
*!**********************************************************************************************
*! Example           : DO ARVInvG
*!**********************************************************************************************
*! C200352,1 HBG 19/06/2002 Update 'SHDAEXPR' File for the Voided invoices
*! B123626,1 NNA 11/01/2004 Fix bug that if you have 3 Consolidated invoices and you voided the First
*! B123626,1 NNA            then you voided the third, you'll see that system make the Second invoice
*! B123626,1 NNA            voided in the Consinvh.dbf but it's Status in the InvHdr.dbf still Completed
*! B123626,1 NNA            So this bug effect on the Rebalance Program
*!**********************************************************************************************

*-- Preparing Screen Variables
PRIVATE lcInvExpr , lcScrSessn , lcTempFile , lcArGMA11 , lcArGMA12 , lcBrowTtl , lcFiles ,;
        lnSelRec , lnUnSelRec , llCallScop , llContinue , lcCurrInv , llVoid , llIsEngland ,;
        lcScrMode , llEndVoid , llYesToAll , llNoToAll , ldVDate , ldOldDate , lcInvFrom ,;
        lcInvTo , lcMarker , lcBrowStr , lcSelInv , lnBrRecNo

*-- All this variable is related to the Voiding part [Begin]
PRIVATE lcInvLine , lcInvHdr , ldDefInvDate , ldDefPstDate , lcInvLine , ;
        lcInvHdr , lcTmpAR , lcSysYear , lcSysPeriod , lcIDefSes , ;
        lcIDefDiv , lcIDefWare , lcGlSession , llUpsInsur , ;
        llReBuild , lnUnCmSeRc , lcAdTrnSeq , lcAcTrnSeq , ;
        lcHisSeq , lcRepBat , llUpdGlDif , llUpdMstGL , lnShipAddr , ;
        llActCrdt , lcGlDist , llRetLine , llRetHdr , llFromEDI
DECLARE laSetups[17,2] , laVars[20] , laCodes[1,10]

STORE ''  TO lcIDefSes, lcIDefDiv , lcIDefWare , lcAdTrnSeq , lcAcTrnSeq ,;
             lcHisSeq , lcGlSess  , lcRepBat   , laSetups   , laVars ,;
             lcInvHdr , lcInvLine , lcTmpAR    , lcGlDist , laCodes ,;
             lcInvFrom , lcInvTo , lcMarker , lcBrowStr , lcSelInv
             
STORE {} TO ldDefInvDate , ldDefPstDate , ldVDate , ldOldDate
STORE .F. TO llUpsInsur , llActCrdt , llFromEDI , llVoid , llIsEngland , llEndVoid , ;
             llRetLine , llRetHdr , llYesToAll , llNoToAll
STORE .T. TO llUpdGlDif , llUpdMstGL

*-- GMA is USA client but we have to keep this var. in case we convert this Prg to standard one.
IF UPPER(ALLTRIM(gcContCode))='ENG'   && in case of England
  llIsEngland = .T.
ELSE
  llIsEngland = .F.
ENDIF
lnUnCmSeRc = 0

*C200352,1 HBG 19/06/2002 Flag to be used when Call ARDINV.prg to detarmain that This is for GMA[Begin] 
llGMAPack = .T.
*C200352,1 [End]

*-- order cancelation reason codes
laCodes[1,1] = 'CCANCRESON'
laCodes[1,2] = 'laCanReason'
laCodes[1,3] = 'lnCanReason'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'CCANCRESON'

*-- variable used to update the uncomplete session file
laVars[1]  = 'ldDefInvDate'
laVars[2]  = 'lcIDefSes'  
laVars[3]  = 'lcIDefDiv'
laVars[4]  = 'lcIDefWare'
laVars[5]  = 'llRebuild'
laVars[6]  = 'lcGlSession'
laVars[7]  = 'ldDefPstDate'  
laVars[8]  = 'llUpsInsur'
laVars[9]  = 'lcScrMode'
laVars[10] = 'lcCurrInv'
laVars[11] = 'lnUnCmSeRc'    && Used by Key off program
laVars[12] = 'lcAdTrnSeq'    && Used by Key off program
laVars[13] = 'lcAcTrnSeq'    && Used by Key off program
laVars[14] = 'lcHisSeq'      && Used by Key off program
laVars[15] = 'lcGlSess'      && Used by Key off program
laVars[16] = 'lcRepBat'      && Used by Key off program
laVars[17] = 'llUpdGlDif'    && Used by Key off program
laVars[18] = 'llUpdMstGL'    && Used by Key off program
laVars[19] = 'lnShipAddr'
laVars[20] = 'llActCrdt'
*-- All this variable is related to the Voiding part [End]

STORE '' TO lcInvExpr , lcScrSessn , lcTempFile , lcArGMA11 , lcArGMA12 , lcBrowTtl , ;
            lcFiles , lcCurrInv
STORE 0 TO lnSelRec , lnUnSelRec
STORE .F. TO llCallScop , llContinue
lcScrMode = 'V'

lnBrRecNo = 1                   && Varible to hold the browse record number
lcBrowTtl = 'Invoices'          && Varible to hold the browse title

DIMENSION laPanelObj(2,3)    && Array to add new buttons to the control panel
laPanelObj[1,1] = 'pbScop'
laPanelObj[1,2] = gcBmpHome + 'SCOPE.BMP'
laPanelObj[1,3] = 'VALID lfvScope()'
laPanelObj[2,1] = 'pbRel'
laPanelObj[2,2] = gcBmpHome + 'RELEASE2.BMP'
laPanelObj[2,3] = 'VALID lfRelScr()'

lnSessNo = gnProgCopy

=gfOpenFile(gcDataDir+'Style',gcDataDir+'Style','SH') 
=gfOpenFile(gcDataDir+'StyDye',gcDataDir+'StyDye','SH')

IF !gfSetup()
  RETURN
ENDIF

llNoShow   = .F.       && Flag to make the screen call the PROCEDURE lpShow every time it runs
llFrstTime = .T.       && Flag to know if we are going to call lpShow for the first time

IF !WEXIST(gcBaseWind)
  lcArGMA11  = gfTempName()
  lcArGMA12  = gfTempName()
  lcTempFile = gfTempName()

  lcWindTitle = 'Voiding Invoices'
  
  *-- Uncomplete session
  llContinue = lfChkUnComS()
  lcScrSessn = IIF(llContinue , lcScrSessn , gfSequence('CSESSION'))

  llCallScop = .T.

  *--- First Show an empty Browser
  DO lpCreatTmp           && Create the Temp File Based on InvHdr File
ENDIF

*-- Run the Screen.
DO (gcScrDir + gcWinAppl + '\' + 'ArVInvG.SPX')

IF glQuitting
  IF USED(lcTempFile)
    USE IN (lcTempFile)
  ENDIF
  ERASE &gcWorkdir.&lcTempFile..DBF  
  ERASE &gcWorkdir.&lcTempFile..CDX  
   ERASE &gcWorkdir.&lcTempFile..FPT  
ENDIF

RELEASE WINDOW (lcBrowTtl)

POP KEY
*-- End of Program.

*!***************************************************************************
*! Name      : lfDispBrow
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Browses the TEMP File Records.
*!***************************************************************************
*! Example   : =lfDispBrow()
*!***************************************************************************
*
FUNCTION lfDispBrow

IF EOF(lcTempFile)
  lnBrRecNo = RECCOUNT()
ELSE
  lnBrRecNo = RECNO()
ENDIF
lcBrowStr = "lcMarker=IIF(RECNO() = lnBrRecNo,'>',' '):1:H=' ':W = .F., "+;
            "lcSelInv=IIF(lSelInv , '»', ' '):R :H='»',"+;
            "Invoice  :R :H= 'Invoice'    :8  ,"+;
            "InvDate  :R :H= 'Date'       :10 ,"+;
            "Account  :R :H= 'Account'    :10 ,"+;
            "Store    :R :H= 'Store'      :16 ,"+;
            "cBtName=LOOKUP(Customer.BTName, IIF(EMPTY(Store) , 'M'+ Account ,'S'+ Account+ Store),Customer.Type,'CUSTOMER') :R :H= 'Bill to' :35,"+;
            "CustPO   :R :H= 'Reference'  :16 ,"+;
            "Ship     :R :H= 'Pieces'     :12 ,"+;
            "ShipAmt  :R :H= 'Merchandise':11 "

SELECT (lcTempFile)
LOCATE
BROWSE FIELDS &lcBrowStr               ;
       WINDOW (lcArGMA11)              ;
       WHEN lfwBrows() AND lfvpbSel();
       VALID :F lfvBrows()             ;  
       IN WINDOW (gcBaseWind)          ;
       LOCK 0                          ;
       NOAPPEND                        ;
       NOCLEAR                         ;
       NODELETE                        ;
       NOWAIT                          ;
       NOEDIT                          ;
       NOMENU                          ;
       SAVE                            ;
       TITLE lcBrowTtl
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrowTtl)
*-- End of lfDispBrow.

*!***************************************************************************
*! Name      : lpShow
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Handle The special case for screen modes.
*!***************************************************************************
*! Example   : DO lpShow
*!***************************************************************************
*
PROCEDURE lpShow

SHOW GET pbEdt     DISABLE
SHOW GET pbBrws    DISABLE
SHOW GET pbcpPrint DISABLE
SHOW GET pbSlct    DISABLE
SHOW GET pbDlt     DISABLE

laCtrStat[7]  = "DISABLE"                && Edit button 
laCtrStat[8]  = "DISABLE"                && Delete button
laCtrStat[9]  = "DISABLE"                && Select button
laCtrStat[10] = "DISABLE"                && Browse button

*-- IF Statment to check if we are going to call the option grid for the user
*-- IF these is the first time for these session of the program
IF llCallScop
  SHOW GET pbRel DISABLE
  = lfvScope()
  llCallScop = .F.
ENDIF  && End of IF

llFrstTime = .F.

IF USED(lcTempFile)
  IF EOF(lcTempFile)
    SHOW GET pbSelect DISABLE
    SHOW GET pbSelAll DISABLE
    SHOW GET pbSelNon DISABLE
    SHOW GET pbInvert DISABLE
  ELSE
    SHOW GET pbSelect ENABLE
    SHOW GET pbSelAll ENABLE
    SHOW GET pbSelNon ENABLE
    SHOW GET pbInvert ENABLE
  ENDIF
ENDIF

lnBrRecNo  = RECNO(lcTempFile)
IF TYPE('lcInvExpr') <> 'L'
  SHOW WINDOW (lcBrowTtl) REFRESH
ENDIF
*-- End of lpShow.

*!***************************************************************************
*! Name      : lfChkUnComS
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Check for uncompleted invoice sessions.
*!***************************************************************************
*! Calls     : gfUnCompSession
*!***************************************************************************
*! Returns   : llContinue : .T. Found an uncompleted session.
*!                          .F. No uncompleted sessions Found.
*!***************************************************************************
*! Example   : =lfChkUnComS(.F.)
*!***************************************************************************
*
FUNCTION lfChkUnComS
PRIVATE lcObject

lnReprocess = SET('REPROCESS')
IF lnSessNo = 1 .AND. SEEK('O'+PADR('DINVOICE',10)+gcUser_id,'UNCMSESS')
  SET REPROCESS TO 1
  SELECT UNCMSESS
  SCAN REST WHILE Status+cUTranType+gcUser_id = 'O'+PADR('DINVOICE',10)+gcUser_id FOR RLOCK()
    SET REPROCESS TO lnReprocess
    IF FOUND() AND UPPER(UnCmSess.cCurrObj)="PBDLT"
      llContinue = .T.
    ELSE
  	  llContinue = .F.
    ENDIF
  ENDSCAN  
ENDIF

IF llContinue
  IF gfUnCompSession('DINVOICE', lnSessNo, "Direct Invoice")
    lcScrMode = 'V'
  
    SELECT InvHdr
    =SEEK(lcCurrInv)
    SELECT InvLine
    =SEEK(lcCurrInv)
    
    STORE .T. TO llContinue
    lcSession = UNCMSESS.cSession
    lcFiles = ALLTRIM(UnCmSess.Mtmpfiles)

    lcObject = ALLTRIM(UnCmSess.cCurrObj)
    IF !EMPTY(lcObject)
      IF UPPER(lcObject)="PBDLT"
        =lfRelScr(lcCurrInv)
      ENDIF
    ENDIF
  ENDIF
ENDIF  
RETURN (llContinue)
*-- End of lfChkUnComS.

*!***************************************************************************
*! Name      : lfvScope
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Valid function of push button Scope.
*!***************************************************************************
*! Calls     : gfOpGrid() , lfCrT() , lfDispBrow()
*!***************************************************************************
*! Example   : =lfvScope()
*!***************************************************************************
*
FUNCTION lfvScope

*-- Get the current cancellation Type
PUSH KEY CLEAR
lcInvExpr  = gfOpGrid('ARVINVGM', .T.)
POP KEY

DO lpCreatTmp        && Re-Create the Temp File
DO lpFillField       && AnyWay Start filling the Tmp File

IF EOF(lcTempFile)
  *--Disable Buttons
  SHOW GET pbSelect DISABLE
  SHOW GET pbSelAll DISABLE
  SHOW GET pbInvert DISABLE
  SHOW GET pbSelNon DISABLE
ELSE
  *--Enable Buttons
  SHOW GET pbSelect ENABLE
  SHOW GET pbSelAll ENABLE
  SHOW GET pbInvert ENABLE
  SHOW GET pbSelNon DISABLE
ENDIF

lnBrRecNo = RECNO()
= lfDispBrow()    && Browse What U Got

llCallScop = .F.             && Screen Already Initialized

*-- IF The temp file [lcTempFile] is not empty
IF !EOF()
  laScrMode[2] = .T.
  SHOW GET pbSelect ENABLE
  SHOW GET pbSelAll ENABLE
  SHOW GET pbInvert ENABLE
  SHOW GET pbSelNon DISABLE
ELSE    && Else
  laScrMode[1] = .T.
  SHOW GET pbSelect DISABLE
  SHOW GET pbSelAll DISABLE
  SHOW GET pbInvert DISABLE
  SHOW GET pbSelNon DISABLE
ENDIF    && End of IF

SHOW GETS ONLY
SHOW GET pbTop DISABLE
SHOW GET pbPrvs DISABLE
SHOW GET pbBtm DISABLE
SHOW GET pbNxt DISABLE

*laCtrStat[1]  = "DISABLE"               && First button 
*laCtrStat[4]  = "DISABLE"               && Priveus button
*-- End of lfvScope.

*!***************************************************************************
*! Name      : lpCreatTmp
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Function To Create the temp file used in browsing.
*!***************************************************************************
*! Example   : DO lfCreatTmp
*!***************************************************************************
*
PROCEDURE lpCreatTmp
CREATE TABLE (lcTempFile) ;
             (Invoice C(6), InvDate D(8) , Account C(5), Store C(8) , ;
              Order   C(6), CustPO  C(15), Rep1    C(3), Ship N(7,0), ;
              ShipAmt N(14,2), lSelInv L(1))
INDEX ON Invoice TAG (lcTempFile)
SET ORDER TO TAG (lcTempFile)
*-- End of lpCreatTmp.

*!***************************************************************************
*! Name      : lpFillField
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Function To Fill The Temp Table according to user selection
*!***************************************************************************
*! Example   : DO lpFillField
*!***************************************************************************
*
PROCEDURE lpFillField

SELECT InvHdr
LOCATE

IF TYPE('lcInvExpr') = "C"
  lcInvExpr = lcInvExpr + IIF(EMPTY(lcInvExpr),'',[ AND ]) + [Status <> "V"]
  SCAN FOR &lcInvExpr
    SCATTER MEMVAR MEMO
    m.lSelInv = .F.
    WAIT WINDOW 'Collecting Invoice# '+ m.Invoice NOWAIT  
    INSERT INTO (lcTempFile) FROM MEMVAR
  ENDSCAN
  
  SELECT ConsInvH
  LOCATE
  lcInvExpr = STRTRAN(lcInvExpr,'INVHDR.','CONSINVH.')
  SCAN FOR &lcInvExpr
    IF !SEEK(ConsInvH.Invoice,(lcTempFile))
      SCATTER MEMVAR MEMO
      m.lSelInv = .F.
      WAIT WINDOW 'Collecting Invoice# '+ m.Invoice NOWAIT  
      INSERT INTO (lcTempFile) FROM MEMVAR
    ENDIF  
  ENDSCAN
   
  SELECT (lcTempFile)
  IF EOF()
    WAIT WINDOW 'No Records Selected.' NOWAIT
  ENDIF
ELSE
  WAIT WINDOW 'No Records Selected.' NOWAIT
ENDIF  
WAIT CLEAR
*-- End of lpFillField.

*!***************************************************************************
*! Name      : lfvSelect
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Valid Function for pbSelect
*!***************************************************************************
*! Example   : =lfvSelect()
*!***************************************************************************
*
FUNCTION lfvSelect

REPLACE lSelInv WITH !lSelInv
=lfvPbSel()
lnSelRec   = IIF(lSelInv , lnSelRec + 1 , lnSelRec - 1)
lnUnSelRec = IIF(lSelInv , lnUnSelRec - 1 , lnUnSelRec + 1)
SHOW WINDOW (lcBrowTtl) REFRESH

*-- IF No records was selected
IF lnSelRec = 0
  SHOW GET pbSelNon DISABLE
  SHOW GET pbRel    DISABLE
  SHOW GET pbSelAll ENABLE
ELSE   
  SHOW GET pbSelNon ENABLE
  SHOW GET pbRel    ENABLE
  
  *-- IF All the records was selected
  IF lnUnSelRec = 0
    SHOW GET pbSelAll DISABLE
  ELSE    
    SHOW GET pbSelAll ENABLE
  ENDIF   
ENDIF    
*-- End of lfvSelect.

*!***************************************************************************
*! Name      : lfvSelAll
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Valid Function for pbSelall
*!***************************************************************************
*! Example   : =lfvSelAll()
*!***************************************************************************
*
FUNCTION lfvSelAll
IF !EOF()
  REPLACE ALL lSelInv WITH .T.
  lnSelRec   = RECCOUNT() 
  lnUnSelRec = 0
  GO lnBrRecNo
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
  SHOW WINDOW (lcBrowTtl) REFRESH
  SHOW GET pbRel    ENABLE
  SHOW GET pbSelNon ENABLE
  SHOW GET pbSelAll DISABLE
ENDIF
*-- End of lfvSelAll.

*!***************************************************************************
*! Name      : lfvSelNon
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Valid Function for pb Select None
*!***************************************************************************
*! Example   : =lfvSelNon()
*!***************************************************************************
*
FUNCTION lfvSelNon
IF !EOF()
  REPLACE ALL lSelInv WITH .F.
  lnSelRec   = 0
  lnUnSelRec = RECCOUNT() 
  GO lnBrRecNo
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
  SHOW WINDOW (lcBrowTtl) REFRESH
  SHOW GET pbRel DISABLE
  SHOW GET pbSelNon DISABLE
  SHOW GET pbSelAll ENABLE
ENDIF
*-- End of lfvSelNon.

*!***************************************************************************
*! Name      : lfvInvert
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Valid Function for pbInv
*!***************************************************************************
*! Example   : =lfvInvert()
*!***************************************************************************
*
FUNCTION lfvInvert
REPLACE ALL lSelInv WITH !lSelInv
GO lnBrRecNo
=lfvpbSel()
lnUnSelRec = lnSelRec
lnSelRec   = RECCOUNT() - lnSelRec
SHOW WINDOW (lcBrowTtl) REFRESH

*-- IF there is no selected records
IF lnSelRec = 0
  SHOW GET pbRel    DISABLE
  SHOW GET pbSelNon DISABLE
  SHOW GET pbSelAll ENABLE
ELSE   
  SHOW GET pbRel    ENABLE
  SHOW GET pbSelNon ENABLE

  *-- IF All the records was selected
  IF lnUnSelRec = 0
    SHOW GET pbSelAll DISABLE
  ENDIF    
ENDIF    
*--End of lfvInvert.

*!***************************************************************************
*! Name      : lfvpbSel
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Function for Switching 'Select And Unselect'
*!***************************************************************************
*! Example   : =lfvpbSel()
*!***************************************************************************
*
FUNCTION lfvpbSel
*-- IF The record is selected
IF lSelInv
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'
ELSE    
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'
ENDIF   
RETURN .T.
*-- End of lfvpbSel.

*!**************************************************************************
*! Name      : lfwBrows
*! Developer : Hossam El Etreby
*! Date      : 1/09/1998
*! Purpose   : When Function The Browse
*!***************************************************************************
*! Passed Parameters : None
*!***************************************************************************
*
FUNCTION lfwBrows

lnBrRecNo  = RECNO(lcTempFile)
SHOW WINDOW (lcBrowTtl) REFRESH

STORE .F. TO llEOF , llBOF

IF !EOF() .AND. !BOF()
  SKIP 1
  IF EOF()
    llEOF = .T.
    SKIP -2
    IF BOF()
      llBOF = .T.
    ELSE
      SKIP 1
    ENDIF
  ELSE
    SKIP -2
    IF BOF()
      llBOF = .T.
    ELSE
      SKIP 1
    ENDIF
  ENDIF

ELSE
  STORE .T. TO llEOF , llBOF
ENDIF

*laCtrStat[1]  = IIF(llBOF , 'DISABLE' , 'ENABLE')         && First    button 
*laCtrStat[4]  = laCtrStat[1]                              && Previous button
*laCtrStat[2]  = IIF(llEOF , 'DISABLE' , 'ENABLE')         && Last     button
*laCtrStat[3] = laCtrStat[2]                               && Next     button
*SHOW GET pbTop  &laCtrStat[1]
*SHOW GET pbPrvs &laCtrStat[4]
*SHOW GET pbBtm  &laCtrStat[2]
*SHOW GET pbNxt  &laCtrStat[3]
*-- End of lfwBrows.

*!**************************************************************************
*! Name      : lfvBrows
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Stop browse case activating another screen.
*!**************************************************************************
*! Example   : =lfvBrows()
*!**************************************************************************
*
FUNCTION lfvBrows

IF WONTOP(lcBrowTtl) 
*IF WONTOP(lcBrowTtl) .AND. lastkey() <> 27
  glFromBrow = .T.
ELSE
  = gfStopBrow()
  glFromBrow = .F.  
  IF !WVISIBLE(gcBaseWind)
  *IF WVISIBLE(gcBaseWind)  
    glQuitting = .T.
    CLEAR READ
  ENDIF  
ENDIF
*-- End of lfvBrows.

*!**************************************************************************
*! Name      : lfReadAct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Read activate function.
*!**************************************************************************
*! Example   : =lfReadAct()
*!**************************************************************************
*
FUNCTION lfReadAct
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
*-- End of lfReadAct.

*!**************************************************************************
*! Name      : lfClrTrap
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Trap Browse window.
*!**************************************************************************
*! Example   : =lfClrTrap()
*!**************************************************************************
*
FUNCTION lfClrTrap
*ACTIVATE WINDOW (lcArGma12) 
IF WONTOP() = (lcBrowTtl)
  ON KEY LABEL TAB     DO lpFld4Tab WITH (lcArGMA12),pbSelect
  ON KEY LABEL BACKTAB DO lpBackTab
ENDIF
*-- End of lfClrTrap.

*!**************************************************************************
*! Name      : lpFld4Tab
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Trap Browse window.
*!**************************************************************************
*! Example   : DO lpFld4Tab
*!**************************************************************************
*
PROCEDURE lpFld4Tab
PARAMETERS lcWindToAct,lcObject
ACTIVATE WINDOW (lcWindToAct)
_CUROBJ = OBJNUM(lcObject)
*-- End of lpFld4Tab.

*!**************************************************************************
*! Name      : lpBackTab
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Trap Browse window.
*!**************************************************************************
*! Example   : DO lpBackTab
*!**************************************************************************
*
PROCEDURE lpBackTab
lcWind = "GWCCONTRL1"
lcObj  = 'pbCls'
ACTIVATE WINDOW (lcWind)
_CUROBJ = OBJNUM(lcObj)
*-- End of lpBackTab.

*!**************************************************************************
*! Name      : lfvAccount
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Validate the entered account from Option Grid.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : = lfvAccount()
*!**************************************************************************
*
FUNCTION lfvAccount
PRIVATE lcObjVal
lcObjVal = EVALUATE(SYS(18))  && Varible to hold  the value of the current GET field

IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd,lcObjName
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER
  
  lcObjName = SYS(18)           && Varible to hold  the name of the memory variable used to create the current GET field
  
  *-- IF The user want to Browse or if the Account he/she entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal
  ENDIF 
  
  IF !(lcObjVal == lcOldVal)
    llClearInv = .T.  && Clear previous Invoice Range
    lcOldVal = lcObjVal
  ENDIF  

  SELECT CUSTOMER  
  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)
ENDIF  
*-- End of lfvAccount.

*!***************************************************************************
*! Name      : lfSRInv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : control browse Invoices for InvHdr File.
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSRInv()
*!***************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!***************************************************************************
*
FUNCTION lfSRInv
PARAMETERS lcParm
PRIVATE lnAlias

lnAlias = SELECT(0)
SELECT InvHdr

DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO TAG InvHdrA
    LOCATE
  CASE lcParm = 'R'  && Reset code
    llClearInv = .F.
    SELECT (lnAlias)
    SET ORDER TO TAG InvHdr
ENDCASE
*-- End of lfSRInv.

*!***************************************************************************
*! Name      : lfSRInv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : control browse Orders for OrdHdr File.
*!***************************************************************************
*! Called from : Option Grid
*!***************************************************************************
*! Example   : =lfSROrder()
*!***************************************************************************
*! Note      : S symbol is [S,Set- R,ReSet]
*!***************************************************************************
*
FUNCTION lfSROrder
PARAMETERS lcParm
PRIVATE lnAlias , lcCustRel
lnAlias = SELECT(0)
SELECT OrdHdr

DO CASE
  CASE lcParm = 'S'
    lcCustRel = [IIF(EMPTY(Store) , 'M' + Account,'S' + Account + Store)]
    SET RELATION TO &lcCustRel INTO CUSTOMER       && To customer file.
    LOCATE  
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET RELATION OFF INTO CUSTOMER                 && To customer file.
    SELECT (lnAlias)
ENDCASE
*-- End of lfSROrder.

*!***************************************************************************
*! Name      : lfvOk
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Validation for the OK Push button that appears in 
*!             Void Date screen.
*!***************************************************************************
*! Notes     : This function is called from the Void Date screen OK Button
*!             that appears during the voiding process of the invoice.
*!***************************************************************************
*! Example   : =lfvOk()
*!***************************************************************************
*
FUNCTION lfvOk
IF laSetups[4,2]='Y' AND ldVDate < InvHdr.dPostDate
  llVoid = .F.
  =gfModalGen("INM40163B00000" , "DIALOG")
  ldVDate   = ldOldDate
  _CurObj   = OBJNUM(ldVDate)
ELSE
  llVoid = .T.
  CLEAR READ
ENDIF
*-- End of lfvOk.
  
*!***************************************************************************
*! Name      : lfvCan
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Validation for the Cancel Push button that appears in 
*!             Void Date screen.
*!***************************************************************************
*! Notes     : This function is called from the Void Date screen Cancel Button
*!             that appears during the voiding process of the invoice.
*!***************************************************************************
*! Example   : =lfvCan()
*!***************************************************************************
*
FUNCTION lfvCan
llVoid = .F.
*-- End of lfvCan.

*!***************************************************************************
*! Name      : lfwVDate
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : When function for the void Date field in Void screen 
*!***************************************************************************
*! Notes     : This function is called from the Void Date screen that appears
*!             during the voiding process of the invoice.
*!***************************************************************************
*! Example   : =lfwVDate()
*!***************************************************************************
*
FUNCTION lfwVDate
ldOldDate = ldVDate
*-- End of lfwVDate.

*!***************************************************************************
*! Name      : lfRelScr
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Cancelling the Selected Lines
*!***************************************************************************
*! Notes     : This function is taken from ARDINV Prg with some 
*!             modifications.
*!***************************************************************************
*! Example   : =lfRelScr()
*!***************************************************************************
*
FUNCTION lfRelScr
PARAMETERS lcInvNo
EXTERNAL ARRAY laData
DECLARE laData[32]
STORE '' TO laData , lcInvFrom , lcInvTo
  
laSetups[4,1]  = 'M_LINK_GL'      &&  Check for Gl link
laSetups[8,1]  = 'M_DYELOT'       &&  Use Dylot Y Or N      
laSetups[9,1]  = 'M_TAX'          &&  use Taxes Y or N
laSetups[17,1] = 'XPOSTFINV'      &&  Post Factored invoice to customer
=gfGetMemVar(@laSetups,gcAct_Comp)

PRIVATE lcInvTag
SELECT InvHdr
lcInvTag = TAG()
SET ORDER TO TAG InvHdr

*-- Open all files needed for Voiding process.
=gfOpenFile(gcDataDir+'IcStyHst', gcDataDir+'Styhst','SH')
=gfOpenFile(gcDataDir+'ArCusHst', gcDataDir+'Acthst','SH')
=gfOpenFile(gcDataDir+'Debit', gcDataDir+'Debit','SH')
=gfOpenFile(gcDataDir+'RepComm', gcDataDir+'RepComm','SH')
=gfOpenFile(gcDataDir+'ConsInvL', gcDataDir+'ConsInvL','SH')
=gfOpenFile(gcDataDir+'ArHist', gcDataDir+'ArHistT','SH')
=gfOpenFile(gcDataDir+'Credit', gcDataDir+'Credit','SH')
=gfOpenFile(gcDataDir+'Scale',gcDataDir+'Scale','SH')
=gfOpenFile(gcDataDir+'SalesRep',gcDataDir+'SalesRep','SH')

IF laSetups[4,2]='Y'
  =gfOpenFile(gcDataDir+'GLDist', gcDataDir+'GLDistNo', 'SH')
ENDIF  

IF 'RM' $ gcCmpModules
  *-- if the return mechendise module installed 
  llRetLine = gfOpenFile(gcDataDir+'RETLINE',gcDataDir+'RETLINEI','SH')
  llRetHdr  = gfOpenFile(gcDataDir+'RETHDR',gcDataDir+'RETHDR','SH')
ENDIF
*C200352,1 HBG 19/06/2002 Open 'SHDAEXPR' File to be updated when void the invoices [Begin] 
*=gfOpenFile(gcDataDir+'SHDAEXPR',gcDataDir+'SHDAEXPR','SH')
*C200352,1 [End]
SELECT InvHdr
SET RELATION TO 'M' + Account INTO Customer

SELECT Style
SET RELATION TO 'S' + Scale INTO Scale

SELECT InvLine
SET RELATION TO Style + cWareCode + DyeLot INTO StyDye
SET RELATION TO Style INTO Style ADDITIVE

IF TYPE('lcInvNo') = "L"     && In Case of UnComp. Session.
  lcInvLine = gfTempName()
  lcInvHdr  = gfTempName()  
  lcTmpAR   = gfTempName()
  lcGlDist  = gfTempName()
  DO lfCratTemp IN (gcAppHome+gcAct_Appl+'\ARDInv')
  
  SELECT (lcTempFile)
  SCAN FOR lSelInv
    WAIT WINDOW 'Voiding Invoice# ' + &lcTempFile..Invoice NOWAIT
    =SEEK(Invoice,'InvHdr')
    laData[1]  = InvHdr.Invoice
    laData[2]  = InvHdr.Account
    laData[24] = InvHdr.cFacCode
    laData[32] = InvHdr.DiscPcnt
  
    STORE ''  TO lcSysYear , lcSysPeriod , lcCurrInv
    STORE .F. TO llReBuild , llContinue
    =SEEK('O'+InvHdr.Order,'OrdHdr')
    lnShipAddr  = IIF(OrdHdr.Alt_ShpTo,2,1)
    lcSession   = gfsequence('CSESSION')
    lcGlSession = gfsequence('GLSESSION')
    *C200352,1 HBG 19/06/2002 Update 'SHDAEXPR' File if the invoice Voided [Begin] 
    *=lfVoidInv(&lcTempFile..Invoice)
    IF lfVoidInv(&lcTempFile..Invoice)
    *  lcCurAlis = SELECT(0)
    *  IF SEEK(&lcTempFile..Invoice,'SHDAEXPR')
    *    SELECT SHDAEXPR 
    *    REPLACE cSiisVoid WITH 'Y'
    *    DELETE
    *  ENDIF
    *  SELECT (lcCurAlis)
    ENDIF
    *C200352,1 [End]
    IF llEndVoid
      EXIT 
    ENDIF
  ENDSCAN
  ZAP
  = lfDispBrow()    && Browse What U Got

ELSE
  WAIT WINDOW 'Voiding Invoice# ' + lcInvNo NOWAIT
  *=SEEK(Invoice,'InvHdr')
  laData[1]  = InvHdr.Invoice
  laData[2]  = InvHdr.Account
  laData[24] = InvHdr.cFacCode
  laData[32] = InvHdr.DiscPcnt
  
  STORE ''  TO lcSysYear , lcSysPeriod , lcCurrInv
  STORE .F. TO llReBuild
  =SEEK('O'+InvHdr.Order,'OrdHdr')
  lnShipAddr = IIF(OrdHdr.Alt_ShpTo,2,1)
  lcSession   = gfsequence('CSESSION')
  lcGlSession = gfsequence('GLSESSION')
  
  *C200352,1 HBG 19/06/2002 Update 'SHDAEXPR' File if the invoice Voided [Begin] 
  *=lfVoidInv(lcInvNo)
  IF lfVoidInv(lcInvNo)
    *lcCurAlis = SELECT(0)
    *IF SEEK(lcInvNo,'SHDAEXPR')
    *  SELECT SHDAEXPR 
    *  REPLACE cSiisVoid WITH 'Y'
    *ENDIF
    *SELECT (lcCurAlis)
  ENDIF
  *C200352,1 [End]

  IF llEndVoid
    EXIT 
  ENDIF
ENDIF  

WAIT CLEAR
*--Disable Buttons
SHOW GET pbSelect DISABLE
SHOW GET pbSelAll DISABLE
SHOW GET pbInvert DISABLE
SHOW GET pbSelNon DISABLE

IF EMPTY(lcInvFrom) AND EMPTY(lcInvTo)
  =gfModalGen('TRM00000B00000','ALERT','','','No Invoice Voided.')
  *-- <No Invoice Voided.>
  *-- Button : 00000 
  *-- <        Ok        >
ENDIF

IF !EMPTY(lcInvFrom) AND !EMPTY(lcInvTo)
  IF lcInvFrom == lcInvTo
    =gfModalGen('TRM00000B00000','ALERT','','','Invoice# ' + lcInvFrom + ' has been Voided.')
    *-- <Invoice# lcInvFrom has been Voided.>
    *-- Button : 00000 
    *-- <                         Ok                         >
  ELSE
    =gfModalGen('TRM00000B00000','ALERT','','','Invoices from ' + lcInvFrom + ' to ' + lcInvTo + ' have been Voided.')
    *-- <Invoices from lcInvFrom to lcInvTo have been Voided.>
    *-- Button : 00000 
    *-- <                         Ok                         >
  ENDIF  
ENDIF  

SELECT InvHdr
SET ORDER TO TAG (lcInvTag)
STORE .F. TO llEndVoid , llVoid , llYesToAll , llNoToAll

*-- Release all relations.
SELECT InvHdr
SET RELATION TO
SELECT InvLine
SET RELATION TO
SELECT Style
SET RELATION TO

*-- Open all files used during Voiding process.
=gfCloseFile('IcStyHst')
=gfCloseFile('ArCusHst')
=gfCloseFile('Debit')
=gfCloseFile('RepComm')
=gfCloseFile('ConsInvL')  
=gfCloseFile('ArHist')
=gfCloseFile('Credit')
=gfCloseFile('Scale')
=gfCloseFile('SalesRep')

IF laSetups[4,2]='Y'
  =gfCloseFile('GLDist')
ENDIF

IF llRetHdr
  USE IN RETHDR
ENDIF
  
IF llRetLine
  USE IN RETLINE
ENDIF

IF USED(lcInvLine)
  USE IN (lcInvLine)
ENDIF
ERASE (gcWorkDir+lcInvLine+".DBF")
ERASE (gcWorkDir+lcInvLine+".CDX")
ERASE (gcWorkDir+lcInvLine+".FPT")
IF USED(lcInvHdr)
  USE IN (lcInvHdr)
ENDIF
ERASE (gcWorkDir+lcInvHdr+".DBF")
ERASE (gcWorkDir+lcInvHdr+".CDX")
IF USED(lcTmpAR)
  USE IN (lcTmpAR)
ENDIF
ERASE (gcWorkDir+lcTmpAR+'.DBF')
ERASE (gcWorkDir+lcTmpAR+'.CDX')
*-- End of lfRelScr.

*!***************************************************************************
*! Name      : lfVoidInv
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 01/21/2001
*! Purpose   : Voiding an Invoice.
*!***************************************************************************
*! Notes     : This function is taken from ARDINV Prg with some 
*!             modifications to accept parameters (Invoice number).
*!***************************************************************************
*! Parameters : Invoice #
*!***************************************************************************
*! Example   : =lfVoidInv(Invoice)
*!***************************************************************************
*
FUNCTION lfVoidInv
PARAMETER lcInvoice

PRIVATE lcInvYear,lcInvPeriod,lcInvLines,lnAnswer

IF laSetups[17,2]='Y' AND EMPTY(laData[24]) 
  SELECT ArHist
  =SEEK(laData[2]+laData[1])
  LOCATE REST WHILE Account+Tran+cInstalNo = laData[2]+laData[1] FOR TranType='1'
  llPaid = FOUND()
  SELECT InvHdr
  IF llPaid
    *-- Message : 40130
    *-- <This invoice has already been paid. Cannot void.>
    *-- Button : 00000 
    *-- <                      Ok                        >
    =gfModalGen('TRM40130B00000','ALERT','# '+lcInvoice)
    *hbg
    *RETURN
    RETURN .F.
    *hbg
  ENDIF  
ENDIF

IF 'RM' $ gcCmpModules
  *-- seek for the invoice  in the return merchandise files 
  llRetInv  = SEEK(laData[2]+laData[1],'RETLINE') AND ;
              SEEK(RETLINE.CrMemo,'RETHDR') AND RETHDR.Status <> 'V'
  SELECT InvHdr
  IF llRetInv
    *-- Message : 40134
    *-- <'Invoice# ' + lcInvoice + ' is a return merchandise, Cannot void'>
    *-- Button : 00000 
    *-- <                                 Ok                              >
    =gfModalGen('TRM00000B00000','ALERT','','','Invoice# ' + lcInvoice + ' is a return merchandise, Cannot void')
    *hbg
    *RETURN
    RETURN .F.
    *hbg
  ENDIF
ENDIF

*-- ask the user about voiding an invoice only once.
IF !llVoid
  STORE gdSysDate TO ldVDate,ldOldDate
  *-- get the void date from the user
  DO (gcScrDir+gcWinAppl+"\ARVDATE.SPR") 
  IF !llVoid
    llEndVoid = .T.
    *hbg
    *RETURN
    RETURN .F.
    *hbg
  ENDIF

  *-- check if the void data is valid or not
  IF !CHECKPRD(ldVDate,'lcSysYear','lcSysPeriod','VI1')
    llEndVoid = .T.
    *hbg
    *RETURN
    RETURN .F.
    *hbg
  ENDIF
  
  llVoid = .T.
ENDIF  

STORE '' TO lcInvYear,lcInvPeriod
IF !CHECKPRD(InvHdr.dPostDate,'lcInvYear','lcInvPeriod','VI2')
  RETURN .F.
ENDIF

IF SEEK(InvHdr.Invoice,'InvLine')
  m.Dyelot = InvLine.Dyelot
ELSE
  *-- Message : 40002
  *-- <lines of invoice are not found>
  *-- Button : 00000 
  *-- <               Ok             >
  =gfModalGen('INM40002B00000','ALERT',lcInvoice)
  *hbg
  *RETURN
  RETURN .F.
  *hbg
ENDIF

IF InvHdr.CONSOL = 'Y' .AND. (!SEEK(InvHdr.Invoice,'ConsInvH') OR ;
   !SEEK(InvHdr.Invoice,'ConsInvL') )
  *-- Message : 40002
  *-- <lines of consolidated invoice # lcInvoice are not found>
  *-- Button : 00000 
  *-- <                          Ok                           >
  =gfModalGen('TRM00000B00000','ALERT','','','lines of consolidated invoice # ' + lcInvoice + ' are not found')
  *hbg
  *RETURN
  RETURN .F.
  *hbg
ENDIF

IF !llContinue
  SELECT (lcInvLine)
  DELETE ALL
  SELECT (lcInvHdr)
  DELETE ALL
  
  llRebuild = .T.
    
  *-- Message : 40003
  *-- <Invoice# lcInvoice is a direct invoice, Would you like to rebuild the order>
  *-- Button : 00001 
  *-- <             <Yes>      <Yes to All>      <No>     <No to all>             >  
  lnAnswer = 0
  IF InvHdr.Direct_Inv AND !llYesToAll AND !llNoToAll
    lnAnswer=gfModalGen('TRM00000B40016','ALERT','','','Invoice# ' + lcInvoice + ' is a direct invoice. Would you like to rebuild the order')
    
    DO CASE
      CASE lnAnswer = 2
        llYesToAll = .T.
      CASE lnAnswer = 4
        llNoToAll = .T.
    ENDCASE

    *-- If user selected (Yes to All)
    IF llYesToAll
      llRebuild = .T.
    ELSE        && user didn't select (Yes to All)
      *-- If user selected (No to All)
      IF llNoToAll
        llRebuild = .F.
      ELSE      && user didn't select (No to All)
        *-- If user selected (Yes)
        IF lnAnswer = 1
          llRebuild = .T.
        ELSE    && user didn't select (Yes)
          llRebuild = .F.
        ENDIF
        *-- EndIf of user selected (Yes)
      ENDIF
      *-- EndIf of user selected (No to All)
    ENDIF
    *-- EndIf of user selected (Yes to All)
  
    *-- If Answer is (Yes) or (Yes to All)
    IF lnAnswer = 1 OR lnAnswer = 2
      llRebuild = .T.
    ELSE     && Answer is (No) or (No to All)
      llRebuild = .F.
    ENDIF
  ENDIF
      
  SELECT 'UNCMSESS'
  IF SEEK('I')
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE Status     WITH 'O'        ,;
          cUTranType WITH 'DINVOICE' ,;
          cUserId    WITH gcUser_id  ,;
          cSession   WITH lcSession  ,;
          cCurrObj   WITH 'PBDLT'    ,;
          cProgram   WITH 'ARDINV'   ,;
          cCurrScr   WITH 'ARDINV'   ,;
          dTranDate  WITH ldVDate    ,;
          cTranTime  WITH TIME()
  =RLOCK()
  lcCurrInv = laData[1]
  DO lfCreatHst IN (gcAppHome+gcAct_Appl+'\ARDInv')
  lcFiles = 'lcInvHdr,'+lcInvHdr+',' +ORDER(lcTmpAR)+';lcInvLine,'+lcInvLine+',InvLine;'
  lcFiles = lcFiles + "lcTmpAR,"  + lcTmpAR+","  + ORDER(lcTmpAR)+";"
  =gfSavSess('DINVOICE', lcFiles, @laVars,lcSession)
ENDIF
DECLARE laGlArray[2,13]
STORE '' TO laGlArray
IF laSetups[4,2]='Y'
  SELECT GLDist

  *-- This checking is added to support multiple voiding [Begin]
  IF FILE(gcWorkDir+lcGLDist+'.DBF')
    ERASE gcWorkDir+lcGLDist+'.DBF'
    ERASE gcWorkDir+lcGLDist+'.CDX'    
    ERASE gcWorkDir+lcGLDist+'.DBF'
  ENDIF
  *-- This checking is added to support multiple voiding [End]

  COPY STRUCTURE TO (gcWorkDir+lcGLDist)
  =gfOpenFile(gcWorkDir+lcGlDist, '', 'EX')
  STORE 'VI'        TO laGlArray[1,4],laGlArray[2,4]
  STORE laData[1]   TO laGlArray[1,5],laGlArray[2,5]   && Invoice no
  STORE ldVDate     TO laGlArray[1,6],laGlArray[2,6]   && Void Date
  STORE lcSysYear   TO laGlArray[1,7],laGlArray[2,7]   && Year
  STORE lcSysPeriod TO laGlArray[1,8],laGlArray[2,8]   && Period
  STORE lcGLDist    TO laGlArray[1,9],laGlArray[2,9]
  STORE ''          TO laGlArray[1,11],laGlArray[2,11]
  STORE ''          TO laGlArray[1,12],laGlArray[2,12]
  STORE ''          TO laGlArray[1,13],laGlArray[2,13]
ENDIF
SET ORDER TO TAG DRTRAN IN Debit
lcUntSin = ''
lcExRSin = gfGetExSin(@lcUntSin, InvHdr.cCurrCode)

lcInvHFile = IIF(InvHdr.CONSOL='Y','ConsInvH','InvHdr')
SELECT (lcInvHFile)
SCAN REST WHILE Invoice = laData[1]
  IF !SEEK(Account+Order+Store+PikTKt,lcInvHdr)
    INSERT INTO (lcInvHdr) (Account,Order,Store,PikTKt) VALUES ;
    (&lcInvHFile..Account,&lcInvHFile..Order,&lcInvHFile..Store,&lcInvHFile..PikTkt)
  ENDIF
  IF SEEK('O'+Order,'OrdHdr')
    WAIT 'Rebuilding order# '+Order+' ...' WINDOW NOWAIT
    lnDifValue = 0
    IF &lcInvHdr..nSteps < 1
      *-- the invoice line file
      lcInvLines = IIF(InvHdr.CONSOL='Y','ConsInvL','InvLine')
      SELECT (lcInvLines)
      =SEEK(InvHdr.Invoice+IIF(InvHdr.CONSOL='Y',&lcInvHFile..Store,''))
      SCAN REST WHILE Invoice+IIF(InvHdr.CONSOL='Y',Store,'') = ;
           &lcInvHFile..Invoice+IIF(InvHdr.CONSOL='Y',&lcInvHFile..Store,'') ;
           FOR Order = &lcInvHFile..Order
        IF !SEEK(Account+Order+Store+SPACE(6)+STR(LineNo,6),lcInvLine)
          =SEEK('O'+Order+STR(LineNo,6),'OrdLine')
          SELECT ORDLINE
          =RLOCK()
          *-- update the order line quantity
          REPLACE QTY1   WITH QTY1 + &lcInvLines..QTY1 ,;
                  QTY2   WITH QTY2 + &lcInvLines..QTY2 ,;
                  QTY3   WITH QTY3 + &lcInvLines..QTY3 ,;
                  QTY4   WITH QTY4 + &lcInvLines..QTY4 ,;
                  QTY5   WITH QTY5 + &lcInvLines..QTY5 ,;
                  QTY6   WITH QTY6 + &lcInvLines..QTY6 ,;
                  QTY7   WITH QTY7 + &lcInvLines..QTY7 ,;
                  QTY8   WITH QTY8 + &lcInvLines..QTY8 ,;
                  TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
          UNLOCK
          lnDifValue = lnDifValue + ( (&lcInvLines..TotQty * &lcInvLines..Price) - ;
                                      (&lcInvLines..TotQty * Price) )
          =gfTraceKey('ORDLINE','O'+ORDLINE.Order+STR(ORDLINE.LineNo,6),'M')
          INSERT INTO (lcInvLine) (Account,Order,Store,LineNo) VALUES ;
          (&lcInvLines..Account,&lcInvLines..Order,&lcInvLines..Store,&lcInvLines..LineNo)
        ENDIF
      ENDSCAN
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF  
    IF &lcInvHdr..nSteps < 2
      SELECT ORDHDR
      =RLOCK()
      IF llRebuild
        *-- update the order header
        *-- open amount =open amount + shipped amount- difference between invine price 
        *-- and order line price
        REPLACE STATUS     WITH 'O'  ,;
                SHIP       WITH SHIP    - &lcInvHFile..SHIP,;
                SHIPAMT    WITH SHIPAMT - &lcInvHFile..SHIPAMT ,;
                OPEN       WITH OPEN    + &lcInvHFile..SHIP,;
                OPENAMT    WITH OPENAMT + &lcInvHFile..SHIPAMT - lnDifValue,;
                APPRAMT    WITH IIF((InvHdr.Consol='Y' OR (&lcInvHFile..Store <> Store)),APPRAMT+ROUND(&lcInvHFile..SHIPAMT,0),;
                                                      ROUND(&lcInvHFile..SHIPAMT,0))
      ELSE
        REPLACE CANCEL     WITH SHIP       ,;
                CANCELAMT  WITH SHIPAMT    ,;
                SHIPAMT    WITH 0          ,;
                SHIP       WITH 0          ,;
                CANCELLED  WITH ldVDate    ,;
                CCANCRESON WITH lfCanReason() ,;
                STATUS     WITH 'X'
      ENDIF
      UNLOCK
      =gfTraceKey('ORDHDR','O'+ORDHDR.Order,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
  ENDIF

  IF (!EMPTY(&lcInvHFile..Rep1) .AND. &lcInvHFile..CommAmt1 > 0) .AND. ;
    SEEK(&lcInvHFile..Rep1,'SalesRep')
    WAIT 'Charging back salesrep commissions ...' WINDOW NOWAIT
    *-- Calculate the equivlant amount
    lnEqvAmnt = -1* &lcInvHFile..CommAmt1 &lcExRSin InvHdr.nExRate ;
                    &lcUntSin InvHdr.nCurrUnit
    IF &lcInvHdr..nSteps < 3
      SELECT RepComm
      APPEND BLANK
      REPLACE STATUS     WITH 'O',;
              REPCODE    WITH &lcInvHFile..REP1   ,;
              ORDER      WITH &lcInvHFile..Order  ,;
              TRAN       WITH &lcInvHFile..Invoice,;
              DATE       WITH ldVdate,;
              TRANTYPE   WITH '6',;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo  ,;
              ACCOUNT    WITH &lcInvHFile..Account ,;
              STORE      WITH &lcInvHFile..Store   ,;
              AMOUNT     WITH lnEqvAmnt      ,;
              BALANCE    WITH SalesRep.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH -1 * &lcInvHFile..COMMAMT1  ,;
              cCurrCode  WITH InvHdr.cCurrCode,;
              nCurrUnit  WITH InvHdr.nCurrUnit,;
              nExRate    WITH InvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate,;
              cAdd_Time  WITH TIME(),;
              cAdd_User  WITH gcUser_id
     
      =gfTraceKey('RepComm',RepComm.REPCODE+DTOS(RepComm.DATE)+RepComm.TRAN+RepComm.TRANTYPE,'A')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    IF &lcInvHdr..nSteps < 4
      SELECT SalesRep
      =RLOCK()
      REPLACE CURRENT  WITH CURRENT + lnEqvAmnt,;
              BALANCE  WITH CURRENT + AGE30+AGE60+AGE90+AGE120
      UNLOCK
      =gfTraceKey('SALESREP',SalesRep.REPCODE,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 4
      UNLOCK
    ENDIF
  ENDIF
  IF (!EMPTY(&lcInvHFile..Rep2) .AND. &lcInvHFile..CommAmt2 > 0) .AND. ;
     SEEK(&lcInvHFile..Rep2,'SalesRep')
    WAIT 'Charging back salesrep commissions ...' WINDOW NOWAIT
    lnEqvAmnt = -1 * &lcInvHFile..CommAmt2 &lcExRSin InvHdr.nExRate ;
                   &lcUntSin InvHdr.nCurrUnit
    IF &lcInvHdr..nSteps < 5
      SELECT RepComm
      APPEND BLANK
      =RLOCK()
              
      REPLACE STATUS     WITH 'O'           ,;
              REPCODE    WITH &lcInvHFile..REP2   ,;
              ORDER      WITH &lcInvHFile..Order  ,;
              TRAN       WITH &lcInvHFile..Invoice,;
              DATE       WITH ldVDate     ,;
              TRANTYPE   WITH '6'           ,;
              DESC       WITH 'CHARGE BACK - VOID',;
              CUSTPO     WITH &lcInvHFile..CustPo ,;
              ACCOUNT    WITH &lcInvHFile..Account,;
              STORE      WITH &lcInvHFile..Store  ,;
              AMOUNT     WITH lnEqvAmnt     ,;
              BALANCE    WITH SalesRep.BALANCE + lnEqvAmnt,;
              nForAmnt   WITH -1 *  &lcInvHFile..COMMAMT2 ,;
              cCurrCode  WITH InvHdr.cCurrCode,;
              nCurrUnit  WITH InvHdr.nCurrUnit,;
              nExRate    WITH InvHdr.nExRate  ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()  ,;
              cAdd_User  WITH gcUser_id
      UNLOCK
      =gfTraceKey('RepComm',RepComm.REPCODE+DTOS(RepComm.DATE)+RepComm.TRAN+RepComm.TRANTYPE,'A')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 5
      UNLOCK
    ENDIF
    IF &lcInvHdr..nSteps < 6
      SELECT SalesRep
      =RLOCK()
      REPLACE CURRENT  WITH CURRENT + lnEqvAmnt,;
              BALANCE  WITH CURRENT + AGE30+AGE60+AGE90+AGE120
      UNLOCK
      =gfTraceKey('SalesRep',SalesRep.REPCODE,'M')
      SELECT (lcInvHdr)
      =RLOCK()
      REPLACE nSteps WITH 6
      UNLOCK
    ENDIF
  ENDIF
ENDSCAN
=SEEK(laData[1],'InvHdr')
=SEEK(laData[1],'InvLine')
WAIT 'Updating the Style Cut & Sold ...' WINDOW NOWAIT
IF !SEEK(InvHdr.Account+InvHdr.Order+InvHdr.Store+InvHdr.PikTKt,lcInvHdr)
  INSERT INTO (lcInvHdr) (Account,Order,Store,PikTKt) VALUES ;
  (InvHdr.Account,InvHdr.Order,InvHdr.Store,InvHdr.PikTkt)
ENDIF
lnCOGSAmt = 0
SELECT InvLine
=SEEK(InvHdr.Invoice)
SCAN REST WHILE INVOICE = InvHdr.Invoice
  IF !SEEK(Style,'Style')
    LOOP
  ENDIF
  IF !SEEK(Account+Order+Store+SPACE(6)+STR(LineNo,6),lcInvLine)
    INSERT INTO (lcInvLine) (Account,Order,Store,LineNo) VALUES ;
    (InvLine.Account,InvLine.Order,InvLine.Store,InvLine.LineNo)
  ENDIF
 
  =SEEK('O'+InvLine.Order+STR(LineNo,6),'ORDLINE')
  
  IF laSetups[8,2]='Y' .AND. Style.cDye_Flg = 'Y' .AND. ;   
    SEEK(InvLine.Style+IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode)+InvLine.Dyelot,'StyDye')

    IF llRebuild .AND. &lcInvLine..nSteps < 1
      SELECT StyDye 
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + InvLine.QTY1,;
              ORD2   WITH ORD2 + InvLine.QTY2,;
              ORD3   WITH ORD3 + InvLine.QTY3,;
              ORD4   WITH ORD4 + InvLine.QTY4,;
              ORD5   WITH ORD5 + InvLine.QTY5,;
              ORD6   WITH ORD6 + InvLine.QTY6,;
              ORD7   WITH ORD7 + InvLine.QTY7,;
              ORD8   WITH ORD8 + InvLine.QTY8,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF
    IF &lcInvLine..nSteps < 2
      SELECT StyDye
      =RLOCK()
      REPLACE SHP1   WITH SHP1 - InvLine.QTY1 ,;
              SHP2   WITH SHP2 - InvLine.QTY2 ,;
              SHP3   WITH SHP3 - InvLine.QTY3 ,;
              SHP4   WITH SHP4 - InvLine.QTY4 ,;
              SHP5   WITH SHP5 - InvLine.QTY5 ,;
              SHP6   WITH SHP6 - InvLine.QTY6 ,;
              SHP7   WITH SHP7 - InvLine.QTY7 ,;
              SHP8   WITH SHP8 - InvLine.QTY8 ,;
              TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF
    =gfTraceKey('STYDYE',StyDye.Style+StyDye.CWARECODE+StyDye.DYELOT,'M')
  ENDIF
  IF SEEK(InvLine.Style+IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode)+SPACE(10),'StyDye')
    IF llRebuild .AND. &lcInvLine..nSteps < 3
      SELECT StyDye
      =RLOCK()
      REPLACE ORD1   WITH ORD1 + InvLine.QTY1,;
              ORD2   WITH ORD2 + InvLine.QTY2,;
              ORD3   WITH ORD3 + InvLine.QTY3,;
              ORD4   WITH ORD4 + InvLine.QTY4,;
              ORD5   WITH ORD5 + InvLine.QTY5,;
              ORD6   WITH ORD6 + InvLine.QTY6,;
              ORD7   WITH ORD7 + InvLine.QTY7,;
              ORD8   WITH ORD8 + InvLine.QTY8,;
              TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    IF &lcInvLine..nSteps < 4
      SELECT StyDye
      =RLOCK()
      REPLACE SHP1   WITH SHP1 - InvLine.QTY1 ,;
              SHP2   WITH SHP2 - InvLine.QTY2 ,;
              SHP3   WITH SHP3 - InvLine.QTY3 ,;
              SHP4   WITH SHP4 - InvLine.QTY4 ,;
              SHP5   WITH SHP5 - InvLine.QTY5 ,;
              SHP6   WITH SHP6 - InvLine.QTY6 ,;
              SHP7   WITH SHP7 - InvLine.QTY7 ,;
              SHP8   WITH SHP8 - InvLine.QTY8 ,;
              TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
      UNLOCK
      SELECT (lcInvLine)
      =RLOCK()
      REPLACE nSteps WITH 4
      UNLOCK
    ENDIF
    =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.CWARECODE+STYDYE.DYELOT,'M')
  ENDIF

  SELECT Style
  =SEEK(INVLINE.Style)
  =RLOCK()
  IF llRebuild .AND. &lcInvLine..nSteps < 5
    REPLACE ORD1   WITH ORD1 + InvLine.QTY1 ,;
            ORD2   WITH ORD2 + InvLine.QTY2 ,;
            ORD3   WITH ORD3 + InvLine.QTY3 ,;
            ORD4   WITH ORD4 + InvLine.QTY4 ,;
            ORD5   WITH ORD5 + InvLine.QTY5 ,;
            ORD6   WITH ORD6 + InvLine.QTY6 ,;
            ORD7   WITH ORD7 + InvLine.QTY7 ,;
            ORD8   WITH ORD8 + InvLine.QTY8 ,;
            TOTORD WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 5
    UNLOCK
    SELECT Style
  ENDIF
  IF &lcInvLine..nSteps < 6
    REPLACE SHP1   WITH SHP1 - InvLine.QTY1 ,;
            SHP2   WITH SHP2 - InvLine.QTY2 ,;
            SHP3   WITH SHP3 - InvLine.QTY3 ,;
            SHP4   WITH SHP4 - InvLine.QTY4 ,;
            SHP5   WITH SHP5 - InvLine.QTY5 ,;
            SHP6   WITH SHP6 - InvLine.QTY6 ,;
            SHP7   WITH SHP7 - InvLine.QTY7 ,;
            SHP8   WITH SHP8 - InvLine.QTY8 ,;
            TOTSHP WITH SHP1+SHP2+SHP3+SHP4+SHP5+SHP6+SHP7+SHP8
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 6
    UNLOCK
  ENDIF
  SELECT Style
  UNLOCK
  =gfTraceKey('Style',Style.Style,'M')

  lnCOGSAmt = lnCOGSAmt + InvLine.TotQty*InvLine.Cost
  IF laSetups[4,2]='Y'
    DO GLDIST WITH InvLine.GL_Sales,'003',(InvLine.TotQty *InvLine.Price),'VI',InvLine.Invoice,;
                   ldVDate,lcSysYear,lcSysPeriod,lcGlDist,InvLine.cSalesAcnt,;
                   InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
    
    IF InvHdr.ShipAmt <> 0
      DO GLDIST WITH InvLine.GL_Sales,'005',-(InvLine.TotQty*InvLine.Price*ABS(InvHdr.Discount)/InvHdr.ShipAmt),;
                     'VI',InvLine.Invoice,ldVDate,lcSysYear,lcSysPeriod,;
                     lcGlDist,InvLine.cDiscAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                     InvHdr.nExRate
    
    ENDIF 
  ENDIF
  SELECT INVLINE
  =SEEK('O'+INVLINE.Order+STR(LINENO,6),'ORDLINE')
  IF !EMPTY(Ordline.AltStyle) .AND. (Ordline.AltStyle <> Invline.Style)
    *DO lpSwchSty WITH Ordline.Style,Ordline.AltStyle
  ENDIF
  SELECT icStyHst
  IF SEEK(INVLINE.Style+lcInvYear) .AND. &lcInvLine..nSteps < 7
    lnShipAmt = ROUND(InvLine.TotQty*InvLine.Price &lcExRSin InvHdr.nExRate ;
                    &lcUntSin InvHdr.nCurrUnit, 2)
    lnDiscount = ROUND(InvLine.TotQty*InvLine.Price*laData[32]/100 &lcExRSin InvHdr.nExRate ;
                    &lcUntSin InvHdr.nCurrUnit, 2)
    =RLOCK()
    REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - InvLine.TotQty ,;
            nSlsQty              WITH nSlsQty              - InvLine.TotQty ,;
            nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt  ,;
            nSlsAmt              WITH nSlsAmt              - lnShipAmt  ,;
            nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount ,;
            nDisAmt              WITH nDisAmt              - lnDiscount ,;
            nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - InvLine.TotQty*InvLine.Cost  ,;
            nCOGSAmt             WITH nCOGSAmt             - InvLine.TotQty*InvLine.Cost  
    UNLOCK
    =gfTraceKey('icStyHst',INVLINE.Style+lcInvYear,'M')
    SELECT (lcInvLine)
    =RLOCK()
    REPLACE nSteps WITH 7
    UNLOCK
  ENDIF

  SELECT INVLINE
  SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laInvQty
  IF laSetups[4,2]='Y'
    laGlArray[1,1]  = GL_Sales
    laGlArray[1,2]  = '008'
    laGlArray[1,3]  = -1
    laGlArray[1,10] = cCOGSAcnt

    laGlArray[2,1]  = GL_COST
    laGlArray[2,2]  = '006'
    laGlArray[2,3]  = 1
    laGlArray[2,10] = cICAcnt
  ENDIF
  
  PRIVATE lcRefer
  IF InvHdr.DIRECT_INV
    lcRefer = 'CUST# '+ Customer.Account + "-" + Customer.BTName
  ELSE
    lcRefer = 'CUST# '+ Customer.Account + " Sales order " + InvHdr.Order
  ENDIF
  =gfStyCrl('4',Style,IIF(EMPTY(Invline.cWareCode),OrdLine.cWareCode,InvLine.cWareCode),Dyelot,ldVDate,laData[1],;
            @laInvQty,Cost,lcRefer,lcGlSession,'',8,lcInvLine,'nSteps',@laGlArray,InvLine.LineNo)
ENDSCAN
SELECT arCusHst
IF SEEK(laData[2]+lcInvYear) .AND. &lcInvHdr..nSteps < 7
  lnShipAmt = ROUND(InvHdr.SHIPAMT &lcExRSin InvHdr.nExRate ;
                  &lcUntSin InvHdr.nCurrUnit, 2)
  lnDiscount = ROUND(ABS(InvHdr.DISCOUNT) &lcExRSin InvHdr.nExRate ;
                  &lcUntSin InvHdr.nCurrUnit, 2)
  =RLOCK()
  REPLACE nSlsQty&lcInvPeriod  WITH nSlsQty&lcInvPeriod  - InvHdr.SHIP ,;
          nSlsQty              WITH nSlsQty              - InvHdr.SHIP ,;
          nSlsAmt&lcInvPeriod  WITH nSlsAmt&lcInvPeriod  - lnShipAmt   ,;
          nSlsAmt              WITH nSlsAmt              - lnShipAmt   ,;
          nDisAmt&lcInvPeriod  WITH nDisAmt&lcInvPeriod  - lnDiscount  ,;
          nDisAmt              WITH nDisAmt              - lnDiscount  ,;
          nCOGSAmt&lcInvPeriod WITH nCOGSAmt&lcInvPeriod - lnCOGSAmt   ,;
          nCOGSAmt             WITH nCOGSAmt             - lnCOGSAmt
  UNLOCK
  =gfTraceKey('arCusHst',laData[2]+lcInvYear,'M')
  SELECT (lcInvHdr)
  =RLOCK()
  REPLACE nSteps WITH 7
  UNLOCK
ENDIF
SELECT InvHdr
IF laSetups[4,2]='Y'
  IF llIsEngland
    =gfOpenFile(gcDataDir+'InvChrg',gcDataDir+'InvChrg','SH')
    IF SEEK(InvHdr.Invoice,'INVCHRG')
      SELECT INVCHRG
      SCAN REST WHILE Invoice= InvHdr.Invoice
        DO GLDIST WITH InvHdr.LINK_CODE,'004',INVCHRG.nChrgAmnt,'VI',;
                  InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
                  INVCHRG.cFrgtAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
      ENDSCAN
    ENDIF
    =gfCloseFile('InvChrg')
  ELSE
    DO GLDIST WITH InvHdr.LINK_CODE,'004',(InvHdr.Freight+InvHdr.Insur+InvHdr.Cod),;
              'VI',InvHdr.Invoice,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
              InvHdr.cFrgtAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,InvHdr.nExRate
  ENDIF                 
  IF laSetups[9,2]='Y'
    DO GLDIST WITH InvHdr.LINK_CODE,'014',InvHdr.TAX_AMT+InvHdr.nPstAmt,;
                   'VI',InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,;
                   lcGlDist,InvHdr.cTaxAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                    InvHdr.nExRate
  ENDIF
  DO GLDIST WITH InvHdr.LINK_CODE,'001',-(InvHdr.TOTALCHG),'VI', ;
                 InvHdr.INVOICE,ldVDate,lcSysYear,lcSysPeriod,lcGlDist,;
                 InvHdr.cArAcnt,InvHdr.cCurrCode,InvHdr.nCurrUnit,;
                 InvHdr.nExRate  
ENDIF
SELECT InvHdr
=RLOCK()
REPLACE STATUS     WITH 'V',;
        CUSTPO     WITH '*VOID*',;
        VDATE      WITH ldVDate
REPLACE VCOMMAMT1  WITH IIF(VCOMMAMT1=0,COMMAMT1,VCOMMAMT1),;
        VCOMMAMT2  WITH IIF(VCOMMAMT2=0,COMMAMT2,VCOMMAMT2),;
        VSHIP      WITH IIF(VSHIP=0,SHIP,VSHIP),;
        VSHIPAMT   WITH IIF(VSHIPAMT=0,SHIPAMT,VSHIPAMT),;
        VCOD       WITH IIF(VCOD=0,COD,VCOD),;
        VCOD_AMT   WITH IIF(VCOD_AMT=0,COD_AMT,VCOD_AMT),;
        VINSUR     WITH IIF(VINSUR=0,INSUR,VINSUR),;
        VFREIGHT   WITH IIF(VFREIGHT=0,FREIGHT,VFREIGHT),;
        VDISCOUNT  WITH IIF(VDISCOUNT=0,DISCOUNT,VDISCOUNT),;
        VTOTALCHG  WITH IIF(VTOTALCHG=0,TOTALCHG,VTOTALCHG),;
        COMMAMT1   WITH 0,;
        COMMAMT2   WITH 0,;
        SHIP       WITH 0,;
        SHIPAMT    WITH 0,;
        COD        WITH 0,;
        COD_AMT    WITH 0,;
        INSUR      WITH 0,;
        FREIGHT    WITH 0,;
        DISCOUNT   WITH 0,;
        TOTALCHG   WITH 0 ,;
        nVPstAmt   WITH IIF(nVPstAmt=0,nPstAmt,nVPstAmt)  ,;
        nPstAmt    WITH 0        ,;
        VTAX_AMT   WITH IIF(VTAX_AMT=0,TAX_AMT,VTAX_AMT)  ,;
        TAX_AMT    WITH 0        ,;
        nvCharges  WITH IIF(nvCharges=0,nCharges,nvCharges) ,;
        nCharges   WITH 0
UNLOCK
=gfTraceKey('InvHdr',laData[1],'M')

IF InvHdr.CONSOL='Y'
  SELECT ConsInvH
  =SEEK(InvHdr.Invoice)
  SCAN REST WHILE INVOICE = InvHdr.Invoice

   *B123626,1 NNA 11/01/2004 (Begin) Stop command [Rest] Because it is doing as [Replace All]
   * REPLACE REST STATUS WITH 'V', CUSTPO WITH '*VOID*' 
    REPLACE STATUS WITH 'V' , CUSTPO WITH '*VOID*' 
   *B123626,1 NNA (End)

    =gfTraceKey('CONSINVH',INVOICE+STORE+ORDER,'M')
  ENDSCAN
ENDIF
IF laSetups[4,2]='Y' AND !SEEK(laData[1]+'VI'+lcGlSession,'GLDIST')
  WAIT 'Updating the general ledger distribution file ...' WINDOW NOWAIT
  SELECT (lcGlDist)

  SCAN
    REPLACE GLSESSION WITH lcGlSession
    =gfAdd_Info(lcGlDist)  
  ENDSCAN
  
  USE
  SELECT GLDist
  APPEND FROM (gcWorkDir+lcGlDist)
  =gfTraceKey('GLDIST',laData[1]+'VI'+lcGlSession,'A')
ENDIF

IF EMPTY(InvHdr.cFacCode) .OR. laSetups[17,2]='Y'

  SELECT Debit
  =SEEK('1'+laData[1])

  lnCreAmt = 0 
  
  *-- Scan Debit records.
  SCAN REST WHILE TRANTYPE+TRAN+CINSTALNO='1'+laData[1]
    SCATTER MEMVAR MEMO
    m.cShToOpn  = 'Y'
    
    INSERT INTO (lcTmpAr) FROM MEMVAR
    lnCreAmt = lnCreAmt - m.Amount
  ENDSCAN

  *-- Add Credit Data to master credit file[Begin]
  m.Batch     = gfsequence('BATCH')
  m.Tran = InvHdr.Invoice
  m.TranType  = 'I' 
  m.Desc      = 'CREDIT ADJ./VOID INV'
  m.Reference = 'Void Invoice'
  m.Amount    = lnCreAmt
  m.cInstalNo = ''
  
  SELECT Credit
  =RLOCK()
  INSERT INTO ('Credit') FROM MEMVAR
  =gfAdd_Info('Credit')

  =gfTraceKey('CREDIT','I'+Tran,'A')
  UNLOCK
  *-- Add Credit Data to master credit file[End  ]

  *-- Add Credit Data to temporary history file[Begin]
  =RLOCK(lcTmpAr)
  INSERT INTO (lcTmpAr) FROM MEMVAR
  UNLOCK  
  *-- Add Credit Data to temporary history file[End  ]
  
  *-- Call key off program.
  DO lfKeyOff IN (gcapphome+'ARKEYOF.PRG') WITH ;
           InvHdr.ACCOUNT,ldVDate,ABS(lnCreAmt),lnCreAmt,lcTmpAR
  SELECT CUSTOMER
  IF SEEK('M'+laData[2]) .AND. &lcInvHdr..nSteps < 8
    lnEqvAmnt = ABS(InvHdr.VTotalChg) &lcExRSin InvHdr.nExRate;
                &lcUntSin InvHdr.nCurrUnit * -1
    *=lfHgWUpdat()
    DO lfHgWUpdat IN (gcAppHome+gcAct_Appl+'\ARDInv')
    =gfTraceKey('CUSTOMER','M'+laData[2],'M')
    SELECT (lcInvHdr)
    =RLOCK()
    REPLACE nSteps WITH 8
    UNLOCK
  ENDIF
ENDIF

IF 'EB' $ gcCmpModules 
  llEdiTrans = gfOpenFile(gcDataDir+'EDITRANS',gcDataDir+'TYPEKEY','SH')
  SELECT EDITRANS
  IF SEEK('810'+PADR(InvHdr.Invoice,20)+'A'+InvHdr.Account)
    DELETE
  ENDIF
  IF SEEK('810'+PADR(InvHdr.Invoice,20)+'F'+InvHdr.cFacCode)
    DELETE
  ENDIF
  IF llEdiTrans
    USE IN 'EDITRANS'
  ENDIF
ENDIF

SELECT unCmSess
REPLACE STATUS WITH 'C'
llContinue = .F.
UNLOCK

IF EMPTY(lcInvFrom)
  lcInvFrom = lcInvoice  
ENDIF
lcInvTo = lcInvoice
*-- End of lfVoidInv.
