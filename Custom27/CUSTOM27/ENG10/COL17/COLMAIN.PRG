*!**************************************************************************
*! Name      : COLMAIN.PRG
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/18/2002
*! Purpose   : COL Custom Process Program .
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLTRIM(lcEvntFun)+'('+lcFunPars+')'

*-- Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*-- End of Program.

*!**************************************************************************
*! Name      : lfStuAdBar
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/18/2002
*! Purpose   : Add a new custom Bar to option's pad in Style Purchase Order screen.
*! Reference : C200305,1
*!**************************************************************************
*! Example   : lfStuAdBar()
*!**************************************************************************
*
FUNCTION lfStuAdBar
IF !llImpCost
  DEFINE BAR 11 OF _OPTIONPOP PROMPT '\<Labelling Instructions' SKIP FOR laScrMode[1]
ELSE
  IF llDispPric
    DEFINE BAR 14 OF _OPTIONPOP PROMPT '\<Labelling Instructions' SKIP FOR laScrMode[1]
  ELSE
    DEFINE BAR 13 OF _OPTIONPOP PROMPT '\<Labelling Instructions' SKIP FOR laScrMode[1]
  ENDIF
ENDIF

IF !USED(lcTmpLblIn)
  PRIVATE lnAlias
  lnAlias = SELECT(0)
  SELECT LBLINST
  =AFIELDS(laLblStru)
  =gfCrtTmp(lcTmpLblIn,@laLblStru,'cStyType+Po',lcTmpLblIn)
  SELECT (lcTmpLblIn)
  APPEND BLANK
  SELECT (lnAlias)
ENDIF
*--end of lfStuAdBar.

*!**************************************************************************
*! Name      : lfRunLblIn
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/18/2002
*! Purpose   : Run custom Labelling instructions screen from Option's pad
*!             in Style Purchase Order screen.
*! Reference : C200305,1
*!**************************************************************************
*! Example   : lfRunLblIn()
*!**************************************************************************
*
FUNCTION lfRunLblIn
PRIVATE lcPoAlias

lcPoAlias = ALIAS()
  
*-- Clear the trapped keys.
PUSH KEY
ON KEY

IF (BAR() = 10 .AND. llImpCost) .OR. (BAR() = 14 .AND. llDispPric) .OR. (BAR() = 13 .AND. !llDispPric)
  PUSH MENU _MSYSMENU
  RELEASE PAD _OPTIONPOP OF _MSYSMENU
  DO (gcAppHome+gcWinAppl+'\POLBLINS.FXP')
  POP MENU _MSYSMENU
ENDIF  
  
*-- Restore the trapped keys.
POP KEY
SELECT (lcPoAlias)
*-- End of lfRunLblIn.

*!**************************************************************************
*! Name      : lfSavLblIn
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/18/2002
*! Purpose   : Save data to LBLINST file.
*! Reference : C200305,1
*!**************************************************************************
*! Example   : lfSavLblIn()
*!**************************************************************************
*
FUNCTION lfSavLblIn

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpLblIn)
SCATTER MEMO MEMVAR
SELECT LBLINST
IF SEEK(ALLTRIM(lcAType)+laData[1])
  GATHER MEMO MEMVAR
ELSE
  APPEND BLANK
  GATHER MEMO MEMVAR
  REPLACE CSTYTYPE WITH ALLTRIM(lcAType),;
          PO       WITH laData[1]
ENDIF
SELECT (lnAlias)
*-- End of lfSavLblIn.

*!**************************************************************************
*! Name      : lfGetLblIn
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/18/2002
*! Purpose   : Get data From LBLINST file.
*! Reference : C200305,1
*!**************************************************************************
*! Example   : lfGetLblIn()
*!**************************************************************************
*
FUNCTION lfGetLblIn

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT LBLINST
IF laScrMode[2]
  SEEK ALLTRIM(lcAType)+laData[1]
  SCATTER MEMO MEMVAR
ELSE
  SCATTER MEMO MEMVAR BLANK
ENDIF
SELECT (lcTmpLblIn)
GATHER MEMO MEMVAR
SELECT (lnAlias)
*-- End of lfGetLblIn.

*!**************************************************************************
*! Name      : AddOptn                     (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : Add a new Option to the  menu Bar
*!**************************************************************************
*! Example   : lfAddOptn()
*!*************************************************************
*!
FUNCTION lfADDOPTN

SET SKIP OF PAD _OPTION OF _MSYSMENU (.F.)
DEFINE BAR 2 OF _OPTIONPOP PROMPT 'Amend material costs'  SKIP FOR laScrMode[2]
ON SELECTION BAR 2 OF _OPTIONPOP DO lpDispScr IN COLMAIN
*-End 


*!**************************************************************************
*! Name      : lpDispScr            (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : Display Screen to show the total costs for the shipment.
*!*************************************************************
*! Example   : DO lpDispScr
*!*************************************************************
*!
PROCEDURE lpDispScr

lcTmpFil  = gfTempName()
=lfCrtTmp()
lnBrTrNo = 0
lcBrTrttl = "Total Costs"
=lfColctDat()
SELECT (lcTmpFil)
lcOkBmp    = gcBmpHome + "Ok.bmp"
lcCanBmp   = gcBmpHome + "Can.bmp"
lcEscInt = ON('KEY','ESC')
lcTabInt = ON('KEY','TAB')
lcHldBtb = ON('KEY','BACKTAB')    
lcAltB   = ON('KEY','ALT+B')    

DO (gcScrDir+"PO\POEDUCS.SPX")

ON KEY LABEL ESC      &lcEscInt
ON KEY LABEL TAB      &lcTabInt
ON KEY LABEL BACKTAB  &lcHldBtb
ON KEY LABEL ALT+B    &lcAltB

*!*************************************************************
*! Name      : lfLCCrTrp        (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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
*! Example   : =lfLCCrTrp()
*!*************************************************************

FUNCTION lfLCCrTrp

*-- THIS is function is called in activate snippet of the screen
*-- if the screen on top is not the browse screen restore 
*-- the previous on key label 

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ESC

*!*************************************************************
*! Name      : lfTrBrTab                      (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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
*! Example   : =lfBrTab()
*!*************************************************************

FUNCTION lfTrBrTab

ON KEY LABEL TAB
ACTIVATE WINDOW PoEdUcs2
_CUROBJ = OBJNUM(pbCan)

*!*************************************************************
*! Name      : lfTrBrBack  (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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
*ON KEY LABEL BACKTAB
*IF WONTOP() = (lcBrTrttl)
  _CUROBJ = OBJNUM(pbCan)
*ENDIF

*!*************************************************************
*! Name      : lfTrap             (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : TO Assign functions to some keys to not affect the browse
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfBrTab,lfBrBack
*!*************************************************************
*! Passed Parameters  : NONE 
*!*************************************************************
*! Returns            : NONE
*!*************************************************************
*! Example   : =lfTrnTrap()
*!*************************************************************

FUNCTION lfLCTrnTrp

*-- This function is called in deactivate snippet of the screen
*-- if the screen on top is the browse screen assign fuction to the key

IF WONTOP()  = lcBrTrttl
  ON KEY LABEL TAB     DO lfTrBrTab
  ON KEY LABEL BACKTAB DO lfTrBrBack
  ON KEY LABEL ESC     DO lfTrBrEsc
ENDIF
RETURN .F.


*!*************************************************************
*! Name      : lfTrBrEsc               (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
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

ACTIVATE WINDOW PoEdUcs2
_CUROBJ = OBJNUM(pbCan)
KEYBOARD "{ENTER}" CLEAR


*!*************************************************************
*! Name      : lfLcTrnBrw                 (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : Browse the Shipment records
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

FUNCTION lfLcTrnBrw

lnCurAlias = SELECT(0)

lcBrCost = "cMarkr = IIF(RECNO()=lnMarkB,'>',' '):H=' ':R:1,"+;
           "Item       :H= 'Material'   :R:20 ,"+;
           "IClr       :H= 'Color'      :R:9  ,"+;
           "Desc       :H= 'Description':R:16 ,"+;
           "UntCost    :H= 'Cost' :P='999999999.999':13 "
             
SELECT (lcTmpFil)
LOCATE

BROWSE FIELDS &lcBrCost;
   WINDOW 'PoEdUcs1'     ;
   IN WINDOW 'PoEdUcs' ;
   WHEN lfwTrnBrow() ;
   LOCK 0            ;
   NOAPPEND          ;
   NOCLEAR           ;
   NODELETE          ;
   NOMENU            ;   
   SAVE              ;
   TITLE lcBrTrttl   ;
   NOWAIT

SELECT (lnCurAlias)               
*!*************************************************************
*! Name      : lfCrtTmp                 (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : Create Temp File 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfCrtTmp()
*!*************************************************************

FUNCTION lfCrtTmp
CREATE Table (gcWorkDir+lcTmpFil) (Item C(19),IClr C(6),Desc C(20),UntCost N(11,3))
INDEX ON Item+Iclr TAG (lcTmpFil)

*!*************************************************************
*! Name      : lfwTrnBrow             (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : When Function for browse 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfwTrnBrow()
*!*************************************************************
*!
FUNCTION lfwTrnBrow
lnCurAlias = SELECT(0)
SELECT (lcTmpFil)
lnBrTrNo = RECNO()
*--initialize fields
SELECT (lnCurAlias)
SHOW WINDOW (lcBrTrttl) REFRESH

*--End
*!*************************************************************
*! Name      : lfColctDat                 (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : Collect needed Data 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfColctDat()
*!*************************************************************
*!
FUNCTION lfColctDat

lnAlias = SELECT()
SELECT (lcTmpBom)
SCAN 
  IF &lcTmpBom..cCatgTyp = 'F' OR &lcTmpBom..cCatgTyp = 'T'
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTmpFil) FROM MEMVAR
  ENDIF
ENDSCAN
SELECT(lnAlias)

*!*************************************************************
*! Name      : lfvOK                (C200303,1)
*! Developer : Hend Ghabem (HBG)
*! Date      : 13/03/2002
*! Purpose   : Validation of Ok bottun
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : = lfvOK()
*!*************************************************************
*!
FUNCTION lfvOK

SELECT (lcTmpBom)
SET RELATION TO Item + Iclr INTO (lcTmpFil) ADDITIVE
SCAN 
  IF &lcTmpBom..cCatgTyp = 'F' OR &lcTmpBom..cCatgTyp = 'T'
    REPLACE &lcTmpBom..UntCost WITH &lcTmpFil..UntCost,;
            &lcTmpBom..TotCost WITH ROUND(&lcTmpBom..nbomtotqty * &lcTmpBom..UntCost , 2),;
            &lcTmpBom..cStatus WITH IIF(&lcTmpBom..cStatus = 'A',&lcTmpBom..cStatus,'M')
  ENDIF
ENDSCAN
LOCATE

CLEAR READ

*!**************************************************************************
*! Name      : lfColAdBar
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/27/2002
*! Purpose   : Add a new custom Bar to option's pad in Style screen.
*! Reference : C200307,1
*!**************************************************************************
*! Example   : lfColAdBar()
*!**************************************************************************
*
FUNCTION lfColAdBar
DEFINE BAR 10 OF _lPopOpt PROMPT '\<Assign coded notes' SKIP FOR laScrMode[1]

IF !USED(lcTmpStyNt)
  PRIVATE lnAlias,lnArrLen
  lnAlias = SELECT(0)
  SELECT STYCDNOT
  =AFIELDS(laLblStru)
  lnArrLen = ALEN(laLblStru,1)
  DECLARE laLblStru[lnArrLen+1,4]
  laLblStru[lnArrLen+1,1] = 'CNOTE'
  laLblStru[lnArrLen+1,2] = 'C'
  laLblStru[lnArrLen+1,3] = 65
  laLblStru[lnArrLen+1,4] = 0

  lnArrLen = ALEN(laLblStru,1)
  DECLARE laLblStru[lnArrLen+1,4]
  laLblStru[lnArrLen+1,1] = 'CFLAG'
  laLblStru[lnArrLen+1,2] = 'C'
  laLblStru[lnArrLen+1,3] = 1
  laLblStru[lnArrLen+1,4] = 0

  =gfCrtTmp(lcTmpStyNt,@laLblStru,'cStyMajor+Code',lcTmpStyNt)
  SELECT (lnAlias)
ENDIF
*--end of lfColAdBar.

*!**************************************************************************
*! Name      : lfRunStyNt
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/27/2002
*! Purpose   : Run custom Assign coded notes screen from Option's pad
*!             in Style Purchase Order screen.
*! Reference : C200307,1
*!**************************************************************************
*! Example   : lfRunStyNt()
*!**************************************************************************
*
FUNCTION lfRunStyNt
PRIVATE lcStyAlias

lcStyAlias = ALIAS()
  
*-- Clear the trapped keys.
PUSH KEY
ON KEY

IF BAR() = 10
  PUSH MENU _MSYSMENU
  RELEASE PAD _OPTIONPOP OF _MSYSMENU
  DO (gcAppHome+gcWinAppl+'\ICSTYCDN.FXP')
  POP MENU _MSYSMENU
ENDIF  
  
*-- Restore the trapped keys.
POP KEY
SELECT (lcStyAlias)
*-- End of lfRunStyNt.

*!**************************************************************************
*! Name      : lfSavStyNt
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/27/2002
*! Purpose   : Save data to STYCDNOT file.
*! Reference : C200307,1
*!**************************************************************************
*! Example   : lfSavStyNt()
*!**************************************************************************
*
FUNCTION lfSavStyNt

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpStyNt)
SCAN
  SCATTER MEMVAR
  SELECT STYCDNOT
  IF SEEK(m.cStyMajor+m.Code)
    IF m.cFlag = 'D'
      DELETE
    ELSE
      GATHER FIELDS CSTYMAJOR,CODE MEMVAR
    ENDIF
  ELSE
    IF m.cFlag # 'D'
      APPEND BLANK
      GATHER FIELDS CSTYMAJOR,CODE MEMVAR
    ENDIF
  ENDIF
ENDSCAN
SELECT (lnAlias)
*-- End of lfSavStyNt.

*!**************************************************************************
*! Name      : lfGetStyNt
*! Developer : Ahmed Maher (AMH)
*! Date      : 03/27/2002
*! Purpose   : Get data From STYCDNOT file.
*! Reference : C200307,1
*!**************************************************************************
*! Example   : lfGetStyNt()
*!**************************************************************************
*
FUNCTION lfGetStyNt

PRIVATE lnAlias
lnAlias = SELECT(0)
SELECT (lcTmpStyNt)
DELETE ALL
IF laScrMode[2] .AND. SEEK(lcMajor,'STYCDNOT')
  SELECT STYCDNOT
  m.cFlag = 'A'
  SCAN REST WHILE CSTYMAJOR+CODE = lcMajor
    SCATTER MEMVAR
    INSERT INTO (lcTmpStyNt) FROM MEMVAR
  ENDSCAN
ENDIF
SELECT (lnAlias)
*-- End of lfGetStyNt.