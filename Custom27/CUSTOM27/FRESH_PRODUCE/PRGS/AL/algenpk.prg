*:***************************************************************************
*: Program file  : ALGENPK
*: Program desc. : Generate Picking Tickets
*: For screen    : ALGENPK.SCX
*:        System : Aria Advantage Series.
*:        Module : Allocation (AL)
*:        Date   : 07/19/1999
*:     Developer : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:     Procedures : 
*:     Functions  : 
*:***************************************************************************
*: Example : DO ALGenPk
*:***************************************************************************
*: Due to C200084,1 [Fresh Produces Generate Picking tickets Screen]
*:***************************************************************************
*: Modifications :
*B603111,1 MAB 08/12/1999 1- Only one record for the same pick ticket.
*B603111,1 MAB 08/12/1999 2- Generate New/Add Message if pick ticket
*                            created in previous session only.
*
*MAB 10/16/1999 Filter on Specific pick type only.
*C200111,1 TAK 02/05/2000 Added to release the allocated line if line ratio failed.
*:***************************************************************************
PARAMETERS lcPassType

*--
IF !gfSetup()
  RETURN
ENDIF  

SELECT ORDHDR
GO TOP
IF EOF()
  *** Message : "There is no records in the ð file can not proceed."
  ***           "                     < Ok >                       "
  =gfModalGen("TRM44010B00000" , "DIALOG" , "Order Header")
  RETURN
ENDIF  

*-- MAB 10/16/1999 Filter on Specific pick type only.
IF TYPE('lcPassType') $ "UL"
  lcPassType = "B"
ENDIF

*-- Save Setup Parameters in memvar [Begin]
DIMENSION laSetups[6,2]
laSetups[1,1] = "M_MAAP"
laSetups[2,1] = "M_CLIR"
laSetups[3,1] = "M_BLANKS"
laSetups[4,1] = "M_DESIGNS"
laSetups[5,1] = "M_NMDROPS"
laSetups[6,1] = "M_DESDROPS"
=gfGetMemVar(@laSetups,gcAct_Comp)
lnCmpMnAlo = laSetups[1,2]  && Company Min. Allowable allocation Percentage.
llCmpRatio = laSetups[2,2]  && Company Line item ratio. 
lnBlankBk  = laSetups[3,2]  && Blanks Min. Back Order Quantity.
lnDesignBk = laSetups[4,2]  && Designs only Min. Back Order Quantity.
lnNmDropBk = laSetups[5,2]  && Name Drops only Min. Back Order Quantity.
lnDesDrpBk = laSetups[6,2]  && Designs and Name Drops Min. Back Order Quantity.
*-- Save Setup Parameters in memvar [End  ]


*-- Open Required Files [Begin]
=gfOpenFile(gcDataDir+"ORDLINE","ORDLINE",'SH')
=gfOpenFile(gcDataDir+"STYLE","STYLE",'SH')
=gfOpenFile(gcDataDir+"STYDYE","STYDYE",'SH')
=gfOpenFile(gcDataDir+"SCALE","SCALE",'SH')
=gfOpenFile(gcDataDir+"BOMVAR","BOMVAR",'SH')
*-- Open Required Files [End  ]

*-- Define Screens Variables and Temporary file [Begin]
lcGenPkCh1 = gfTempName()  && Child window 1 (For Browse).
lcGenPkCh2 = gfTempName()  && Child window 2 (For Controls).
lcTmpGenPk = gfTempName()  && Temporary File Name.
lcOrdUniqu = gfTempName()  && Unique Tag for Orders.

*-- MAB 10/16/1999 Filter on Specific pick type only.
*lcPickBrow = 'Orders'
lcPickBrow = 'Blank Orders'
*-- Define Screens Variables and Temporary file [End  ]

*B603111,1 Create Temp. File hold piktkts generated in this session [Begin]
lcTmpPkTk = gfTempName()
CREATE CURSOR (lcTmpPkTk) (Order  C(6), Store C(8), cPickType C(1),;
                           PikTkt C(6))
ZAP
INDEX ON Order + Store + cPickType TAG (lcTmpPkTk) OF ;
         (gcWorkDir+lcTmpPkTk+'.CDX')
*B603111,1 Create Temp. File hold piktkts generated in this session [End  ]

*-- Create Temporary File... [Begin]
DIMENSION laTempStru[1,4] , laHoldSzs[1,3]
laHoldSzs = ''

SELECT ORDLINE
lnFldsCnt =AFIELDS(laTempStru)
DIMENSION laTempStru[lnFldsCnt+7,4]

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

CREATE TABLE (gcWorkDir+lcTmpGenPk) FROM ARRAY laTempStru
ZAP

*-- MAB 10/16/1999 Filter on Specific pick type only. [Begin]
*INDEX ON Order TAG (lcOrdUniqu) OF ;
*         (gcWorkDir+lcTmpGenPk+'.CDX') UNIQUE
INDEX ON Order + cPickType FOR cPickType = lcPassType TAG (lcOrdUniqu) OF ;
                                       (gcWorkDir+lcTmpGenPk+'.CDX') UNIQUE

*INDEX ON Order+Store+cPickType+STR(LineNo,6) TAG (lcTmpGenPk) OF ;
*         (gcWorkDir+lcTmpGenPk+'.CDX')
INDEX ON Order+Store+cPickType+STR(LineNo,6) FOR cPickType = lcPassType TAG (lcTmpGenPk) OF ;
         (gcWorkDir+lcTmpGenPk+'.CDX')
*-- MAB 10/16/1999 Filter on Specific pick type only. [End  ]

*-- Create Temporary File... [End  ]

*-- lcSelSt    : < Select >     Status
*-- lcSelAllSt : < Select All > Status
*-- lcSelNonSt : < Select Non > Status
*-- lcInvertSt : < Invert >     Status
*-- lcPickSt   : < Pick >       Status
STORE 'DISABLE' TO lcSelSt,lcSelAllSt,lcSelNonSt,lcInvertSt,lcPickSt

*-- lnBrRecNo  : Browse Record Number
*-- ldFromDate : Start Ship From date.
*-- ldToDate   : Start Ship To date.
*-- ldOldFrom  : Old Start From date.
*-- ldOldTo    : Old Start To   date.
*-- lnSelRecs  : No. of selected records (Zero initially)
*-- lnAllRecs  : All Orders to pick (Selected and Un-Selected)
STORE gdSysDate     TO ldFromDate , ldOldFrom
STORE gdSysDate + 7 TO ldToDate   , ldOldTo
STORE 0 TO lnBrRecNo , lnSelRecs  , lnAllRecs

*-- llDateChng : Set this flag when user change any side of date range.
llDateChng = .T.

SELECT(lcTmpGenPk)

PUSH KEY

ON KEY LABEL ALT+B ACTIVATE WINDOW (lcPickBrow)
ON KEY LABEL Alt+L DO lfvSelect WITH "SEL_UNSEL"
ON KEY LABEL Alt+A DO lfvSelect WITH "SEL_ALL"
ON KEY LABEL Alt+N DO lfvSelect WITH "SEL_NON"
ON KEY LABEL Alt+I DO lfvSelect WITH "INVERT"
ON KEY LABEL Alt+P DO lfvGenPick
ON KEY LABEL Alt+S DO lfvShpDate
ON KEY LABEL Alt+C DO lfvClose

DO (gcScrDir+gcWinAppl+'\ALGENPK.SPX')

*-- Screen Cleanup Code.
glQuitting = .T.  && Rise quit flag because it's modal screen.

POP KEY
RELEASE WINDOW (lcPickBrow)
IF USED(lcTmpGenPk)
  USE IN (lcTmpGenPk)
ENDIF

IF FILE(gcWorkDir+lcTmpGenPk+'.DBF')
  ERASE &gcWorkdir.&lcTmpGenPk..DBF          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+lcTmpGenPk+'.CDX')
  ERASE &gcWorkdir.&lcTmpGenPk..CDX          && Erase the Temp file.
ENDIF

IF FILE(gcWorkDir+lcTmpGenPk+'.FPT')
  ERASE &gcWorkdir.&lcTmpGenPk..FPT          && Erase the Temp file.
ENDIF

*B603111,1 CLOSE piktkts generated in this session [Begin]
IF USED(lcTmpPkTk)
  USE IN (lcTmpPkTk)
ENDIF
*B603111,1 CLOSE piktkts generated in this session [End  ]
*-- end of program code.


**********************************************************************
**************** Control Browse and trapping Functions ***************
**********************************************************************

*!*************************************************************
*! Name      : lfDispBrow
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : Function to create the Browse
*!*************************************************************
*
FUNCTION lfDispBrow

*-- Release browse window if found.
IF WEXIST(lcPickBrow)
  RELEASE WINDOW (lcPickBrow)
ENDIF

SELECT (lcTmpGenPk)
lnBrRecNo  = RECNO()

lcBrowFild = "lcMarker=IIF(RECNO()=lnBrRecNo , '>' , ' ') :1 :H= ' ' :W= .F. ,"+;
             "lcSelect=IIF(lSelRec , '»' , ' ') :R :H= '»' ,"+;
             "ORDER     :R :H= 'Order'     :8  ,"+;
             "ACCOUNT   :R :H= 'Account'   :10 ,"+;
             "Start     :R :H= 'Start Date' ," +;
             "Complete  :R :H= 'Complete Date' ," +;
             "nNetOpen  :R :H= 'Open Qty.' ," +;
             "nStillAlo :R :H= 'Allocated Qty.' ," +;
             "nAlloPer = (nStillAlo/nNetOpen) * 100 :R :H='Allocated % ' :P='999.99',"+;
             "Type ='Blank' :H='Type  ' :R "

BROWSE FIELDS &lcBrowFild     ;
       WINDOW (lcGenPkCh1)    ;
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
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : When Browse Function.
*!*************************************************************
*
FUNCTION lfwBrows
lnBrRecNo  = RECNO(lcTmpGenPk)
=lfvpbSel()
*-- end of lfwBrows.

*!*************************************************************
*! Name      : lfvBrow
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
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
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : When function of the invisible button IbBrow
*!*************************************************************
*! Return      : .T. or .F.
*!*************************************************************
*
FUNCTION lfwIbBrow
*IF The left mouse button is not pressed
IF !MDOWN()
  KEYBOARD "{ALT+B}" CLEAR 
  RETURN .T.
ENDIF    && End of IF
RETURN .F.
*-- end of lfwIbBrow.

*!*************************************************************
*! Name      : lfBrowTrap
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
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
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : Function to untrap the keys for the Browse
*!*************************************************************
*
FUNCTION lfBrwUnTrp

*IF The window on top is not the Browse and coming from the Browse
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
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : Trap (Tab / Backtab) Keys
*!*************************************************************
*
FUNCTION lfTraps
PARAMETERS lcTrap
ACTI WINDOW (lcGenPkCh2) TOP
*-- Tab Case
IF lcTrap = "TAB"
  _CUROBJ = OBJNUM(pbSelect)

ELSE  && Backtab Case.
  _CUROBJ = OBJNUM(pbClose)

ENDIF
*-- end of lfTraps.

**********************************************************************
********************** Control Screen Functions **********************
**********************************************************************

*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
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
IF (lnAllRecs<>0) AND !EOF(lcTmpGenPk) AND (lnBrRecNo<>0) AND ;
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
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Select / Unselect Record.
*!*************************************************************
*!
FUNCTION lfSelUnSel
REPLACE lSelRec WITH !lSelRec
lnSelRecs = lnSelRecs + IIF(lSelRec,1,-1)
*-- end of lfSelUnSel.

*!*************************************************************
*! Name      : lfRefScr
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : Refresh screen Controls
*!*************************************************************
*
FUNCTION lfRefScr
IF lnAllRecs = 0
  STORE 'DISABLE' TO lcSelSt,lcSelAllSt,lcSelNonSt,lcInvertSt,lcPickSt
ELSE
  STORE "ENABLE" TO lcSelSt , lcInvertSt
  STORE IIF(lnSelRecs = 0, "DISABLE", "ENABLE") TO lcPickSt , lcSelNonSt
  lcSelAllSt = IIF(lnSelRecs = lnAllRecs, "DISABLE", "ENABLE")
ENDIF

ACTI WINDOW (lcGenPkCh2) TOP
SHOW GET pbSelAll &lcSelAllSt
SHOW GET pbSelNon &lcSelNonSt
SHOW GET pbInvert &lcInvertSt
SHOW GET pbPick   &lcPickSt
=lfvpbSel()
*-- end of lfRefScr.

*!*************************************************************
*! Name      : lfvpbSel
*! Developer : MAB (Mohamed Atia Badran)
*! Date      : 07/20/1999
*! Purpose   : if Current status is Select prompt is Un-Select and vice versa
*!*************************************************************
*
FUNCTION lfvpbSel
IF lSelRec
  SHOW GET pbSelect,1 PROMPT 'UnSe\<lect'  &lcSelSt
ELSE    && Else
  SHOW GET pbSelect,1 PROMPT 'Se\<lect'    &lcSelSt
ENDIF
SHOW WINDOW (lcPickBrow) REFRESH
*-- end of lfvpbSel.

*!*************************************************************
*! Name      : lfvShpDate
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : <Start date> button validation.
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*!
FUNCTION lfvShpDate

*-- do not forget to do it in best way.
PUSH KEY       && Save Current active Keys on the stack.
=lfClearKey()  && Deactive all keys defined on this screen.

ON KEY LABEL Alt+C DO lfBtnDatCn
ON KEY LABEL Alt+O DO lfBtnDatOk

ldOldFrom = ldFromDate
ldOldTo   = ldToDate
llOkPress = .F.

DO (gcScrDir+gcWinAppl+'\ALDATRNG.SPX')  && Date Range Screen.

ON KEY LABEL Alt+C
ON KEY LABEL Alt+O

POP KEY        && Restore last saved Keys from the stack.

IF llOkPress
  =lfDispBrow()  && Control Browse and screen controls.
  =lfRefScr()
  _CUROBJ = OBJNUM(ibBrow)
ENDIF  

*-- end of lfvShpDate.

*!*************************************************************
*! Name      : lfClearKey
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
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
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ALT+B
*-- end of lfClearKey.

*!*************************************************************
*! Name      : lfBtnDatCn
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Start date <Cancel> Button Pressed Valid Function
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*!
FUNCTION lfBtnDatCn
*-- Restore original values.
ldFromDate = ldOldFrom
ldToDate   = ldOldTo
SHOW GET ldFromDate
SHOW GET ldToDate
*-- end of lfBtnDatCn.

*!*************************************************************
*! Name      : lfBtnDatOk
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Start date <Ok> Button Pressed Valid Function
*!*************************************************************
*! Passed Parameters  : ...
*!*************************************************************
*!
FUNCTION lfBtnDatOk

llDateChng = llDateChng OR (ldFromDate != ldOldFrom) OR ;
                           (ldToDate != ldOldTo)

*-- if user change any date bounds and lower value 
*-- was greater than upper.
IF llDateChng AND ldFromDate > ldToDate
  WAIT WINDOW "Lower date must be less than or equal Higher date." TIMEOUT 2
  =lfBtnDatCn()    && Do Cancel Function.
  _CUROBJ = OBJNUM(ldFromDate)
  RETURN
ENDIF

*-- Start Business Rules ...........................
*----------------------- ...........................
CLEAR READ
*-- if user change date bounds, then apply business Rules.
IF llDateChng

  *-- llUpdtFrst : Update first line.
  *-- lnAlocPcs  : No. of Allocated Pieces.
  *-- lnNetOpen  : No. of actually open pieces.
  PRIVATE llUpdtFrst , lnAlocPcs , lnNetOpen
  llDateChng = .F.
  lnSelRecs  = 0
  lnAllRecs  = 0
  
  SELECT (lcTmpGenPk)
  IF RECCOUNT() > 0
    ZAP
  ENDIF
  
  =lfSetRelat()  && Set Relation between files.

  SELECT ORDHDR
  =SEEK("O")
  SCAN REST WHILE cOrdType + Order = "O" ;
              FOR BETWEEN(Start,ldFromDate,ldToDate) AND Status = 'O'
    
    *-- MAB 10/16/1999 Filter on Specific pick type only. [Begin]
    STORE 0 TO lnAlocPcs , lnNetOpen
    *lnAlocPcs  = nNetAloQty
    *lnNetOpen  = lfNetOpnAl()
    =lfNetOpnAl(@lnAlocPcs , @lnNetOpen)
    *-- MAB 10/16/1999 Filter on Specific pick type only. [End  ]
    
    llUpdtFrst = .F.

    WAIT WINDOW "Processing Order # " + Order NOWAIT
    =lfBusiness()
    
    *-- Update first line.
    SELECT (lcTmpGenPk)
    IF llUpdtFrst AND SEEK(OrdHdr.Order)
      SET ORDER TO (lcOrdUniqu)
      =SEEK(OrdHdr.Order)
      
      REPLACE nStillAlo WITH lnAlocPcs    ,;
              nNetOpen  WITH lnNetOpen    ,;
              Start     WITH OrdHdr.Start ,;
              Complete  WITH OrdHdr.Complete
      lnAllRecs = lnAllRecs + 1  && Increase all order records counter.
    
      *-- If 100% Percenage to be picked, Record will selected by default.
      IF lnAlocPcs = lnNetOpen
        REPLACE lSelRec WITH .T.
        lnSelRecs = lnSelRecs + 1  && Increase Selected Records counter.
      ENDIF
      
      SET ORDER TO (lcTmpGenPk)
    ENDIF 

  ENDSCAN
  WAIT CLEAR
  =lfReSetRel()  && ReSet Relation between files.
  SELECT (lcTmpGenPk)
  SET ORDER TO (lcOrdUniqu)
  GO TOP
  llOkPress = .T.
ENDIF
*-- end of lfBtnDatOk.

*!*************************************************************
*! Name      : lfSetRelat
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Set Relation between files.
*!*************************************************************
*!
FUNCTION lfSetRelat
SELECT STYLE
SET RELATION TO "S" + scale INTO Scale

SELECT ORDLINE
SET RELATION TO Style INTO Style ,;
                "SO" + Order + STR(LineNo,6) INTO Bomvar
*-- end of lfSetRelat.

*!*************************************************************
*! Name      : lfReSetRel
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : ReSet Relation between files.
*!*************************************************************
*!
FUNCTION lfReSetRel
SELECT STYLE
SET RELATION TO

SELECT ORDLINE
SET RELATION TO
*-- end of lfReSetRel.

**********************************************************************
************************ Business Rules Section **********************
**********************************************************************

*!*************************************************************
*! Name      : lfBusiness
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Apply Business Rules Function.
*!*************************************************************
*!
FUNCTION lfBusiness

************************* FIRST ORDER RULES ******************************
*-- Rule # 1 (ThreShold OK Percentage)
*-------------------------------------
llPassRule = .F.

IF lfThrShold("O")

  *-- Line Item Ratio Rules...
  *-- Rule # 2 (Company line item ratio)
  *-- Rule # 3 (It is not back order and Order header line item 
  *--           ratio was set)
  IF lfCmpRatio() AND !lfBackOrd() AND lfOrdRatio()
    ***************************** LINE RULES *********************
    =lfApplyLns()
    llUpdtFrst = .T.
  ELSE
    llPassRule = .T.
  ENDIF

  ************************* SECOND ORDER RULES *****************
  *-- Rule # 5 (Threshold again to the new quantity)
  =SEEK(Order , lcTmpGenPk)
  IF lfThrShold("O")
    IF llPassRule
      =lfAddLines()
    ENDIF
    llUpdtFrst = .T.
  ELSE  && Does not meet threshold ok again
    IF !(llPassRule OR EOF(lcTmpGenPk))
      SELECT (lcTmpGenPk)
      =SEEK(OrdHdr.Order)
      DELETE FOR Order = OrdHdr.Order
    ENDIF  
  ENDIF
ENDIF  
*-- end of lfBusiness.

*!*************************************************************
*! Name      : lfApplyLns
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Rules on lines
*!*************************************************************
*!
FUNCTION lfApplyLns
= SEEK("O"+Order,"ORDLINE")
SELECT ORDLINE
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = "O"+OrdHdr.Order ;
          FOR PikTkt = "******"
   
  SCATTER MEMVAR MEMO
  m.cPickType = IIF(EOF("BOMVAR"),"B","A")

  *-- MAB 10/16/99 Filter on Specific pick type only.
  IF m.cPickType = lcPassType
    INSERT INTO (lcTmpGenPk) FROM MEMVAR
      
    *-- Rule # 4,1 (Style line item ratio)
    *-- Rule # 4,2 (All Sizes was filled)
    *-- Rule # 4,3 (AT least one size has 40%)
    IF lfStyRatio() AND lfAllFill() AND lfOneHas40()

      *-- Rule # 4,4 (Total 40s% / Size count >= 60 %)
      *-- Rule # 4,5 (Consecutive 3 Sizes)
      IF !(lf60SzCnt() AND lfConSecTv())
        SELECT (lcTmpGenPk)
        *C200111,1 Start.
        lnAlocPcs = lnAlocPcs - TotPik
        REPLACE Flag WITH 'R'
        DELETE
        *C200111,1 End.
      ENDIF

    ENDIF
  ENDIF  

ENDSCAN    
*-- end of lfApplyLns.

*!*************************************************************
*! Name      : lfAddLines
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Add order lines.
*!*************************************************************
*!
FUNCTION lfAddLines
= SEEK("O"+Order,"ORDLINE")
SELECT ORDLINE
SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = "O"+OrdHdr.Order ;
          FOR PikTkt = "******"
   
  SCATTER MEMVAR MEMO
  m.cPickType = IIF(EOF("BOMVAR"),"B","A")
  
  *-- MAB 10/16/99 Filter on Specific pick type only.
  IF m.cPickType = lcPassType
    INSERT INTO (lcTmpGenPk) FROM MEMVAR
  ENDIF  

ENDSCAN
*-- end of lfAddLines.

*!*************************************************************
*! Name      : lfNetOpnAl
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Compute Open for allocation Quantity.
*!*************************************************************
*! Return    : Net Open Quantity.
*!*************************************************************
*!
FUNCTION lfNetOpnAl
PARAMETERS lnNetAlo , lnNetQty
PRIVATE lnNetAlo , lnNetQty ,  lnOpenQty , lcFltExp
STORE 0 TO lnNetAlo , lnNetQty , lnOpenQty

lcOrdKey = "O"+ORDHDR.Order
IF SEEK(lcOrdKey, "OrdLine")
  lcFltExp = IIF(lcPassType="B",'','!') + [EOF("BOMVAR")]
  SELECT ORDLINE
  SCAN REST WHILE cOrdType+Order+STR(LineNo,6)=lcOrdKey FOR &lcFltExp
    lnNetQty = lnNetQty + TotQty
    lnNetAlo = lnNetAlo + TotPik
  ENDSCAN
  SELECT ORDHDR
ENDIF  
*-- end of lfNetOpnAl.

*!*************************************************************
*! Name      : lfThrShold
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Compute Order ThreShold OK percentage
*!*************************************************************
*! Parameters: Min. Allow level (Company or Order)
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfThrShold
PARAMETERS lcChkLevel
PRIVATE llRetVal , lnChKQty
lnChKQty = IIF(lcChkLevel="C",lnCmpMnAlo,OrdHdr.nMinAloPer)
llRetVal = lnNetOpen <> 0 AND ((lnAlocPcs/lnNetOpen)*100 >= lnChKQty)
RETURN llRetVal
*-- end of lfThrShold.

*!*************************************************************
*! Name      : lfCmpRatio
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Evaluate Company Line item ratio.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfCmpRatio

RETURN llCmpRatio
*-- end of lfCmpRatio.

*!*************************************************************
*! Name      : lfBackOrd
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Detect if it was back order .
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfBackOrd
PRIVATE llRetVal
llRetVal = ((Status = "O") AND (Ship > 0))
RETURN llRetVal
*-- end of lfBackOrd.

*!*************************************************************
*! Name      : lfOrdRatio
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check order header line item ratio flag.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfOrdRatio

RETURN lItemRatio
*-- end of lfOrdRatio.

*!*************************************************************
*! Name      : lfStyRatio
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check Style line item ratio.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfStyRatio

RETURN Style.lItemRatio
*-- end of lfStyRatio.

*!*************************************************************
*! Name      : lfAllFill
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check if all sizes have allocated quantity.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfAllFill
PRIVATE lcPickType
lcPickType = IIF(EOF("BOMVAR"),"B","A")

=SEEK(Order + Store + lcPickType + STR(LineNo,6) , lcTmpGenPk)
PRIVATE llRetVal , lnI , lcI
FOR lnI = 1 TO Scale.Cnt
  lcI = STR(lnI,1)
  *C200111,1 Start.
  *llRetVal = (Pik&lcI > 0)
  llRetVal = (Qty&lcI > 0)
  *C200111,1 End.
  IF llRetVal
    =lfUpdatTmp()
  ELSE
    EXIT
  ENDIF
ENDFOR
RETURN llRetVal
*-- end of lfAllFill.

*!*************************************************************
*! Name      : lfUpdatTmp
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Update Temporary file new fields.
*!*************************************************************
*!
FUNCTION lfUpdatTmp
PRIVATE llOneHas40,lnTot40,lcConSecTv
llOneHas40 = &lcTmpGenPk..lOneHas40
lnTot40    = &lcTmpGenPk..nTot40
lcConSecTv = &lcTmpGenPk..mConSecTv3

IF Pik&lcI/Qty&lcI >= 0.40
  llOneHas40 = .T.
  lnTot40    = lnTot40 + 1
  lcConSecTv = lcConSecTv + "|"
ELSE
  lcConSecTv = lcConSecTv + " "
ENDIF  
SELECT (lcTmpGenPk)
REPLACE lOneHas40  WITH llOneHas40 ,;
        nTot40     WITH lnTot40    ,;
        mConSecTv3 WITH lcConSecTv
SELECT Ordline
*-- end of lfUpdatTmp.

*!*************************************************************
*! Name      : lfOneHas40
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check if at least one size has 40%.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfOneHas40

RETURN &lcTmpGenPk..lOneHas40
*-- end of lfOneHas40.

*!*************************************************************
*! Name      : lf60SzCnt
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Number of Sizes have 40% greater than or equal 60% 
*!           : relative to all sizes.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lf60SzCnt
PRIVATE llRetVal
llRetVal = (&lcTmpGenPk..nTot40/Scale.Cnt) >= 0.60
RETURN llRetVal
*-- end of lf60SzCnt.

*!*************************************************************
*! Name      : lfConSecTv
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check if have 40% sizes in 3 consecuative order.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfConSecTv
PRIVATE llRetVal
IF Scale.Cnt <= 5
  *llRetVal = ("|||" $ &lcTmpGenPk..mConSecTv3)
  DO CASE
    CASE Scale.Cnt=5 OR Scale.Cnt=4
      llRetVal = ("|||" $ &lcTmpGenPk..mConSecTv3)
    CASE Scale.Cnt=3 OR Scale.Cnt=2
      llRetVal = ("||" $ &lcTmpGenPk..mConSecTv3)
    CASE Scale.Cnt=1
      llRetVal = ("|" $ &lcTmpGenPk..mConSecTv3)
  ENDCASE  
ELSE
  llRetVal = .T.
ENDIF
RETURN llRetVal
*-- end of lfConSecTv.

**********************************************************************
******************** Generate pick tickets Section *******************
**********************************************************************

*!*************************************************************
*! Name      : lfvGenPick
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Generate Pick Ticket Function.
*!*************************************************************
*!
FUNCTION lfvGenPick
IF (lnSelRecs<>0) AND !EOF(lcTmpGenPk) AND (lnBrRecNo<>0)
  
  *-- lcMyPickNo : Current Pick ticket No.  
  PRIVATE lcOldTag,lcPkTkKey,llOpnPkTk,lnBlanks,;
          lnNmDpOnly,lnDsgnOnly,lnDsgnDrop,llNamDrop,llDesign,;
          lcThrmMsg1,lcThrmMsg2
  STORE "" TO lcOldTag   , lcPkTkKey, lcMyPickNo

  *-- Open Pick ticket File if not opened. [Begin]
  IF USED(PIKTKT)
    llOpnPkTk = .F.
    lcOldTag = ORDER("PIKTKT")
    SET ORDER TO OrdPik IN PIKTKT
    lcPkTkKey = PIKTKT.Order + PIKTKT.PikTkt
  ELSE
    llOpnPkTk = gfOpenFile(gcDataDir+"PIKTKT","ORDPIK",'SH')
  ENDIF
  *-- Open Pick ticket File if not opened. [End  ]

  USE (gcWorkDir+lcTmpGenPk) IN 0 AGAIN ALIAS GenPikTkt ORDER TAG lcTmpGenPk
  SELECT GenPikTkt
  SET ORDER TO TAG lcTmpGenPk

  SELECT (lcTmpGenPk)
  GO TOP
  SCAN FOR lSelRec
    
    = SEEK("O"+Order , "OrdHdr")
    STORE 0 TO lnBlanks,lnNmDpOnly,lnDsgnOnly,lnDsgnDrop
    
    SELECT GenPikTkt
    *C200111,1 Start, Added.
    *-First release alocation for not valid item ration lines.
    =lfRelLIRAL()
    GO TOP
    *SCAN FOR Order+Store+cPickType+STR(LineNo,6) = &lcTmpGenPk..Order
    SCAN FOR Order = &lcTmpGenPk..Order
    *C200111,1 End.
      
      *B603111,1 There is one line added in this session. [Begin]
      *lcMyPickNo = lfGenPkNum(Order,Store,cWareCode,cPickType,OrdHdr.cDivision)
      IF SEEK(Order+Store+cPickType,lcTmpPkTk)
        lcMyPickNo = &lcTmpPkTk..PikTkt
      ELSE
        lcMyPickNo = lfGenPkNum(Order,Store,cWareCode,cPickType,OrdHdr.cDivision)
      ENDIF  
      *B603111,1 There is one line added in this session. [End  ]

      =lfGenPikTk(lcMyPickNo)

      *-- Caling Thermometer. [Begin]
      lcThrmMsg1 = "Generate Picking Ticket # "
      lcThrmMsg2 = lcMyPickNo + " Order " + Order + ", Store " + Store +;
                   IIF(cPickType="A",", Adorned",", Blank")

      WAIT WINDOW lcThrmMsg1 + lcThrmMsg2 NOWAIT
      *-- Caling Thermometer. [End  ]
      
      IF lfAlowBack()
        SELECT GenPikTkt
        lnBlanks = lnBlanks + (TotQty - TotPik)
      ENDIF  
      
    ENDSCAN

    *-- Last Step (if Allow Back Order flag)
    *-- and Non of numeric values achive the minimum value then 
    *-- Reset Allow Back Order flag.
    IF lfAlowBack() AND (lnBlanks   < lnBlankBk ) AND ;
                        (lnDsgnOnly < lnDesignBk) AND ;
                        (lnNmDpOnly < lnNmDropBk) AND ;
                        (lnDsgnDrop < lnDesDrpBk)
      SELECT ORDHDR
      REPLACE lAlwBkOrd WITH .F.
    ENDIF

    *-- Delete First Order Line.
    SELECT (lcTmpGenPk)
    lnSelRecs = lnSelRecs - 1
    lnAllRecs = lnAllRecs - 1
    DELETE
    
  ENDSCAN

  WAIT CLEAR
  USE IN GenPikTkt

  IF !llOpnPkTk
    SELECT PIKTKT
    =SEEK(lcPkTkKey)
    SET ORDER TO &lcOldTag
  ENDIF
  
  *-- if no records in temporary file.
  SELECT (lcTmpGenPk)
  IF lnAllRecs = 0
    ZAP
  ENDIF

  *B603111,1 Clear Piktkt temp. file for new session [Begin]
  IF USED(lcTmpPkTk)
    ZAP
  ENDIF
  *B603111,1 Clear Piktkt temp. file for new session [End  ]

  *-- Refresh Screen.
  =lfDispBrow()  && Control Browse and screen controls.
  =lfRefScr()
  _CUROBJ = OBJNUM(ibBrow)
  
ENDIF
*-- end of lfvGenPick.

*!*************************************************************
*! Name      : lfGenPkNum
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Generate new pick ticket number or add to old one.
*!*************************************************************
*! Return    : Pick Ticket Number.
*!*************************************************************
*!
FUNCTION lfGenPkNum
*-- lcMyOrder  : Order     #.
*-- lcMyStore  : Store     #.
*-- lcMyLoc    : Location  #.
*-- lcMyPikTyp : Pick Type #.
*-- lcMyDiv    : Division  #.
PARAMETERS lcMyOrder , lcMyStore , lcMyLoc , lcMyPikTyp , lcMyDiv

PRIVATE lnCurAlias,lcPikTkt,laPikNo,llNewPick,lcWhereExpr
        
lnCurAlias = SELECT(0)

*-- Define array hold the piktkts no. , status , printed or not.
DECLARE laPikNo[1]

*-- Initialize the needed variables.
STORE ""  TO laPikNo , lcPikTkt , lcOldTag , lcPkTkKey , lcLocExpr
llNewPick = .F. && Create new pick ticket

*-- Select pick file.
SELECT PIKTKT
*-- if you find pick tickets for this order.
IF SEEK(lcMyOrder) 
  SET ORDER TO  && Activiate Rush-More As I can.

  lcWhereExpr = [Order + PikTkt = lcMyOrder AND !(Status $ "CX") AND (Store = lcMyStore) AND (cWareCode = lcMyLoc) AND (cPickType = lcMyPikTyp)]
  SELECT DISTINCT PikTkt FROM PikTkt   ;
    WHERE &lcWhereExpr ORDER BY PikTkt ;
    INTO ARRAY laPikNo

  SET ORDER TO OrdPik  && Restore Tag setting. 

  IF _TALLY = 0
    llNewPick = .T.
  ELSE

    IF ALEN(laPikNo,1) = 1
    
      *** There is Picking ticket no. : {laPikNo[1]}.     ***
      *** Do you wish to assign a new picking ticket no.? ***
      *** < Add > - < New > ***
      IF gfModalGen("TRM44011B44000","DIALOG" , laPikNo[1]) = 1
        =SEEK(lcMyOrder + laPikNo[1])
      ELSE  
        llNewPick = .T.
      ENDIF
    
    ELSE  && More than one Pick Ticket.
      *-- Loop Picktkt array to add required values.
      FOR lnI = 1 TO ALEN(laPikNo,1)
        =SEEK(lcMyOrder + laPikNo[lnI])
        lcAddToArr = SPACE(1) + IIF(PrtFlag="P","Yes","No ") +;
                     SPACE(5) + IIF(status $ "O ","Open   ",;
                                IIF(Status="H","On Hold",SPACE(7))) +;
                     SPACE(1) + DTOC(Date)
        laPikNo[lnI] = laPikNo[lnI] + lcAddToArr
      ENDFOR
      lsPikLst = 1
      DO ALPIKLST.SPR
   
    ENDIF

  ENDIF

ELSE
  llNewPick = .T.
ENDIF

*-- If create new piktkt.
IF llNewPick
  lcPikTkT = gfSequence('PIKTKT', '', '', lcMyDiv)

ELSE  && Add to Old PikTkt
  lcPikTkT = PikTkt
ENDIF
SELECT (lnCurAlias)
RETURN lcPikTkT
*-- end of lfGenPkNum.

*!*************************************************************
*! Name      : lfvGetPik
*! Developer : (MAB) Mohamed Atia Badran
*! Date      : 07/21/1999
*! Purpose   : Valid function for both buttons : Select - New (ALPIKLST) Screen.
*!*************************************************************
*! Passed parameters : lnButtonNo
*!*************************************************************
*
FUNCTION lfvGetPik
PARAMETERS lnButtonNo

*-- If lnButtonNo = 1, Select an existing picking ticket from the list.
*-- If lnButtonNo = 2, Get new Picking ticket.

DO CASE
  CASE lnButtonNo = 1
    =SEEK(lcMyOrder + SUBSTR(laPikNo[lsPikLst],1,6))

  CASE lnButtonNo = 2
    *-- Get new picking ticket.
    llNewPick = .T.
ENDCASE
*-- end of lfvGetPik.

*!*************************************************************
*! Name      : lfGenPikTk
*! Developer : (MAB) Mohamed Atia Badran
*! Date      : 07/21/1999
*! Purpose   : Function to update the needed files to Generate a Pick
*!             ticket for the current record of the temp. Order lines file
*!*************************************************************
*! Passed Parameters : Pick ticket number
*!*************************************************************
*
FUNCTION lfGenPikTk
PARAMETERS lcPickParm
IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
  SELECT ORDLINE
  = RLOCK()
  REPLACE PikTkt  WITH lcPickParm,;
          Picked  WITH .T.,;
          PikDate WITH gdSysDate
  =gfAdd_Info()
  UNLOCK
ENDIF    && End of IF

SELECT ORDHDR
= RLOCK()
REPLACE nNetAloQty WITH MAX(nNetAloQty - GenPikTkt.TotPik,0)
=gfAdd_Info()
UNLOCK
*--

SELECT PIKTKT
*IF There is a no record for this Pick ticket number in the PIKTKT file

*B603111,1 One Record per the same pick ticket. [Begin]
*IF !SEEK(lcPickParm)
IF !SEEK(GenPikTkt.Order + lcPickParm)
*B603111,1 One Record per the same pick ticket. [End  ]
  APPEND BLANK
  = RLOCK()
  REPLACE Account   WITH GenPikTkt.Account   ,;
          Store     WITH GenPikTkt.Store     ,;
          Order     WITH GenPikTkt.Order     ,;
          cPickType WITH GenPikTkt.cPickType ,;
          PikTkt    WITH lcPickParm          ,;
          Date      WITH gdSysDate           ,;
          cWareCode WITH OrdHdr.cWareCode    ,;
          CustPo    WITH IIF(ORDHDR.MultiPo , GenPikTkt.CustPo , ORDHDR.CustPo) ,;
          Status    WITH 'O'

  =gfAdd_Info()
  UNLOCK
  
  *-- Audit Trial.
  IF ASCAN(laEvntTrig,PADR("GEN_PIK",10)) <> 0
    =gfDoTriger('ALGENPK',PADR("GEN_PIK",10))
  ENDIF
    
ENDIF    && End of IF

*B603111,1 Add new Piktkt to temp. session file. [Begin]
IF !SEEK(Order+Store+cPickType,lcTmpPkTk)
  m.Order = Order
  m.Store = Store
  m.cPickType = cPickType
  m.PikTkt = PikTkt
  INSERT INTO (lcTmpPkTk) FROM MEMVAR
ENDIF
*B603111,1 Add new Piktkt to temp. session file. [End  ]

SELECT GenPikTkt
*-- if it is not First Order Line.
IF Order+Store+cPickType+STR(LineNo,6) <>   ;
    &lcTmpGenPk..Order + &lcTmpGenPk..Store +;
    &lcTmpGenPk..cPickType + STR(&lcTmpGenPk..LineNo,6)
  DELETE
ENDIF    
*-- end of lfGenPikTk.

*!*************************************************************
*! Name      : lfAlowBack
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Check allow back order flag.
*!*************************************************************
*! Return    : .T. or .F.
*!*************************************************************
*!
FUNCTION lfAlowBack

RETURN OrdHdr.lAlwBkOrd
*-- end of lfAlowBack.

*!*************************************************************
*! Name      : lpSetBack
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Set Back order Flag.
*!*************************************************************
*!
PROCEDURE lpSetBack
*-- end of lpSetBack.



**********************************************************************
************************ Close screen Section ************************
**********************************************************************

*!*************************************************************
*! Name      : lfvClose
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/20/99
*! Purpose   : Clear Read Cycle
*!*************************************************************
*!
FUNCTION lfvClose
ACTI WINDOW (lcGenPkCh2) TOP
_CUROBJ = OBJNUM(pbClose)
KEYBOARD CHR(13) CLEAR PLAIN
*-- end of lfvClose.

*!*************************************************************
*! Name      : lfRelLIRAL()
*! Developer : TAK
*! Date      : 02/05/00
*! Purpose   : Release allocation from order line if line item
*!             ration check is faild.
*!*************************************************************
*C200111,1.
FUNCTION lfRelLIRAL

SELECT GenPikTkt
lcDSEt = SET('DELETE')
SET DELETE OFF
GO TOP
SCAN FOR DELETED() AND Order = ORDHDR.Order AND Flag='R' AND Piktkt='*'
  IF SEEK('O' + Order + STR(LineNo , 6) , 'ORDLINE')
    SELECT STYLE
    IF SEEK(ORDLINE.Style) 
      = RLOCK()
      REPLACE Alo1   WITH Alo1 - ORDLINE.Pik1,;
              Alo2   WITH Alo2 - ORDLINE.Pik2,;
              Alo3   WITH Alo3 - ORDLINE.Pik3,;          
              Alo4   WITH Alo4 - ORDLINE.Pik4,;          
              Alo5   WITH Alo5 - ORDLINE.Pik5,;          
              Alo6   WITH Alo6 - ORDLINE.Pik6,;
              Alo7   WITH Alo7 - ORDLINE.Pik7,;          
              Alo8   WITH Alo8 - ORDLINE.Pik8,;
              TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
      UNLOCK
    ENDIF
    SELECT STYDYE
    IF SEEK(ORDLINE.Style+ORDLINE.cWareCode)
      = RLOCK()
      REPLACE Alo1   WITH Alo1 - ORDLINE.Pik1,;
              Alo2   WITH Alo2 - ORDLINE.Pik2,;
              Alo3   WITH Alo3 - ORDLINE.Pik3,;          
              Alo4   WITH Alo4 - ORDLINE.Pik4,;          
              Alo5   WITH Alo5 - ORDLINE.Pik5,;          
              Alo6   WITH Alo6 - ORDLINE.Pik6,;
              Alo7   WITH Alo7 - ORDLINE.Pik7,;          
              Alo8   WITH Alo8 - ORDLINE.Pik8,;
              TotAlo WITH Alo1+Alo2+Alo3+Alo4+Alo5+Alo6+Alo7+Alo8
      UNLOCK
    ENDIF
    SELECT ORDLINE
    = RLOCK()
    REPLACE cWareCode WITH ORDHDR.cWareCode,;
            PikTkt  WITH SPACE(6),;
            Picked  WITH .F.,;
            PikDate WITH {},;
            Pik1    WITH 0,;
            Pik2    WITH 0,;
            Pik3    WITH 0,;
            Pik4    WITH 0,;
            Pik5    WITH 0,;
            Pik6    WITH 0,;
            Pik7    WITH 0,;
            Pik8    WITH 0,;                                                                                    
            TotPik  WITH 0
    UNLOCK
  ENDIF
  SELECT ORDHDR
  = RLOCK()
  REPLACE nNetAloQty WITH MAX(nNetAloQty - GenPikTkt.TotPik,0)
  UNLOCK
  *--
ENDSCAN
SELECT GenPikTkt
SET DELETED &lcDSEt
RETURN

