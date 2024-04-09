*:----------------------------------------------------------------
*: Name       : MFGenLns
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Generate Lines for Cutting Ticket, Purchase Order,
*:              Based on Components Availability.
*:----------------------------------------------------------------
*: Parameters : llViewMode : .T. -> Just to view.
*:                           .F. -> To be able to generate lines.
*:              lcTranType : "M" -> Cutting Ticket.
*:                           "T" ->
*:                           "I" ->
*:              lcMstTmpFl : Temp file name.
*:              lcPItmName : "Item" field name in the temp file.
*:              lcPClrName : "Color" field name in the temp file.
*:              lcPDyeName : "Dyelot" field name in the temp file.
*:              lcPWarName : "Warehouse" field name in the temp file.
*:              lcPQtyName : "Quantities" fields initial name in
*:                           the temp file.
*:              lcPTotName : "Total" field name in the temp file.
*:              lcGnTmpFl  : The temp file name that will contain
*:                           the generated lines.
*:----------------------------------------------------------------
*: Notes ....
*: 1. IF you will call this program as a procedure not a function
*:    define the variable "llGnRetVal" in your program before the
*:    call and check it when the call is done to know if the user
*:    selected to "Apply" the modifications or not.
*:----------------------------------------------------------------
*: Returns    : .T. -> The user selected to apply the generated
*:                     lines.
*:              .F. -> The user selected not to apply the
*:                     generated lines.
*:----------------------------------------------------------------
*: Example    : DO (gcAppHome+"MFGenLns") with  ;
*:                 llView,"M",lcTmpCutLn,"Style","",;
*:                 "Dyelot","cWareCode","Qty","TotQty",lcRetFile
*:----------------------------------------------------------------
*: Modifications:
*: B602650,1 YMA 03/08/99 Let the remaining qty show +ve and -ve qty.
*: E301410,1 RAMY 06/06/2000 Add some new options to the screen
*:----------------------------------------------------------------

*-E301410,1 RAMY [start]  Add new parameter
*PARAMETERS llViewMode, lcTranType, lcMstTmpFl, lcPItmName,;
*           lcPClrName, lcPDyeName, lcPWarName, lcPQtyName,;
*           lcPTotName, lcGnTmpFl


PARAMETERS llViewMode, lcTranType, lcMstTmpFl, lcPItmName,;
  lcPClrName, lcPDyeName, lcPWarName, lcPQtyName,;
  lcPTotName, lcGnTmpFl , lcCutPick
*-E301410,1 RAMY [end]

IF PARAMETERS() < 10
  RETURN
ENDIF

lnAlias    = SELECT(0)
llGnRetVal = .F.
llEmptyLns = .F.


*-- Temp files names.
lcMyMstFl  = gfTempName()
lcGnBomFl  = gfTempName()
lcGnDetFl  = gfTempName()
lcGnOprFl  = gfTempName()

lcGnBrTl   = "Components Details"

*-- Variables to save\restore the program environment.
llOpnSty   = .F.
llOpnScl   = .F.
llOpnFab   = .F.
llOpnBom   = .F.
llOpnFbD   = .F.
llOpnStD   = .F.
lcStyTag   = SPACE(0)
lcSclTag   = SPACE(0)
lcFabTag   = SPACE(0)
lcBomTag   = SPACE(0)
lcFbDTag   = SPACE(0)
lcStDTag   = SPACE(0)
lnStyPos   = 0
lnSclPos   = 0
lnFabPos   = 0
lnBomPos   = 0
lnFbDPos   = 0
lnStDPos   = 0

*-- To hold the firled names (Passed as parameters).
lcItmName  = SPACE(0)
lcClrName  = SPACE(0)
lcDyeName  = SPACE(0)
lcWarName  = SPACE(0)
lcQtyName  = SPACE(0)
lcTotName  = SPACE(0)

STORE SPACE(0) TO lcQt1Name, lcQt2Name, lcQt3Name, lcQt4Name ,;
  lcQt5Name, lcQt6Name, lcQt7Name, lcQt8Name

*-- Get the system setups values.
DIMENSION laSetups[7,2]
laSetups[1,1]  = 'M_WAREHOUSE'
laSetups[2,1]  = 'M_C'+lcTranType+'SLBL1'
laSetups[3,1]  = 'M_C'+lcTranType+'SLBL2'
laSetups[4,1]  = 'M_C'+lcTranType+'SLBL3'
laSetups[5,1]  = 'M_C'+lcTranType+'SLBL4'
laSetups[6,1]  = IIF(lcTranType='T','','M_C'+lcTranType+'SLBL5')
laSetups[7,1]  = 'M_DYELOT'
llNoThing      = gfGetMemVar(@laSetups,gcAct_Comp)

lcMjrHdr   = gfItemMask("HM")
lcMjrMsk   = gfItemMask("PM")
lcAllHdr   = gfItemMask("HI")


*-E301410,1 RAMY [start] 
lcCPBrTtl  = "Quantities Allocated from Order Lines"
llFrstTime = .T.
*-E301410,1 RAMY [end]

IF lfSetEnv() AND lfChkSheet() AND lfBldGnTmp()
  PUSH KEY CLEAR
  ON KEY LABEL TAB        DO lfGnTab
  ON KEY LABEL ESC        DO lfGnEsc
  ON KEY LABEL BACKTAB    DO lfGnSTb
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+W     lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL ALT+B ACTIVATE WINDOW (lcGnBrTl)

  DO (gcScrDir+"MFGenLns.SPR")

  RELEASE WINDOW (lcGnBrTl)
  POP KEY
ENDIF

llNoThing = lfReSetEnv() AND lfClsFls()
SELECT(lnAlias)

RETURN (llGnRetVal)

*:----------------------------------------------------------------
*: Name       : lfGnBr
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Components Details Browse function.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfGnBr()
*:----------------------------------------------------------------

FUNCTION lfGnBr
PRIVATE lnAlias

lnAlias   = SELECT(0)
*-E301410,1 RAMY [start] Change the browse to browse the field "Use This" in tall cases
*                        Add new calculated field to sum the extra qty
*lcBrFilds = ;
"cItem=IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item):H='Item':R,"+;
"cDesc     :H='Description':R:20,"+IIF(llViewMode,"",;
"nDUseThis :H='Use This'   :V=lfBrValid(),")+;
"nDReq_Qty :H='Required'   :R   ,"+;
"nOnHand   :H='On Hand'    :R   ,"+;
"nOnOrder  :H='On Order'   :R   ,"+;
"nRemain   :H='Remaining'  :R"

lcBrFilds = ;
  "cSelect   :H=' '           :R ,"+;
  "cItem=IIF(cCatgTyp$'FT',SUBSTR(Item,1,7)+'-'+IClr,Item):H='Item':R,"+;
  "cDesc     :H='Description':R:20  ,"+ ;
  "nDUseThis :H='Use This'   :W=lfwWhen() :V=lfBrValid() , "+;
  "nDReq_Qty :H='Required'   :R   :10 :P='9999.999',"+;
  "lnExtra = IIF( cShowType = '0' , '' , (nDUseThis - nDReq_Qty)) :H='Extra' :8 :P='9999.999', "+;
  "nOnHand   :H='On Hand'    :R   ,"+;
  "nOnOrder  :H='On Order'   :R   ,"+;
  "nRemain   :H='Remaining'  :R"
*-E301410,1 RAMY [end]
SELECT (lcGnBomFl)
BROWSE FIELDS &lcBrFilds WINDOW GenLines2 IN WINDOW GenLines1 ;
  TITLE (lcGnBrTl) LOCK 0 SAVE NOWAIT NOCLEAR NOAPPEND ;
  NODELETE NOMENU WHEN cShowType = "1"

SELECT(lnAlias)

*:----------------------------------------------------------------
*: Name       : lfBrWhen
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Components Details "When" Function.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfBrWhen()
*:----------------------------------------------------------------
FUNCTION lfBrValid


IF LASTKEY() = 27
  RETURN .F.
ELSE
  IF &lcGnBomFl..nDUseThis < 0
    *-- Component quantity cannot be less than 0.
    RETURN gfModalGen('TRM38122B00000') = 2
  ELSE
    lnAlias = SELECT(0)
    SELECT (lcGnBomFl)
    *-- Update the field that will be used to compute the lines.
    *B602650,1 YMA 03/08/99 Let the remaining qty show +ve and -ve qty.
    REPLACE nUseThis WITH nDUseThis ,;
      nRemain  WITH nOnHand+nOnOrder-nUseThis
    *REPLACE nUseThis WITH nDUseThis ,;
    *        nRemain  WITH MIN(nOnHand+nOnOrder-nUseThis,0)
    *B602650,1 YMA 03/08/99
    SHOW WINDOW (lcGnBrTl) REFRESH
    SELECT (lnAlias)
  ENDIF
ENDIF

*:----------------------------------------------------------------
*: Name       : lfGnTab
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Components Details Tab Trapping.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfGnTab()
*:----------------------------------------------------------------
FUNCTION lfGnTab

ON KEY LABEL TAB
DO CASE
CASE WONTOP() = (lcGnBrTl)
  ACTIVATE WINDOW GenLines3
  *_CUROBJ = IIF(llViewMode, OBJNUM(PBGNCLOSE), OBJNUM(pbGnView))
  _CUROBJ = OBJNUM(PBSELECt)

CASE WONTOP() # (lcGnBrTl) AND SYS(18) = "PBGNCLOSE"
  KEYBOARD "{ALT+B}"

OTHERWISE
  _CUROBJ = _CUROBJ + 1
ENDCASE
ON KEY LABEL TAB DO lfGnTab

*:----------------------------------------------------------------
*: Name       : lfGnStb
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Components Details Shift Tab Trapping.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfGnStb()
*:----------------------------------------------------------------
FUNCTION lfGnStb

ON KEY LABEL BACKTAB
DO CASE
CASE WONTOP() = (lcGnBrTl)
  ACTIVATE WINDOW GenLines3
  _CUROBJ = OBJNUM(PBSELECt)

CASE llViewMode AND WONTOP() # (lcGnBrTl) AND SYS(18) = "PBGNCLOSE"
  KEYBOARD "{ALT+B}"

CASE !llViewMode AND WONTOP() # (lcGnBrTl) AND SYS(18) = "PBSELECt"
  KEYBOARD "{ALT+B}"

OTHERWISE
  _CUROBJ = _CUROBJ - 1
ENDCASE
ON KEY LABEL BACKTAB DO lfGnSTb

*:----------------------------------------------------------------
*: Name       : lfGnEsc
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Components Details Escape Trapping.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfGnEsc()
*:----------------------------------------------------------------
FUNCTION lfGnEsc

ACTIVATE WINDOW GenLines3
_CUROBJ = OBJNUM(pbGnClose)
KEYBOARD "{ENTER}"

*:----------------------------------------------------------------
*: Name       : lfSetVar
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Set the variables that hold the fields names that
*:              will be used to deal with the master temp file.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfSetVar()
*:----------------------------------------------------------------
FUNCTION lfSetVar
PARAMETERS lcAlias

lcItmName = lcAlias + "." + ALLTRIM(lcPItmName)
lcWarName = lcAlias + "." + ALLTRIM(lcPWarName)
lcTotName = lcAlias + "." + ALLTRIM(lcPTotName)
lcQt1Name = lcAlias + "." + ALLTRIM(lcPQtyName) + "1"
lcQt2Name = lcAlias + "." + ALLTRIM(lcPQtyName) + "2"
lcQt3Name = lcAlias + "." + ALLTRIM(lcPQtyName) + "3"
lcQt4Name = lcAlias + "." + ALLTRIM(lcPQtyName) + "4"
lcQt5Name = lcAlias + "." + ALLTRIM(lcPQtyName) + "5"
lcQt6Name = lcAlias + "." + ALLTRIM(lcPQtyName) + "6"
lcQt7Name = lcAlias + "." + ALLTRIM(lcPQtyName) + "7"
lcQt8Name = lcAlias + "." + ALLTRIM(lcPQtyName) + "8"

IF EMPTY (lcPClrName)
  lcClrName = SPACE(6)
ELSE
  lcClrName = lcAlias + "." + ALLTRIM(lcPClrName)
ENDIF

IF EMPTY (lcPDyeName)
  lcDyeName = SPACE(10)
ELSE
  lcDyeName = lcAlias + "." + ALLTRIM(lcPDyeName)
ENDIF

*:----------------------------------------------------------------
*: Name       : lfSetEnv
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Set the program environment.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfSetEnv()
*:----------------------------------------------------------------
FUNCTION lfSetEnv

= lfSetVar(lcMstTmpFl)
IF USED("Style")
  SELECT STYLE
  llOpnSty = .F.
  lcStyTag = TAG()
  lnStyPos = RECNO()
ELSE
  llOpnSty = gfOpenFile(gcDataDir+"Style","","SH")
  lcStyTag = SPACE(0)
  lnStyPos = 0
ENDIF
SELECT STYLE
SET ORDER TO TAG STYLE

IF USED("Scale")
  SELECT Scale
  llOpnScl = .F.
  lcSclTag = TAG()
  lnSclPos = RECNO()
ELSE
  llOpnScl = gfOpenFile(gcDataDir+"Scale","","SH")
  lcSclTag = SPACE(0)
  lnSclPos = 0
ENDIF
SELECT Scale
SET ORDER TO TAG Scale

IF USED("Fabric")
  SELECT Fabric
  llOpnFab = .F.
  lcFabTag = TAG()
  lnFabPos = RECNO()
ELSE
  llOpnFab = gfOpenFile(gcDataDir+"Fabric","","SH")
  lcFabTag = SPACE(0)
  lnFabPos = 0
ENDIF
SELECT Fabric
SET ORDER TO TAG Fabric

IF USED("Bom")
  SELECT Bom
  llOpnBom = .F.
  lcBomTag = TAG()
  lnBomPos = RECNO()
ELSE
  llOpnBom = gfOpenFile(gcDataDir+"Bom","","SH")
  lcBomTag = SPACE(0)
  lnBomPos = 0
ENDIF
SELECT Bom
SET ORDER TO TAG Bom

IF USED("FabDye")
  SELECT FabDye
  llOpnFbD = .F.
  lcFbDTag = TAG()
  lnFbDPos = RECNO()
ELSE
  llOpnFbD = gfOpenFile(gcDataDir+"FabDye","","SH")
  lcFbDTag = SPACE(0)
  lnFbDPos = 0
ENDIF
SELECT FabDye
SET ORDER TO TAG FabDye

IF USED("StyDye")
  SELECT StyDye
  llOpnStD = .F.
  lcStDTag = TAG()
  lnStDPos = RECNO()
ELSE
  llOpnStD = gfOpenFile(gcDataDir+"StyDye","","SH")
  lcStDTag = SPACE(0)
  lnStDPos = 0
ENDIF
SELECT StyDye
SET ORDER TO TAG StyDye

*:----------------------------------------------------------------
*: Name       : lfReSetEnv
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : ReSet the program environment.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfReSetEnv()
*:----------------------------------------------------------------
FUNCTION lfReSetEnv

IF USED("Style")
  SELECT STYLE
  IF llOpnSty
    USE IN STYLE
  ELSE
    SET ORDER TO TAG &lcStyTag
    IF lnStyPos>0 AND lnStyPos<=RECCOUNT()
      GOTO lnStyPos
    ENDIF
  ENDIF
ENDIF

IF USED("Scale")
  SELECT Scale
  IF llOpnScl
    USE IN Scale
  ELSE
    SET ORDER TO TAG &lcSclTag
    IF lnSclPos>0 AND lnSclPos<=RECCOUNT()
      GOTO lnSclPos
    ENDIF
  ENDIF
ENDIF

IF USED("Fabric")
  SELECT Fabric
  IF llOpnFab
    USE IN Fabric
  ELSE
    SET ORDER TO TAG &lcFabTag
    IF lnFabPos>0 AND lnFabPos<=RECCOUNT()
      GOTO lnFabPos
    ENDIF
  ENDIF
ENDIF

IF USED("Bom")
  SELECT Bom
  IF llOpnBom
    USE IN Bom
  ELSE
    SET ORDER TO TAG &lcBomTag
    IF lnBomPos>0 AND lnBomPos<=RECCOUNT()
      GOTO lnBomPos
    ENDIF
  ENDIF
ENDIF

IF USED("FabDye")
  SELECT FabDye
  IF llOpnFbD
    USE IN FabDye
  ELSE
    SET ORDER TO TAG &lcFbDTag
    IF lnFbDPos>0 AND lnFbDPos<=RECCOUNT()
      GOTO lnFbDPos
    ENDIF
  ENDIF
ENDIF

IF USED("StyDye")
  SELECT StyDye
  IF llOpnStD
    USE IN StyDye
  ELSE
    SET ORDER TO TAG &lcStDTag
    IF lnStDPos>0 AND lnStDPos<=RECCOUNT()
      GOTO lnStDPos
    ENDIF
  ENDIF
ENDIF

*:----------------------------------------------------------------
*: Name       : lfBldGnTmp
*: Developer  : Yasser Mohammed Aly - (YMA)
*: Date       : 12/18/97
*: Purpose    : Build the program's temp file.
*:----------------------------------------------------------------
*: Calls      : None.
*:----------------------------------------------------------------
*: Parameters : None.
*:----------------------------------------------------------------
*: Returns    : None
*:----------------------------------------------------------------
*: Example    : = lfBldGnTmp()
*:----------------------------------------------------------------
FUNCTION lfBldGnTmp
PRIVATE lnAlias

lnAlias = SELECT(0)

*-- Count the records just to be able to display the thermometer.
SELECT (lcMstTmpFl)
COUNT ALL FOR !DELETED() AND !EMPTY(STYLE) TO lnTotal

*-- Check if there is at least one record with total quantity
*-- grater than zero.
LOCATE FOR &lcTotName > 0
llEmptyLns = !FOUND()

*-- Build a new temp file with the same number of records
*-- as the master temp file.
*-- If the master temp file does not contain any records with
*-- quantity then initialize the new temp file record with 1s just to
*-- be able to compute the components relatively...
SELECT (lcMstTmpFl)
lnFields  = AFIELDS(laFile)
CREATE TABLE (gcWorkDir+lcMyMstFl) FROM ARRAY laFile
SELECT (lcMstTmpFl)
SCAN
  SCATTER MEMVAR MEMO
  SELECT (lcMyMstFl)
  APPEND BLANK
  IF llEmptyLns
    lnTot = lfGtSclInf(&lcItmName)
    FOR lnI = 1 TO lnTot
      lcVar  = "m." + lcPQtyName + STR(lnI,1)
      lcTot  = "m." + lcPTotName
      &lcVar = 1
      &lcTot = &lcTot + 1
    ENDFOR
  ENDIF
  GATHER MEMVAR MEMO
ENDSCAN
SELECT (lcMyMstFl)
INDEX ON &lcPItmName TAG "YMA"
SET ORDER TO YMA
= lfSetVar(lcMyMstFl)

lnTotal = lnTotal + 3
lnCount = 0
lcTitle = "Building work area.."

lnCount = lnCount + 1
= gfThermo (lnTotal, lnCount, lcTitle, "")

USE (gcDataDir+"CTKTBOM") IN 0 AGAIN ALIAS MstBom
SELECT MstBom
lnFields = AFIELDS(laFile)
USE IN MstBom

*-E301410,1 RAMY Add new Field to the browse file

*DIMENSION laFile[lnFields+8,4]
*laFile[lnFields+1,1] = 'cShowType'
*laFile[lnFields+1,2] = 'C'
*laFile[lnFields+1,3] = 1
*laFile[lnFields+1,4] = 0
*laFile[lnFields+2,1] = 'cDesc'
*laFile[lnFields+2,2] = 'C'
*laFile[lnFields+2,3] = 20
*laFile[lnFields+2,4] = 0
*laFile[lnFields+3,1] = 'nOnHand'
*laFile[lnFields+3,2] = 'N'
*laFile[lnFields+3,3] = 8
*laFile[lnFields+3,4] = 0
*laFile[lnFields+4,1] = 'nOnOrder'
*laFile[lnFields+4,2] = 'N'
*laFile[lnFields+4,3] = 8
*laFile[lnFields+4,4] = 0
*laFile[lnFields+5,1] = 'nRemain'
*laFile[lnFields+5,2] = 'N'
*laFile[lnFields+5,3] = 8
*laFile[lnFields+5,4] = 0
*laFile[lnFields+6,1] = 'nUseThis'
*laFile[lnFields+6,2] = 'N'
*laFile[lnFields+6,3] = 12
*laFile[lnFields+6,4] = 3
*laFile[lnFields+7,1] = 'nDUseThis'
*laFile[lnFields+7,2] = 'N'
*laFile[lnFields+7,3] = 12
*laFile[lnFields+7,4] = 3
*laFile[lnFields+8,1] = 'nDReq_Qty'
*laFile[lnFields+8,2] = 'N'
*laFile[lnFields+8,3] = 12
*laFile[lnFields+8,4] = 3


DIMENSION laFile[lnFields+9,4]
laFile[lnFields+1,1] = 'cSelect'
laFile[lnFields+1,2] = 'C'
laFile[lnFields+1,3] = 1
laFile[lnFields+1,4] = 0
laFile[lnFields+2,1] = 'cShowType'
laFile[lnFields+2,2] = 'C'
laFile[lnFields+2,3] = 1
laFile[lnFields+2,4] = 0
laFile[lnFields+3,1] = 'cDesc'
laFile[lnFields+3,2] = 'C'
laFile[lnFields+3,3] = 20
laFile[lnFields+3,4] = 0
laFile[lnFields+4,1] = 'nOnHand'
laFile[lnFields+4,2] = 'N'
laFile[lnFields+4,3] = 8
laFile[lnFields+4,4] = 0
laFile[lnFields+5,1] = 'nOnOrder'
laFile[lnFields+5,2] = 'N'
laFile[lnFields+5,3] = 8
laFile[lnFields+5,4] = 0
laFile[lnFields+6,1] = 'nRemain'
laFile[lnFields+6,2] = 'N'
laFile[lnFields+6,3] = 8
laFile[lnFields+6,4] = 0
laFile[lnFields+7,1] = 'nUseThis'
laFile[lnFields+7,2] = 'N'
laFile[lnFields+7,3] = 12
laFile[lnFields+7,4] = 3
laFile[lnFields+8,1] = 'nDUseThis'
laFile[lnFields+8,2] = 'N'
laFile[lnFields+8,3] = 12
laFile[lnFields+8,4] = 3
laFile[lnFields+9,1] = 'nDReq_Qty'
laFile[lnFields+9,2] = 'N'
laFile[lnFields+9,3] = 12
laFile[lnFields+9,4] = 3

*-E301410,1 RAMY [end]

CREATE TABLE (gcWorkDir+lcGnBomFl) FROM ARRAY laFile
SELECT (lcGnBomFl)
INDEX ON cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot TAG 'CTKTBOM'
INDEX ON Typ+cShowType+Item+IClr+MFGCode+Dyelot TAG (lcGnBomFl) ADDITIVE
RELEASE laFile

lnCount = lnCount + 1
= gfThermo (lnTotal, lnCount, lcTitle, "")
USE (gcDataDir+"BOMLINE") IN 0 AGAIN ALIAS MstBomLine
SELECT MstBomLine
COPY STRUCTURE TO (gcWorkDir+lcGnDetFl) WITH CDX
USE (gcWorkDir+lcGnDetFl) IN 0 EXCL
SELECT (lcGnDetFl)
INDEX ON STYLE+Item+IClr TAG (lcGnDetFl)
USE IN MstBomLine

lnCount = lnCount + 1
= gfThermo (lnTotal, lnCount, lcTitle, "")
USE (gcDataDir+"MFGOprHd") IN 0 AGAIN ALIAS MstOprHd
SELECT MstOprHd
COPY STRUCTURE TO (gcWorkDir+lcGnOprFl) WITH CDX
USE IN MstOprHd
USE (gcWorkDir+lcGnOprFl) IN 0

SELECT (lcMyMstFl)
GOTO TOP
llGoOn = .T.
SCAN WHILE llGoOn
  DIMENSION  laQty[9]
  STORE 0 TO laQty
  SCATTER FIELDS LIKE (lcPQtyName + "*") TO laQty
  lnCount    = lnCount + 1
  *lnTotQty  = EVAL(lcTotName)
  laQty[9]   = EVAL(lcTotName)
  lcClrToSnd = IIF(EMPTY(lcClrName),SPACE(6),EVAL(lcClrName))
  lcDyeToSnd = IIF(EMPTY(lcDyeName),SPACE(6),EVAL(lcDyeName))
  * llGoOn     = gfSheetItem (lcTranType,SPACE(6),"",&lcItmName ,;
  lcClrToSnd,0,lcDyeToSnd,SPACE(6),;
  @laQty,lnTotQty,"Bom",lcGnBomFl,lcGnDetFl,;
  lcGnOprFl,"",0,'',0,'',0, 0, 0, 0, 0, 0)
llGoOn     = gfSheetItem (lcTranType,SPACE(6),"",&lcItmName ,;
  lcClrToSnd,0,lcDyeToSnd,SPACE(6),SPACE(6),;
  @laQty,"Bom",lcGnBomFl,lcGnDetFl,;
  lcGnOprFl,"",0, 0, 0, 0, 0, 0,'')

llNoThing  = gfThermo (lnTotal, lnCount, lcTitle, "")
ENDSCAN

IF lnTotal # lnCount
  llNoThing = gfThermo (lnTotal, lnTotal, lcTitle, "")
ENDIF

SELECT (lcGnBomFl)
GOTO TOP
llGoOn = llGoOn AND !EOF()

IF llGoOn
  SELECT (lcGnBomFl)
  SCAN
    IF cCatgTyp $ "FS" OR (cCatgTyp = "T" AND Trim_Invt)
      lcDesc    = SPACE(0)
      lnOnHand  = 0
      lnOnOrder = 0
      IF &lcGnBomFl..cCatgTyp = "S"
        IF SEEK(&lcGnBomFl..Item, "Style")
          lcDesc    = Style.Desc
          lnOnHand  = Style.TotStk
          lnOnOrder = Style.TotOrd
        ENDIF
      ELSE
        IF SEEK(SUBSTR(&lcGnBomFl..Item,1,7)+&lcGnBomFl..IClr, "Fabric")
          lcDesc    = Fabric.Desc
          lnOnHand  = Fabric.OnHand
          lnOnOrder = Fabric.OnOrder
        ENDIF
      ENDIF
      REPLACE &lcGnBomFl..cDesc    WITH lcDesc    ,;
        &lcGnBomFl..nOnHand  WITH lnOnHand  ,;
        &lcGnBomFl..nOnOrder WITH lnOnOrder

      *B602650,1 YMA 03/08/99 Let the remaining qty show +ve and -ve qty.
      lnRemain = lnOnHand+lnOnOrder-&lcGnBomFl..Req_Qty
      *lnRemain = MIN(lnOnHand+lnOnOrder-&lcGnBomFl..Req_Qty,0)
      *B602650,1 YMA 03/08/99 End.

      IF llEmptyLns
        REPLACE &lcGnBomFl..nDReq_Qty WITH 0 ,;
          &lcGnBomFl..nDUseThis WITH 0 ,;
          &lcGnBomFl..nUseThis  WITH 0 ,;
          &lcGnBomFl..nRemain   WITH lnOnHand+lnOnOrder
      ELSE
        REPLACE &lcGnBomFl..nDReq_Qty WITH &lcGnBomFl..Req_Qty ,;
          &lcGnBomFl..nDUseThis WITH &lcGnBomFl..Req_Qty ,;
          &lcGnBomFl..nUseThis  WITH &lcGnBomFl..Req_Qty ,;
          &lcGnBomFl..nRemain   WITH lnRemain
      ENDIF

    ELSE
      BLANK
      DELETE
    ENDIF
  ENDSCAN

  SELECT (lcGnBomFl)
  SET ORDER TO TAG (lcGnBomFl)
  FOR lnType = 1 TO 5
    DO WHILE SEEK(STR(lnType,1)+SPACE(1))
      REPLACE cShowType WITH '1'
      IF !SEEK(STR(lnType,1)+'0')
        INSERT INTO (lcGnBomFl) (TYP,cShowType,Item) VALUES ;
          (STR(lnType,1),'0',;
          PADC(SPACE(1)+laSetups[1+lnType,2]+SPACE(1),19,"*"))
      ENDIF
    ENDDO
  ENDFOR
  GOTO TOP
ELSE
  *-- Nothing To Display
  = gfModalGen('TRM38124B00000')
ENDIF

SELECT(lnAlias)

RETURN (llGoOn)

*:----------------------------------------------------------------

FUNCTION lfClsFls

IF USED(lcGnBomFl)
  USE IN (lcGnBomFl)
ENDIF
ERASE (gcWorkDir+lcGnBomFl+".DBF")
ERASE (gcWorkDir+lcGnBomFl+".CDX")
ERASE (gcWorkDir+lcGnBomFl+".FPT")

IF USED(lcGnOprFl)
  USE IN (lcGnOprFl)
ENDIF
ERASE (gcWorkDir+lcGnOprFl+".DBF")
ERASE (gcWorkDir+lcGnOprFl+".CDX")
ERASE (gcWorkDir+lcGnOprFl+".FPT")

IF USED(lcGnDetFl)
  USE IN (lcGnDetFl)
ENDIF
ERASE (gcWorkDir+lcGnDetFl+".DBF")
ERASE (gcWorkDir+lcGnDetFl+".CDX")
ERASE (gcWorkDir+lcGnDetFl+".FPT")


IF USED(lcMyMstFl)
  USE IN (lcMyMstFl)
ENDIF
ERASE (gcWorkDir+lcMyMstFl+".DBF")
ERASE (gcWorkDir+lcMyMstFl+".CDX")
ERASE (gcWorkDir+lcMyMstFl+".FPT")

*:----------------------------------------------------------------

FUNCTION lfChkSheet
PRIVATE lnAlias, llRet

lnAlias = SELECT(0)
llRet   = .T.
llFound = .F.

SELECT (lcMstTmpFl)
SCAN
  llFound   = .T.
  lcToFined = SUBSTR(&lcItmName, 1, LEN(lcMjrMsk))

  IF SEEK(lcToFined, "BOM")
    IF INLIST(lcTranType,'M','T')
      EXIT
    ENDIF
  ELSE
    *-- Cost sheet not found for XXXX : 'XXXX' .
    *-- Cannot proceed.
    *-- < Ok >
    lcStr = PROPER(lcMjrHdr)+'|'+ALLTRIM(lcToFined)
    = gfModalGen('TRM38123B00000','ALERT', lcStr)
    llRet = .F.
    EXIT
  ENDIF
ENDSCAN

SELECT(lnAlias)

RETURN (llRet AND llFound)

*:----------------------------------------------------------------

FUNCTION lfvView
*--Ramy
*IF lfGenLines()
IF lfGenLines(.F.)
*--Ramy
  lcViewName = "Lines"

  PUSH KEY CLEAR
  ON KEY LABEL TAB        DO lfGnVTab
  ON KEY LABEL ESC        DO lfGnVEsc
  ON KEY LABEL BACKTAB    DO lfGnVSTb
  ON KEY LABEL Ctrl+ENTER lnDummy = 1
  ON KEY LABEL Ctrl+HOME  lnDummy = 1
  ON KEY LABEL Ctrl+W     lnDummy = 1
  ON KEY LABEL Ctrl+END   lnDummy = 1
  ON KEY LABEL ALT+T ACTIVATE WINDOW (lcViewName)

  DO (gcScrDir+"MFGnLnV.SPR")

  RELEASE WINDOW (lcViewName)
  POP KEY
  USE IN (lcGnTmpFl)
ENDIF

*:----------------------------------------------------------------

FUNCTION lfGnVewBr

lcFields   = lcPItmName+      ":H='"+lcAllHdr+"':R," +;
  lcPQtyName+"1"+":6:H='Size1':R," +;
  lcPQtyName+"2"+":6:H='Size2':R," +;
  lcPQtyName+"3"+":6:H='Size3':R," +;
  lcPQtyName+"4"+":6:H='Size4':R," +;
  lcPQtyName+"5"+":6:H='Size5':R," +;
  lcPQtyName+"6"+":6:H='Size6':R," +;
  lcPQtyName+"7"+":6:H='Size7':R," +;
  lcPQtyName+"8"+":6:H='Size8':R," +;
  lcPTotName+    ":6:H='Total':R " +;
  IIF(laSetups[1,2]="Y",","+lcPWarName+":R:H='Warehouse'","")                       +;
  IIF(laSetups[7,2]="Y",","+lcPDyeName+":R:H='Dyelot'   ","")

SELECT (lcGnTmpFl)
GOTO TOP
BROWSE FIELDS &lcFields WINDOW GenLineV2 IN WINDOW GenLineV1 ;
  LOCK 0 SAVE NOWAIT NOEDIT NOCLEAR NOAPPEND NODELETE ;
  NOMENU TITLE (lcViewName)

*:----------------------------------------------------------------

FUNCTION lfGnVTab

ON KEY LABEL TAB
DO CASE
CASE WONTOP() = (lcViewName)
  ACTIVATE WINDOW GenLineV3
  _CUROBJ = OBJNUM(pbGnVClose)

CASE WONTOP() # (lcViewName) AND SYS(18) = "PBGNVCLOSE"
  KEYBOARD "{ALT+T}"

OTHERWISE
  _CUROBJ = _CUROBJ + 1
ENDCASE
ON KEY LABEL TAB DO lfGnVTab

*:----------------------------------------------------------------

FUNCTION lfGnVEsc

ACTIVATE WINDOW GenLineV3
_CUROBJ = OBJNUM(pbGnVClose)
KEYBOARD "{ENTER}"

*:----------------------------------------------------------------

FUNCTION lfGnVSTb

ON KEY LABEL BACKTAB
DO CASE
CASE WONTOP() = (lcViewName)
  ACTIVATE WINDOW GenLineV3
  _CUROBJ = OBJNUM(pbGnVClose)

CASE WONTOP() # (lcViewName) AND SYS(18) = "PBGNVCLOSE"
  KEYBOARD "{ALT+T}"

OTHERWISE
  _CUROBJ = _CUROBJ - 1
ENDCASE
ON KEY LABEL BACKTAB DO lfGnVSTb

*:----------------------------------------------------------------

FUNCTION lfGenLines

PARAMETER llAply

SELECT (lcGnBomFl)
*LOCATE FOR nUseThis > 0
LOCATE FOR nUseThis > 0 .AND. !EMPTY(cSelect)
IF !FOUND()
  *-- Since total compoents used is zero, no lines could be generated.
  RETURN gfModalGen('TRM38121B00000') = 2
ENDIF

lcTmp = gfTempName()
CREATE TABLE (gcWorkDir+lcTmp) (STYLE C(19), nTotQty N(12,4), nOrgTot N(10))
INDEX ON STYLE TAG (lcTmp)

SELECT (lcGnDetFl)
SET ORDER TO TAG (lcGnDetFl)
*--new
set relation to cBomTyp + "1" + Item + IClr into (lcGnBomFl)
*--new
*SCAN FOR cCatgTyp $ "SF" OR (cCatgTyp = "T" AND SEEK(SUBSTR(Item,1,7), "Fabric"))
SCAN FOR (cCatgTyp $ "SF" OR (cCatgTyp = "T" AND SEEK(SUBSTR(Item,1,7), "Fabric"))) .AND. &lcGnBomFl..cSelect = '»'
  IF !SEEK(&lcGnDetFl..Style, lcTmp)
    SELECT (lcTmp)
    APPEND BLANK
    REPLACE STYLE WITH &lcGnDetFl..Style
    lnOrgTot  = 0
    lnNoOfRec = 0
    SELECT (lcMyMstFl)
    IF SEEK(&lcTmp..Style, lcMyMstFl)
      SELECT (lcMyMstFl)
      SCAN REST WHILE &lcMyMstFl..Style = &lcTmp..Style
        lnOrgTot  = lnOrgTot  + &lcTotName
      ENDSCAN
    ENDIF
    SELECT (lcTmp)
    REPLACE nOrgTot  WITH lnOrgTot
  ENDIF

  SELECT (lcGnDetFl)
  IF SEEK(cBomTyp + "1" + Item + IClr, lcGnBomFl)
    IF &lcGnBomFl..Req_Qty = 0 OR &lcGnDetFl..UnitQty = 0
      lnNewTot = 0
    ELSE
      lnNewTot = (&lcGnBomFl..nUseThis / &lcGnBomFl..Req_Qty  * ;
                &lcGnDetFl..ItemQty) / &lcGnDetFl..UnitQty
    ENDIF
      IF lnNewTot > 0
        SELECT (lcTmp)
        REPLACE nTotQty WITH IIF(nTotQty=0, lnNewTot, MIN(nTotQty, lnNewTot))
      ENDIF
    ELSE
      lnNewTot = 0
    *-E301410,1 RAMY [start]
    *ENDIF
    *-E301410,1 RAMY [end]
  ENDIF
ENDSCAN
*--new
SELECT (lcGnDetFl)
set relation to

SELECT (lcTmp)
REPLACE ALL nTotQty WITH ROUND(nTotQty,0)

SELECT (lcMyMstFl)
lnFields = AFIELDS(laFile)
DIMENSION laFile[lnFields+1,4]
laFile[lnFields+1,1] = 'nDiff'
laFile[lnFields+1,2] = 'N'
laFile[lnFields+1,3] = 20
laFile[lnFields+1,4] = 17
CREATE TABLE (gcWorkDir+lcGnTmpFl) FROM ARRAY laFile
SELECT (lcGnTmpFl)
INDEX ON &lcPItmName+STR(nDiff,20,17) TAG (lcGnTmpFl)

SELECT (lcTmp)
SCAN
  IF SEEK(&lcTmp..Style, lcMyMstFl)
    	lnStyActTot = 0
    SELECT (lcMyMstFl)
    lnSclCnt = lfGtSclInf(&lcPItmName)
    SCAN REST WHILE &lcPItmName = &lcTmp..Style
      SCATTER MEMVAR MEMO
      lnOrgTot = &lcTmp..nOrgTot
      lnNewTot = &lcTmp..nTotQty
      SELECT (lcGnTmpFl)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE (lcPQtyName+"1") WITH INT(&lcQt1Name/lnOrgTot*lnNewTot),;
        (lcPQtyName+"2") WITH INT(&lcQt2Name/lnOrgTot*lnNewTot),;
        (lcPQtyName+"3") WITH INT(&lcQt3Name/lnOrgTot*lnNewTot),;
        (lcPQtyName+"4") WITH INT(&lcQt4Name/lnOrgTot*lnNewTot),;
        (lcPQtyName+"5") WITH INT(&lcQt5Name/lnOrgTot*lnNewTot),;
        (lcPQtyName+"6") WITH INT(&lcQt6Name/lnOrgTot*lnNewTot),;
        (lcPQtyName+"7") WITH INT(&lcQt7Name/lnOrgTot*lnNewTot),;
        (lcPQtyName+"8") WITH INT(&lcQt8Name/lnOrgTot*lnNewTot),;
        (lcPTotName    ) WITH EVAL(lcPQtyName+"1")+;
        EVAL(lcPQtyName+"2")+;
        EVAL(lcPQtyName+"3")+;
        EVAL(lcPQtyName+"4")+;
        EVAL(lcPQtyName+"5")+;
        EVAL(lcPQtyName+"6")+;
        EVAL(lcPQtyName+"7")+;
        EVAL(lcPQtyName+"8")
      lnShouldBe = (&lcTotName/lnOrgTot)*lnNewTot
      IF lnShouldBe # EVAL(lcPTotName)
        lnDiff = lnShouldBe - EVAL(lcPTotName)
        FOR lnI = 1 TO lnDiff
          IF lnI <= lnSclCnt
            lcSize = lcPQtyName + STR(lnI,1)
            lnSize = &lcSize
            REPLACE &lcSize WITH lnSize + 1
          ENDIF
        ENDFOR
        REPLACE (lcPTotName) WITH EVAL(lcPQtyName+"1")+EVAL(lcPQtyName+"2")+;
          EVAL(lcPQtyName+"3")+EVAL(lcPQtyName+"4")+;
          EVAL(lcPQtyName+"5")+EVAL(lcPQtyName+"6")+;
          EVAL(lcPQtyName+"7")+EVAL(lcPQtyName+"8")
      ENDIF
      lnStyActTot = lnStyActTot + EVAL(lcPTotName)
      REPLACE nDiff WITH lnShouldBe - EVAL(lcPTotName)
    ENDSCAN

    IF lnStyActTot # lnNewTot AND SEEK(&lcTmp..Style, lcGnTmpFl)
      lnDiff    = lnNewTot - lnStyActTot
      SELECT (lcGnTmpFl)
      SCAN REST WHILE STYLE = &lcTmp..Style AND lnDiff > 0
        REPLACE (lcPQtyName+"1") WITH EVAL((lcPQtyName+"1"))+1 ,;
          (lcPTotName    ) WITH EVAL(lcPTotName)+1
        lnStyActTot = lnStyActTot + 1
        lnDiff      = lnNewTot    - lnStyActTot
      ENDSCAN
    ENDIF
  ENDIF
ENDSCAN


*-E301410,1 RAMY [start] if we are coming from generated PO from SO check the Qtys if decreased
DIME laObj[8]
IF llViewMode .AND. llAply
  SELECT (lcGnTmpFl)
  SET RELATION TO EVALUATE(lcPItmName) INTO (lcMstTmpFl)
  SCAN
    lcCtPkKey = IIF(gcAct_Appl = 'PO' , '2' , '1') + laData[1] + EVALUATE(lcGnTmpFl + '.' + lcPItmName)
    IF !USED(lcCutPick)
      *-Create Temp Cut Pick file.
      SELECT CUTPICK
      =AFIELDS(laFStru)
      lnFStru = ALEN(laFStru,1)
      DIMENSION laFStru[lnFStru+9,4]
      FOR I=1 TO 9
        laFStru[lnFStru+I,1] = 'nCurPck'+STR(I,1)
        laFStru[lnFStru+I,2] = 'N'
        laFStru[lnFStru+I,3] = 6
        laFStru[lnFStru+I,4] = 0
        IF I = 9
          laFStru[lnFStru+I,1] = 'nTotCur'
          laFStru[lnFStru+I,2] = 'N'
          laFStru[lnFStru+I,3] = 6
          laFStru[lnFStru+I,4] = 0
        ENDIF
      ENDFOR
      =gfCrtTmp(lcCutPick,@laFStru,'Trancd+cTktNo+Style+cOrdLine+Order',lcCutPick)
      SELECT (lcCutPick)
      SET ORDER TO 1
    ELSE
      SELECT (lcCutPick)
      IF gcAct_Appl = 'MF'
        lcOrder = ORDER()
        USE IN (lcCutPick)
        USE (gcWorkDir + lcCutPick) EXCLUSIVE
        INDEX ON Trancd+cTktNo+Style+cOrdLine+Order TAG lcCutPick
      ENDIF
    ENDIF
    SELECT CUTPICK
    SCAN WHILE TranCd + cTktNo + STYLE = lcCtPkKey
      SCATTER TO laFields
      SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laOdOrd
      SELECT (lcCutPick)
      LOCATE FOR CUTPICK.trancd+CUTPICK.ctktno+CUTPICK.ctktlineno+CUTPICK.order+CUTPICK.style+CUTPICK.cordline = ;
                  trancd + ctktno + ctktlineno + order + style + cordline
      *LOCATE FOR lcCtPkKey + ORDER = CUTPICK.TranCd + CUTPICK.ctktno + CUTPICK.Style + CUTPICK.Order
      *LOCATE FOR IIF(gcAct_Appl = 'PO' , '2' , '1') + laData[1] = CUTPICK.TranCd + CUTPICK.ctktno
      IF !FOUND()
        SELECT (lcCutPick)
        APPEND BLANK
        GATHER FROM laFields
        GATHER FROM laOdOrd FIELDS nCurPck1 , nCurPck2 , nCurPck3 , nCurPck4 , nCurPck5 , nCurPck6 , nCurPck7 , nCurPck8 , nTotCur
      ENDIF
      *CONT
    ENDSCAN
    SELECT (lcGnTmpFl)
    IF EVALUATE(lcPTotName) <  EVALUATE(lcMstTmpFl + '.' + lcPTotName)
      lcMsgStr = EVALUATE(lcMstTmpFl + '.' + IIF(gcAct_Appl = 'PO' , 'PO' , 'cuttkt')) + '|' + EVALUATE(lcMstTmpFl+'.'+lcPItmName) + '|' + IIF(gcAct_Appl = 'PO' , 'P/O' , 'C\T')
      *-- Message-> "The quantity was decreased for PO \ Style, Please adjust the allocated quantity or remove 
      *--            the link with SO for this SO/CT line for decreased sizes"
      *-- Button -> <Update SO allocated qty> <Remove link with so for this line>
      
      IF gfModalGen('INM34169B34016' , 'ALERT' , lcMsgStr) = 2
        IF SEEK(lcCtPkKey , lcCutPick)
          SELECT (lcCutPick)
          SCAN REST WHILE IIF(gcAct_Appl = 'PO' , '2' , '1') + cTktNo + Style = lcCtPkKey
            FOR lnCnt = 1 TO 8
              lcCnt = ALLTRIM(STR(lnCnt))
              REPLACE QTY&lcCnt WITH 0
            ENDFOR
            REPLACE TOTQTY WITH 0
          ENDSCAN
        ENDIF          
        SELECT (lcGnTmpFl)
        FOR lnCnt = 1 TO 8
          lcCnt = ALLTRIM(STR(lnCnt))
          REPLACE Ord&lcCnt WITH 0
        ENDFOR
        REPLACE TOTORD WITH 0
      ELSE
        laObj = .F.
        SELECT (lcCutPick)
        lcTktNo  = cTktNo
        lcTranCd = IIF(gcAct_Appl = 'PO' , '2' , '1')
        COUNT TO lcCnt FOR trancd + cTktNo + EVALUATE(lcPItmName) = lcTranCd + lcTktNo + EVALUATE(lcGnTmpFl + '.' + lcPItmName)
        IF lcCnt > 1
          FOR lnI = 1 TO 8
            lcI = ALLTRIM(STR(lnI))
            IF EVALUATE(lcPQtyName + lcI) <>  EVALUATE(lcGnTmpFl + '.' + lcPQtyName + lcI)
              laObj[lnI] = .T.
            ENDIF
          ENDFOR
          =lfEdtAloQty()
        ELSE
          SELECT (lcGnTmpFl)
          FOR lnCont = 1 TO 8
            lcCont = ALLTRIM(STR(lnCont))
            REPLACE ORD&lcCont WITH QTY&lcCont
          ENDFOR
          REPLACE TOTORD     WITH ORD1+ORD2+ORD3+ORD4+ORD5+ORD6+ORD7+ORD8
          SELECT (lcCutPick)
          IF SEEK(lcCtPkKey)
            FOR lnCont = 1 TO 8
              lcCont = ALLTRIM(STR(lnCont))
              REPLACE Qty&lcCont WITH &lcGnTmpFl..Ord&lcCont
            ENDFOR
            REPLACE TOTQTY   WITH QTY1 + QTY2 + QTY3 + QTY4 + QTY5 + QTY6 + QTY7 + QTY8
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  SELECT (lcGnTmpFl)
  SET RELATION TO 
ENDIF

IF TYPE('lcOrder') = 'C'
  SELECT (lcCutPick)
  SET ORDER TO (lcOrder)
ENDIF
*-E301410,1 RAMY [end]
USE IN (lcTmp)
ERASE (gcWorkDir+lcTmp+".DBF")
ERASE (gcWorkDir+lcTmp+".CDX")
ERASE (gcWorkDir+lcTmp+".FPT")

*:----------------------------------------------------------------

FUNCTION lfGtSclInf
PARAMETERS lcStyle
PRIVATE    llRetVal

llRetVal = 0
IF SEEK(lcStyle, "Style") AND SEEK("S"+Style.Scale, "Scale")
  llRetVal = Scale.Cnt
ENDIF

RETURN (llRetVal)

*:----------------------------------------------------------------


*:*************************************************************
*! Name      : lfvSelect
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : The Select Buttons valid 
*:*************************************************************
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
*: E301410,1 RAMY
FUNCTION lfvSelect
PARAMETER lcSelWhat

SELECT (lcGnBomFl)

DO CASE
CASE lcSelWhat = 'S'
  IF cShowType = '0'
    RETURN
  ELSE
    REPLACE cSelect WITH IIF(cSelect="»",'','»')
  ENDIF

CASE lcSelWhat = 'A'
  REPLACE ALL cSelect WITH '»' FOR cShowType = '1'

CASE lcSelWhat = 'N'
  REPLACE ALL cSelect WITH ' '

CASE lcSelWhat = 'V'
  SCAN FOR cShowType = '1'
    IF cSelect = '»'
      REPLACE cSelect WITH ' '
    ELSE
      REPLACE cSelect WITH '»'
    ENDIF
  ENDSCAN
ENDCASE


SELECT (lcGnBomFl)
LOCATE FOR cSelect="»" .AND. cShowType = '1'
lcDataSel = IIF(FOUND(),'ENABLE','DISABLE')
LOCATE FOR cSelect=" " .AND. cShowType = '1'
lcDataUSel= IIF(FOUND(),'ENABLE','DISABLE')
SHOW GET pbSelAll    &lcDataUSel
SHOW GET pbApprove   &lcDataSel
SHOW GET lnAvailable &lcDataSel
SHOW GET pbSelNone   &lcDataSel


*:*************************************************************
*! Name      : lfwWhen
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : The "Use This" field when
*:*************************************************************
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
*: E301410,1 RAMY
FUNCTION lfwWhen

RETURN IIF(EMPTY(cSelect) , .F. , .T.)



*:*************************************************************
*! Name      : lfCtPkTrBr
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : Browse the allocated orders lines.
*:*************************************************************
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
*: E301410,1 RAMY
FUNCTION lfCtPkTrBr
PRIVATE lcField

GO TOP
lcToBrow = "Order :H='Order# ':R:W=.F."
FOR lnI = 1 TO Scale.Cnt
  lcIStr  = STR(lnI,1)
  lcField = lcCutPick + ".QTY" + lcIStr
  lcHdr   = "'" + ALLTRIM(Scale.Sz&lcIStr) + "'"
  lcToBrow = lcToBrow + ", &lcField:H=&lcHdr. :W=laObj["+lcIStr+"] AND lfwCPGtFld() :B=0,99999 :V=lfvCPGtFld(&lcIStr)"
ENDFOR
lcToBrow = lcToBrow + ", &lcCutPick..TOTQTY:H='TotCut':W=.F."

BROWSE FIELDS &lcToBrow KEY lcCtPkKey LOCK 0 ;
  NOAPPEND NOCLEAR NODELETE NOWAIT SAVE NOMENU   ;
  TITLE (lcCPBrTtl) WINDOW CtPkTr2 IN WINDOW CtPkTr1
RETURN

*:*************************************************************
*! Name      : lfwCPGtFld
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : Save the old value of the current edited allocated
*!             order line quantity.
*:*************************************************************
*: E301410,1 RAMY
FUNCTION lfwCPGtFld

lnOldQty = EVAL(VARREAD())
RETURN

*:*************************************************************
*! Name      : lfCPTab
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : Tab trapping function for the order line
*!             allocation browse.
*:*************************************************************
*: E301410,1 RAMY
FUNCTION lfCPTab

DO CASE
CASE WONTOP() = (lcCPBrTtl)
  ACTIVATE WINDOW CtPkTr3
  _CUROBJ = OBJNUM(pbOk)
CASE WONTOP() # (lcCPBrTtl) AND SYS(18) = "PBCAN"
  KEYBOARD "{ALT+B}"
OTHERWISE
  _CUROBJ = _CUROBJ + 1
ENDCASE

*:*************************************************************
*! Name      : lfCPStb
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : Shit+Tab trapping function for the order line
*!             allocation browse.
*:*************************************************************
*: E301410,1 RAMY
FUNCTION lfCPStb

DO CASE
CASE WONTOP() = (lcCPBrTtl)
  ACTIVATE WINDOW CtPkTr3
  _CUROBJ = OBJNUM(pbCan)
CASE WONTOP() # (lcCPBrTtl) AND SYS(18) = "PBOK"
  KEYBOARD "{ALT+B}"
OTHERWISE
  _CUROBJ = _CUROBJ - 1
ENDCASE

*:*************************************************************
*! Name      : lfCPEsc
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : Escape trapping function for the order line
*!             allocation browse.
*:*************************************************************
*: E301410,1 RAMY
FUNCTION lfCPEsc

ACTIVATE WINDOW CtPkTr3
_CUROBJ = OBJNUM(pbCan)
KEYBOARD "{ENTER}"

*:*************************************************************
*! Name      : lfvCPGtFld
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : To validate the edited allocated order line
*!              quantity.
*:*************************************************************
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
*: E301410,1 RAMY

FUNCTION lfvCPGtFld
PARAMETER lnObj
PRIVATE lnObj, lcObj, lnAlias

lnAlias  = SELECT(0)
lcObj    = STR(lnObj,1)
lnNewQty = &lcCutPick..Qty&lcObj
lnCurQty = &lcCutPick..nCurPck&lcObj

SELECT (lcCutPick)
IF lnNewQty > lnCurQty
  *-- The allocated quantity cannot be greater than 9999.
  *-- < Ok >
  = gfModalGen('TRM38107B00000','DIALOG',ALLTRIM(STR(lnCurQty,6)) )
  REPLACE Qty&lcObj WITH lnOldQty
ELSE
  REPLACE TotQty  WITH TotQty - lnOldQty + lnNewQty
ENDIF
SELECT(lnAlias)
RETURN

*:*************************************************************
*! Name      : lfvCPOk
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : Ok button in the allocated order lines brose screen
*!             valid function.
*:*************************************************************
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
*: E301410,1 RAMY

FUNCTION lfvCPOk
PRIVATE lnAlias

lnAlias = SELECT(0)

SELECT (lcCutPick)
SCAN
  FOR lnI = 1 TO 8
    lcSzNo     = ALLTRIM(STR(lnI))
    lcCurToChk = "nCurPck" + lcSzNo
    lnVarToChk = 'm.' + lcPQtyName + lcSzNo
    lcFldToChk = lcPQtyName + lcSzNo
    llGoOut    = .F.
    llRetVal   = .F.
    SELECT (lcCutPick)
    SUM ALL &lcFldToChk, TotQty, &lcCurToChk, nTotCur ;
      FOR TranCd+cTktNo+STYLE = lcCtPkKey  ;
      TO lnNewVal, lnNewTot, lnOldVal, lnOldTot

    IF lnNewVal > EVALUATE(lnVarToChk)
      *-- The allocated quantity cannot be greater than 9999.
      *-- < Ok >
      = gfModalGen('TRM38107B00000','DIALOG',ALLTRIM(STR(evaluate(lnVarToChk),6)) )
    ELSE
      llGoOut  = .T.
      llRetVal = .T.
    ENDIF

    IF llGoOut
      SELECT (lcGnTmpFL)
      REPLACE Ord&lcSzNo WITH lnNewVal ,;
        TotOrd     WITH TotOrd - lnOldTot + lnNewTot
      CLEAR READ
    ENDIF
  ENDFOR
ENDSCAN

SELECT(lnAlias)


*:*************************************************************
*! Name      : lfvCPCan
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : Cancel button in the allocated order lines brose
*!             screen valid function.
*:*************************************************************
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
*: E301410,1 RAMY

FUNCTION lfvCPCan
PRIVATE lnAlias

lnAlias    = SELECT(0)
llRetVal   = .F.
*lcSzNo     = STR(lnSizeNo,1)

SELECT (lcCutPick)
SCAN
  FOR lnCount = 1 TO 8
    lcSzNo = ALLTRIM(STR(lnCount))
    REPLACE ALL Qty&lcSzNo WITH nCurPck&lcSzNo ,;
      TotQty     WITH nCurPck9
  ENDFOR
ENDSCAN
SELECT(lnAlias)

*:*************************************************************
*! Name      : lfEdtAloQty
*! Developer : Ramy Mabrouk
*! Date      : 06/06/2000
*! Purpose   : To edit the allocated orders quantity.
*:*************************************************************
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
*: E301410,1 RAMY

FUNCTION lfEdtAloQty
PRIVATE lnAlias

llRetVal        = .F.
llFrstTime      = .T.
lcCPBrTtl       = "Quantities Allocated from Order Lines"
lcToBrow        = SPACE(0)
lnOldQty        = 0
lnAlias         = SELECT(0)

PUSH KEY CLEAR
ON KEY LABEL TAB        DO lfCPTab
ON KEY LABEL ESC        DO lfCPEsc
ON KEY LABEL BACKTAB    DO lfCPStb
ON KEY LABEL Ctrl+ENTER lnDummy = 1
ON KEY LABEL Ctrl+HOME  lnDummy = 1
ON KEY LABEL Ctrl+W     lnDummy = 1
ON KEY LABEL Ctrl+END   lnDummy = 1
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcCPBrTtl)

SELECT (lcCutPick)
DO (gcScrDir+"MFCtPkTr.SPX")

POP KEY
RELEASE WINDOW (lcCPBrTtl)
SELECT (lnAlias)
RETURN (llRetVal)



*:----------------------------------------------------------------

FUNCTION newlfGenLines

PARAMETER llAply

SELECT (lcGnBomFl)
LOCATE FOR nUseThis > 0
IF !FOUND()
  *-- Since total compoents used is zero, no lines could be generated.
  RETURN gfModalGen('TRM38121B00000') = 2
ENDIF


lcTmp = gfTempName()
CREATE TABLE (gcWorkDir+lcTmp) (STYLE C(19), nTotQty N(12,4), nOrgTot N(10))
INDEX ON STYLE TAG (lcTmp)

SELECT (lcGnDetFl)
SET ORDER TO TAG (lcGnDetFl)

*--New
*SCAN FOR cCatgTyp $ "SF" OR (cCatgTyp = "T" AND SEEK(SUBSTR(Item,1,7), "Fabric"))
SCAN FOR (cCatgTyp $ "SF" OR (cCatgTyp = "T" AND SEEK(SUBSTR(Item,1,7), "Fabric"))) .AND. &lcGnBomFl..cSelect = '»'
  IF !SEEK(&lcGnDetFl..Style, lcTmp)
    SELECT (lcTmp)
    APPEND BLANK
    REPLACE STYLE WITH &lcGnDetFl..Style
    lnOrgTot  = 0
    lnNoOfRec = 0
    SELECT (lcMyMstFl)
    IF SEEK(&lcTmp..Style, lcMyMstFl)
      SELECT (lcMyMstFl)
      SCAN REST WHILE &lcMyMstFl..Style = &lcTmp..Style
        lnOrgTot  = lnOrgTot  + &lcTotName
      ENDSCAN
    ENDIF
    SELECT (lcTmp)
    REPLACE nOrgTot  WITH lnOrgTot
  ENDIF

  SELECT (lcGnDetFl)
  IF SEEK(cBomTyp + "1" + Item + IClr, lcGnBomFl)
    *-E301410,1 RAMY [start] If the only record selected calculate it
    *IF &lcGnBomFl..cSelect = '»'
    *-E301410,1 RAMY [end]
      IF &lcGnBomFl..Req_Qty = 0 OR &lcGnDetFl..UnitQty = 0
        lnNewTot = 0
      ELSE
        *TAK
        lnTot = 0
        FOR lnI = 1 to 8
          lcI = STR(lnI,1) 
          IF lcI $ &lcGnBomFl..CSIZES
            lnTot = lnTot + &lcGnBomFl..Req_Qty&lnI
          ENDIF
        ENDFOR
        lnNewTot = (&lcGnBomFl..nUseThis / lnTot  * ;
                 &lcGnDetFl..ItemQty) / &lcGnDetFl..UnitQty

      ENDIF
      IF lnNewTot > 0
        SELECT (lcTmp)
        REPLACE nTotQty WITH IIF(nTotQty=0, lnNewTot, MIN(nTotQty, lnNewTot))
      ENDIF
    ELSE
      lnNewTot = 0
    *-E301410,1 RAMY [start]
    *ENDIF
    *-E301410,1 RAMY [end]
  ENDIF
ENDSCAN
*--new
SELECT (lcGnDetFl)
SET RELATION TO