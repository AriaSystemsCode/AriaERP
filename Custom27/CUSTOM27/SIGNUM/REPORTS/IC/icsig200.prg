************************************************************************
*: Program file  : ICSIG200.PRG (REFERE TO C#101526)
*: Program desc. : USTOMIZED OPEN TO SELL REPORT FOR Signum International
*:                 Inc.  Convert SIG200 from 2.6 to 2.7
*:         System: Aria Apparel System
*:         Module: IC
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls : FUNCTIONS  : lfSetRel,lfPrntRep,lfPrnHdr,lfGetWip,lfCalcuSub,
*:                      lfPrnLoc,lfPrnSubTo,lfvStyle,lfvFabric,lfvWareCode
*:                      lfNoOts,lfWhenOg,lfvOGStyle,lfwOldVal,lfSupExp,
*:                      lfEvalSegs,lfUpdFltVar
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************

*-- Initializing the necessary variables.
STORE SPACE(12) TO lcLowStyle, lcHigStyle
lcStyPic  = gfItemMask('HI')
lcFabric   = SPACE(7)
STORE SPACE(6) TO lcColor
STORE SPACE(2) TO laSeason,laDivision,lcGroup,lcSeason,lcDivision
lcPattern  = SPACE(10)
lcStatus   = 'A'
lcQtyType  = ' '
lnMinQty   = 0
lcWareCode = SPACE(06)
lcOpnTitle = SPACE(30)
lcLocation = ''
R_TITLE    = 'STYLE QUANTITIES OPEN TO SELL SUMMARY'
*--- Function to Create filter.
lcMajPict = gfItemMask("PM")
lnMajPict = LEN(lcMajPict)
=lfUpdFltVar()
ROW        = 99
PageNo     = 0

IF lfSetRel()
  =lfPrntRep()
ENDIF

**************************************************************************
*: Program file  : lfSetRel.PRG (REFERE TO C#101526)
*: Program desc. : To create the necessary file
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls : None
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example            :  lfSetRel()
*:************************************************************************
*!
FUNCTION lfSetRel

IF llOgFltCh
  SELECT STYDYE
  USE IN IIF(USED(lcRpTmp),(lcRpTmp),0)
  COPY STRUCTURE TO (gcWorkDir+lcRpTmp)
  =gfOpenFile(gcWorkDir+lcRpTmp,'','EX')
  INDEX ON cWareCode+Style TAG (lcRpTmp)
  RETURN .T.
ENDIF
**************************************************************************
*: Program file  : lfSetRel.PRG (REFERE TO C#101526)
*: Program desc. : To print the report
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls : None
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example            :  =lfPrntRep()
*:************************************************************************
*!
FUNCTION lfPrntRep
PRIVATE lcFilter

IF llOgFltCh
  lcFilter = ".T."
  lcFilter = IIF(!EMPTY(lcFabric),lcFilter + " AND Fabric = lcFabric",lcFilter)
  lcFilter = IIF(!EMPTY(laSeason),lcFilter+" AND Season $ laSeason",;
                                                                 lcFilter)
  lcFilter = IIF(!EMPTY(laDivision),lcFilter+" AND cDivision $ laDivision",;
  											                   lcFilter)
  lcFilter = IIF(!EMPTY(lcGroup),lcFilter + " AND cStyGroup = lcGroup",;
                                                                lcFilter)
  lcFilter = IIF(!EMPTY(lcPattern),lcFilter+" AND Pattern = lcPattern",;
  															   lcFilter)
  lcFilter = IIF(lcStatus <> "B",lcFilter + " AND Status = lcStatus",;
  															   lcFilter)
  SELECT STYLE
  IF !EMPTY(lcLowStyle)
    lnOld = ORDER()
    SET ORDER TO cStyle
    =SEEK(lcLowStyle)
    SET ORDER TO &lnOld
  ELSE
    GO TOP
  ENDIF
  SCAN REST WHILE INKEY() <> 32 AND IIF(!EMPTY(lcHigStyle),cStyMajor <= lcHigStyle,.T.)
    IF !EMPTY(lcColor) AND !(SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen) $ lcColor)
      LOOP
    ENDIF  
    IF Style.MAKE 
      LOOP
    ENDIF
    IF !EVAL(lcFilter)
      LOOP
    ENDIF
    WAIT WINDOW "Selecting record for "+lcStyPic+"  : " + Style +'  - <SPACE BAR> to abort 'NOWAIT
    IF SEEK(STYLE+IIF(!EMPTY(lcWareCode),lcWareCode,''),'STYDYE')
       SELECT STYDYE
       IF !EMPTY(lcWareCode)
         SCATTER MEMVAR
         INSERT INTO(lcRpTmp) FROM MEMVAR       
       ELSE
         SCAN REST WHILE  STYLE+CWARECODE+DYELOT+STR(RECNO(),7) = STYLE.STYLE
           SCATTER MEMVAR
           INSERT INTO(lcRpTmp) FROM MEMVAR                
         ENDSCAN  
         SELECT STYLE
       ENDIF
    ENDIF
    SCATTER MEMVAR
    IF SEEK (STYLE+'P','PoSLn')
      SELECT PoSLn
      DO WHILE STYLE+cStyType+PO+STR(LINENO,4)+TRANCD+STR(RECNO(),7) = ;
               STYLE.Style+'P'
        lcPo = PO
        IF SEEK('P'+Po,'PosHdr') .AND. PoSHdr.Status $ 'OH' AND ;
          IIF(!EMPTY(lcWareCode),PosHdr.cWareCode = lcWareCode,.T.) AND ;
          !SEEK(STYLE+PosHdr.cWareCode,'STYDYE')
           m.cWareCode = PosHdr.cWareCode
           STORE 0 TO m.Stk1,m.Stk2,m.Stk3,m.Stk4,m.Stk5,m.Stk6,m.Stk7,m.Stk8,m.TotStk,;
                      m.Ord1,m.Ord2,m.Ord3,m.Ord4,m.Ord5,m.Ord6,m.Ord7,m.Ord8,m.TotOrd
           IF !SEEK(m.cWareCode+STYLE,'&lcRpTmp')           
             INSERT INTO (lcRpTmp) FROM MEMVAR
           ENDIF  
        ENDIF
        SCAN REST WHILE STYLE+cStyType+PO+STR(LINENO,4)+TRANCD+STR(RECNO(),7) = ;
                        STYLE.Style+'P'+lcPo
        ENDSCAN  
      ENDDO
    ENDIF   
    SELECT STYLE
  ENDSCAN
  WAIT CLEAR
ENDIF
SELECT (lcRpTmp)
GOTO TOP
IF EOF()
  *:C101333,1 SSH  Text "No Record Selected."
  =gfModalGen('TRM38155B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
IF lfNoOts()
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SET DEVICE TO SCREEN
WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
SET DEVICE TO PRINT

*-- To calculate the totals for warehouses
STORE 0 TO lnPosGrd,lnNegGrd,lnNetGrd
DO WHILE INKEY() <> 32 AND !EOF()
  lcWareH = cWareCode
  *-- To calculate the subtotal by warehouses
  STORE 0 TO lnPosWhSub,lnNegWhSub,lnNetWhSub
  =IIF(Row >= 55, lfPrnHdr(),.F.)
  llWare  = .T.
  DO WHILE !EOF() AND cWareCode = lcWareH
    lcStyle = Style
    *-- To calculate the subtotal by styles
    STORE 0 TO lnPosSub,lnNegSub,lnNetSub
    llOnce  = .T.
    SCAN REST WHILE cWareCode + SUBSTR(Style,1,lnMajPict) = lcWareH+ SUBSTR(lcStyle,1,lnMajPict)
      WAIT WINDOW "Printing "+lcStyPic+"  : " + Style +'  - <SPACE BAR> to abort 'NOWAIT
      =IIF(Row >= 55, lfPrnHdr(),.F.)
      =SEEK(Style,'Style')
      =SEEK('S'+Style.Scale,'Scale')
      *-- To calculate the qty per size,postive, and negativ qty
      STORE 0 TO lnPosQty, lnNegQty, lnOTS1, lnOTS2, lnOTS3, lnOTS4,;
                 lnOTS5, lnOTS6, lnOTS7
      FOR lnCounter = 1 TO Scale.Cnt
        lcCounter       = STR(lnCounter,1)
        *-- lfGetWip() function is used to calculate the wip qty from POs
        lnWipQty        = lfGetWip(Style,lcCounter)
        lnOTS&lcCounter = Stk&lcCounter + lnWipQty - Ord&lcCounter
        IF (lnOTS&lcCounter > 0) AND (lcQtyType $ 'PA')
          lnPosQty = lnPosQty + lnOTS&lcCounter
        ENDIF
        IF (lnOTS&lcCounter < 0 ) AND (lcQtyType $ 'NA')
          lnNegQty = lnNegQty + lnOTS&lcCounter
        ENDIF
      ENDFOR
      DO CASE
        CASE lcQtyType = 'P' AND lnPosQty < lnMinQty
          LOOP
        CASE lcQtyType = 'N' AND lnNegQty > lnMinQty
          LOOP
        CASE lnPosQty = 0 AND lnNegQty = 0
            LOOP 
      ENDCASE
      IF llWare
        @ Row,00 SAY "WareHouse : " + lcWareH
        ROW = ROW+1
        llWare = .F.
      ENDIF  
      IF llOnce
        @ Row,00 SAY "**  " + SUBSTR(Style,1,lnMajPict)
        lnCol = 43
        FOR lnCounter = 1 TO Scale.Cnt
          lcCount = STR(lnCounter,1)
          @ Row,lnCol SAY PADL(ALLTRIM(Scale.SZ&lcCount),5)
          lnCol = lnCol + 7
        ENDFOR
        llOnce = .F.
      ENDIF  
      Row = Row + 1
      =IIF(Row >= 55, lfPrnHdr(),.F.)
      *-- To get the color description.
      lcColorCod = SUBSTR(STYLE.STYLE,lnNonMajSt,lnFreeLen)
      lcClrDesc = gfCodDes(lcColorCod,'COLOR     ')
      @ Row,00 SAY lcColorCod
      @ Row,07 SAY SUBSTR(lcClrDesc,1,15)
      @ Row,23 SAY LEFT(Style.Desc,18)
      lnCol = 42
      FOR lnCounter = 1 TO Scale.Cnt
        lcCount = STR(lnCounter,1)
        @ Row,lnCol SAY lnOTS&lcCount PICTURE "999999"
        lnCol = lnCol + 7
      ENDFOR
      @ Row,92  SAY lnPosQty PICTURE "9999999"
      @ Row,101 SAY lnNegQty PICTURE "9999999"
      @ Row,109 SAY lnPosQty + lnNegQty PICTURE "999999999"
      *-- To calculate the subtotals
      =lfCalcuSub()
      IF lcQtyType $ 'PA'
        @ Row,123 SAY Style.PriceA PICTURE "9999.99"
      ENDIF  
      *-- If the setting of locations and chooses to print location
      *-- then lfPrnLoc() function will print the locations.
      IF llWareLoc AND lcLocation = "Y"
        =lfPrnLoc(Style+SPACE(06)+lcWareH)
      ENDIF  
    ENDSCAN
    IF lnPosSub <> 0 OR lnNegSub <> 0 OR lnNetSub <> 0
      *-- To print  the subtotal per style
      =lfPrnSubTo('S')
    ENDIF  
  ENDDO  
  *-- To print the subtotal per warehouse
  IF !llWare
    =lfPrnSubTo('W')
  ENDIF
ENDDO
=IIF(Row >= 55, lfPrnHdr(),.F.)
@ Row,00  SAY "** Total  **"
@ Row,90  SAY lnPosGrd  PICTURE "999999999"
@ Row,99  SAY lnNegGrd  PICTURE "999999999"
@ Row,108 SAY lnNetGrd  PICTURE "9999999999"
WAIT CLEAR
DO ENDREPORT
SET DEVICE TO SCREEN

**************************************************************************
*: Program file  : lfPrnHdr.PRG (REFERE TO C#101526)
*: Program desc. : To print the report headr
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls : None
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example            :  = lfPrnHdr()
*:************************************************************************
*!
FUNCTION lfPrnHdr

*-- To print the report header
PageNo = PageNo + 1
DO Rpt_Hdr WITH 'ICSIG200',lcOpnTitle,R_WIDTH
Row = 5
IF lcQtyType $ "PA"
  @ Row,00 SAY 'COLOR  COLOR DESCR.... STYLE DESCRIPTION     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7      POS      NEG       NET W.SALE UNIT'
  Row = Row + 1
  @ Row,00 SAY '                                                                                              O-T-S    O-T-S     O-T-S PRICE'
ELSE
  @ Row,00 SAY 'COLOR  COLOR DESCR.... STYLE DESCRIPTION     SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7      POS      NEG       NET'
  Row = Row + 1
  @ Row,00 SAY '                                                                                              O-T-S    O-T-S     O-T-S'
ENDIF
Row = Row + 1
@ Row,00 SAY REPLICATE('*',132)
Row = Row + 1

**************************************************************************
*: Program file  : lfGetWip.PRG (REFERE TO C#101526)
*: Program desc. : To calculate the wip qty per size
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls : None
*:************************************************************************
*: Passed Parameters  : lcKey   : Style
*:                    : lcQtyNo : The Qty per size.
*:************************************************************************
*: Example            :  = lfGetWip()
*:************************************************************************
FUNCTION lfGetWip
PARAMETERS lcKey,lcQtyNo
PRIVATE lnAlias

lnAlias = SELECT(0)
lnQty = 0
IF SEEK (lcKey,'PosLn')
  SELECT PosLn
  SCAN REST WHILE Style+cStyType = lcKey+'P'
  IF SEEK('P'+Po,'PosHdr') AND PosHdr.cWareCode = IIF(!EMPTY(lcWareCode),lcWareCode,lcWareH);
                       .AND. PoSHdr.Status $ 'OH'
     IF TranCd = '1'
       lnQty = lnQty + Qty&lcQtyNo
     ELSE
       lnQty = lnQty - Qty&lcQtyNo
     ENDIF
   ENDIF       
  ENDSCAN
ENDIF
SELECT(lnAlias)
RETURN (lnQty)

**************************************************************************
*: Program file  : lfCalcuSub.PRG (REFERE TO C#101526)
*: Program desc. : To calculate the subtotal per style and warehouse
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls : None
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example            :  = lfCalcuSub()
*:************************************************************************
FUNCTION lfCalcuSub

lnPosSub   = lnPosSub + lnPosQty
lnNegSub   = lnNegSub + lnNegQty
lnNetSub   = lnNetSub + lnPosQty + lnNegQty
lnPosGrd   = lnPosGrd + lnPosQty
lnNegGrd   = lnNegGrd + lnNegQty
lnNetGrd   = lnNetGrd + lnPosQty + lnNegQty
lnPosWhSub = lnPosWhSub + lnPosQty
lnNegWhSub = lnNegWhSub + lnNegQty
lnNetWhSub = lnNetWhSub + lnPosQty + lnNegQty 

**************************************************************************
*: Program file  : lfPrnLoc.PRG (REFERE TO C#101526)
*: Program desc. : To print the style/warehouse locations.
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls         : None
*:************************************************************************
*: Passed Parameters  : lcKey : Style+lcWareH
*:************************************************************************
*: Example            :  = lfPrnLoc()
*:************************************************************************
*!
FUNCTION lfPrnLoc
PARAMETERS lcKey
PRIVATE lnAlias,lnCol

lnAlias = SELECT(0)
lnCol   = 0

IF SEEK(lcKey,'WhsLoc')
  Row = Row + 1
  SELECT WhsLoc
  SCAN REST WHILE Style+SPACE(06)+cWareCode = lcKEy
    =IIF(Row >= 55, lfPrnHdr(),.F.)
    IF lnCol >= 130
      lnCol = 0
      Row   = Row + 1
      =IIF(Row >= 55, lfPrnHdr(),.F.)
    ENDIF
    @ Row,lnCol SAY cLocation
    lnCol = lnCol + LEN(ALLTRIM(cLocation)) + 2
  ENDSCA
  =IIF(Row >= 55, lfPrnHdr(),.F.)
ENDIF

SELECT(lnAlias)

**************************************************************************
*: Program file  : lfPrnSubTo.PRG (REFERE TO C#101526)
*: Program desc. : To print the style/warehouse locations.
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls         : None
*:************************************************************************
*: Passed Parameters  : lcType : 'S' for style
*:                               'W' for warehouse
*:************************************************************************
*: Example            :  = lfPrnSubTo()
*:************************************************************************
FUNCTION lfPrnSubTo
PARAMETERS lcType

IF lcType = 'S'
  Row = Row + 1
  @ Row,00 SAY REPLICATE ('-',132)
  Row = Row + 1
  @ Row,00  SAY "SubTotal for style : " + SUBSTR(lcStyle,1,lnMajPict)
  @ Row,90  SAY lnPosSub  PICTURE "999999999"
  @ Row,99  SAY lnNegSub  PICTURE "999999999"
  @ Row,108 SAY lnNetSub PICTURE "9999999999"
  Row = Row + 1
  @ Row,00 SAY REPLICATE ('-',132)
  Row = Row + 1
ELSE
  =IIF(Row >= 55, lfPrnHdr(),.F.)
  @ Row,00 SAY REPLICATE ('-',132)
  Row = Row + 1
  @ Row,00  SAY "SubTotal for warehouse : " + lcWareH
  @ Row,90  SAY lnPosWhSub  PICTURE "999999999"
  @ Row,99  SAY lnNegWhSub  PICTURE "999999999"
  @ Row,108 SAY lnNetWhSub  PICTURE "9999999999"
  Row = Row + 1
  =IIF(Row >= 55, lfPrnHdr(),.F.)
  @ Row,00 SAY REPLICATE ('-',132)
  Row = Row + 1
ENDIF

**************************************************************************
*: Program file  : lfNoOts.PRG (REFERE TO C#101526)
*: Program desc. : To check if there is OTS for the entered styles.
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls         : None
*:************************************************************************
*: Passed Parameters  : None.
*:************************************************************************
*: Example            :  = lfNoOts()
*:************************************************************************
FUNCTION lfNoOts

llSel_Dev =.F. && Flag to call Sel_Dev only if there is OTS to print.
DO WHILE !EOF()
  lcWareH = cWareCode
  
  *-- To calculate the subtotal by warehouses
  STORE 0 TO lnPosWhSub,lnNegWhSub,lnNetWhSub
  DO WHILE cWareCode = lcWareH
    lcStyle = Style

    *-- To calculate the subtotal by styles
    STORE 0 TO lnPosSub,lnNegSub,lnNetSub
    SCAN REST WHILE cWareCode+Style = lcWareH+lcStyle
      WAIT WINDOW "Printing "+lcStyPic+"  : " + Style +'  - <SPACE BAR> to abort 'NOWAIT
      =SEEK(Style,'Style')
      =SEEK('S'+Style.Scale,'Scale')
      *-- To calculate the qty per size,postive, and negativ qty
      STORE 0 TO lnPosQty, lnNegQty, lnOTS1, lnOTS2, lnOTS3, lnOTS4,;
                 lnOTS5, lnOTS6, lnOTS7

      FOR lnCounter = 1 TO Scale.Cnt
        lcCounter       = STR(lnCounter,1)

        *-- lfGetWip() function is used to calculate the wip qty from POs
        lnWipQty        = lfGetWip(Style,lcCounter)
        lnOTS&lcCounter = Stk&lcCounter + lnWipQty - Ord&lcCounter
        IF (lnOTS&lcCounter > 0) AND (lcQtyType $ 'PA')
          lnPosQty = lnPosQty + lnOTS&lcCounter
        ENDIF
        IF (lnOTS&lcCounter < 0 ) AND (lcQtyType $ 'NA')
          lnNegQty = lnNegQty + lnOTS&lcCounter
        ENDIF
      ENDFOR

      DO CASE
        CASE lcQtyType = 'P' AND lnPosQty < lnMinQty
          LOOP
        CASE lcQtyType = 'N' AND lnNegQty > lnMinQty
          LOOP
        CASE lnPosQty = 0 AND lnNegQty = 0
          LOOP 
      ENDCASE
      llSel_Dev = .T.
      EXIT            
    ENDSCAN  
    IF llSel_Dev
      EXIT      
    ENDIF      
  ENDDO  
  IF llSel_Dev
    EXIT      
  ENDIF        
ENDDO
WAIT CLEAR
IF !llSel_Dev
  *:C101333,1 SSH  Text "No Record Selected."
  =gfModalGen('TRM38155B00000','DIALOG' )
  RETURN (.T.)
ELSE
  RETURN (.F.)  
ENDIF

**************************************************************************
*: Program file  : lfWhenOg.PRG (REFERE TO C#101526)
*: Program desc. : Option Grid When Function.
*: Developer     : AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls         : None
*:************************************************************************
*: Passed Parameters  : None.
*:************************************************************************
*: Example            :  = lfWhenOg()
*:************************************************************************
FUNCTION lfWhenOg

R_WIDTH    = 'W'
llWareLoc =  (gfGetMemVar('M_WareLoc') = 'Y')

*!*************************************************************
*! Name      : lfvOGStyle
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : Valid function of the Style
*!*************************************************************
*! Called from : Option grid [Style Get field]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvOGStyle
PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = ALLTRIM(EVALUATE(SYS(18)))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))

  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.

  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , laOldVal)
  &lcObjName = ALLTRIM(lcObjVal)

ENDIF    && End of IF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)
*-- end of lfvOGStyle.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Ahmed Salah Shalaby -(SSH)
*!      Date : 05/10/99
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvFabric
*! Developer : 
*! Date      : 01/27/99
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT Fabric
lcFabOrder = ORDER()
SET ORDER TO Fabric
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
SELECT Fabric
SET ORDER TO &lcFabOrder
SELECT(lnAlias)

FUNCTION lfSupExp
CLEAR READ
DO CASE
  CASE lcRPSel='P'
    lnRPMinQ = 1  
  CASE lcRPSel='N'
    lnRPMinQ = -1
  CASE lcRPSel='A'
    lnRPMinQ = 0
ENDCASE

****************************************************************************
* FUNC: lfvWareHouse
* DESC: To valid the warehouse code.
****************************************************************************
FUNCTION lfvWareHouse

lcWareHo = VARREAD()
lcTag = ORDER('WAREHOUS')
SET ORDER TO WAREHOUS IN WAREHOUS
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcWareHo.,'WAREHOUS') 
    &lcWareHo = WAREHOUS.cWareCode
  ELSE
    &lcWareHo = gfBroWWare(.T.)
  ENDIF
ELSE
  &lcWareHo = ''
ENDIF
SET ORDER TO WAREHOUS IN WAREHOUS

*!*************************************************************
*! Name        : lfEvalSegs
*! Developer   : 
*! Date        : 
*! Purpose     : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs

lnMajSeg = gfItemMask('SM')  && No. of major segments.

*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.      
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]

*-- Eval Sort By arrays. [Begin]
DIMENSION laSortDesc[2,1],laSortVal[2,1]

laSortDesc[1,1] = lcStyMajor + ' Group' 
laSortDesc[2,1] = lcStyMajor
  
*-- laSortVal to hold the sorting method "S" for style "G" for group
laSortVal[1,1] = 'G'
laSortVal[2,1] = 'S'

RETURN ''


*!*************************************************************
*! Name        : lfUpdFltVar
*! Developer   : AHMED SALAH SHALABY (SSH)
*! Date        : 
*! Purpose     : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

lcPattern  = lcRpPat
lcStatus   = lcRPACB
lcQtyType  = lcRPSel
lnMinQty   = lnRPMinQ
lcWareCode = lcRpWrOnly
lcOpnTitle = lcRpOpTlt
lcLocation = lcRPPrtLoc

FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.FABRIC' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))

*--- Fabric
      lcFabric  = ALLTRIM(laOgFxFlt[lnInd,6])
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))

*--- Season
      laSeason = laOgFxFlt[lnInd,6]
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
*--- Color
      lcColor = ALLTRIM(laOgFxFlt[lnInd,6])
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
*--- laDivision
      laDivision = laOgFxFlt[lnInd,6]
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYMAJOR' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
*--- Style Major
      lcLowStyle = SUBSTR(laOgFxFlt[lnInd,6],1,19)
      lcHigStyle = SUBSTR(laOgFxFlt[lnInd,6],21,19)
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
*--- Style Group.
      lcGroup = ALLTRIM(laOgFxFlt[lnInd,6])
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfClearRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
ERASE &gcWorkDir.&lcRpTmp+'.DBF'
ERASE &gcWorkDir.&lcRpTmp+'.CDX'