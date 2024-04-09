*:***************************************************************************
*: Program file  : ICEIL65.PRG
*: Program desc. : TEMPORARY INVENTORY ADJUSTMENTS REPORT
*: System        : Aria Advantage Series.
*: Module        : INVENTORY CONTROL (IC)
*: Developer     : Khalid Mohi El-Din Mohamed (KHM)
*: Customer      : EILEEN FISHER
*:***************************************************************************
*: C101399,1 KHM 03/23/99
*:***************************************************************************

*-- To check if there are no batches selected then do not proceed.
IF !(USED(laOGFxFlt[1,6]) AND RECCOUNT(laOGFxFlt[1,6]) > 0))  
  =gfModalGen('TRM42169B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

*-- Initializing the necessary varibales.
TmpLines  = gfTempName()
lcBatchFl = laOGFxFlt[1,6]

*-- To restore the system's settings.
DIMENSION laSetup[2,2]
laSetUp[1,1] = 'M_DYELOT'
laSetUp[2,1] = 'M_WAREHOUSE'
=gfGetMemVar(@laSetUp,gcAct_Comp)

llDyeLot  = ALLTRIM(laSetUp[1,2]) = 'Y'
llMultiWH = ALLTRIM(laSetUp[2,2]) = 'Y'

*-- Opening the necessary files.
=gfOpenFile(gcDataDir+'TmpInvH' ,'TmpInvH' ,'SH')
=gfOpenFile(gcDataDir+'TmpInvL' ,'TmpInvLS','SH')
SET RELATION TO cBatchNo INTO TmpInvH
=gfOpenFile(gcDataDir+'Style' ,'Style','SH')

*-- Printing the report.
=lfPrnRep()

*!*************************************************************
*! Name      : lfPrnRep
*! Developer : Khalid Mhoi El-Din
*! Date      : 03/23/99
*! Purpose   : To print the report.
*!*************************************************************
*! Example            :  lfPrnRep()
*!*************************************************************
FUNCTION lfPrnRep

lcReport = 'ICEIL65'
R_Title  = 'TEMPORARY INVENTORY ADJUSTMENTS REPORT'
Row      = 99
lnMaxRow = 53
PageNo   = 0

SET TALK ON
SET TALK WINDOW ON
SELECT TmpInvL.* ;
  FROM (lcBatchFl),TmpInvL;
  WHERE TmpInvL.cBatchNo+TmpInvL.cSheet+TmpInvL.Style+TmpInvL.Color+;
        TmpInvL.cWareCode+TmpInvL.Dyelot = &lcBatchFl..cBatchNo AND;
        BETWEEN(TmpInvL.Style,lcLowStyle,lcHigStyle) AND TmpInvL.TotQty > 0;
  INTO CURSOR(TmpLines)
  
SET TALK OFF
SET TALK WINDOW OFF
WAIT CLEAR

IF _TALLY = 0
  =gfModalGen('TRM42170B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN  
ENDIF

SELECT (TmpLines)
IF lcRpSort = 'C'
  INDEX ON cBatchNo+Style+Color+cWareCode+Dyelot TAG(TmpLines) OF (TmpLines)
ELSE
  INDEX ON cBatchNo+cSheet+Style+Color TAG(TmpLines) OF (TmpLines)
ENDIF  

SET DEVICE TO PRINT

SELECT (TmpLines)
GOTO TOP
DO WHILE INKEY() <> 32 AND !EOF()
  lcBatchNo = cBatchNo
  PageNo = PageNo + 1
  DO RPT_HDR WITH lcReport,'',R_WIDTH
  Row = 5
  @ ROW+0,1 SAY 'BATCH :' + lcBatchNo + '  ' + TmpInvH.cBatDesc
  @ ROW+1,1 SAY 'SHEET  STYLE        COLOR  '+IIF(llMultiWH,'WAREHOUSE ','')+IIF(llDyeLot,'DYELOT     ','')+;
                ' SIZE1  SIZE2  SIZE3  SIZE4  SIZE5  SIZE6  SIZE7  SIZE8  '+' TOT ORG TOT ADJ BALANCE'
  @ Row+2,1 SAY REPLICATE('-',130)
  Row = Row + 3
  SCAN REST WHILE INKEY() <> 32  AND cBatchNo = lcBatchNo
    WAIT WINDOW "Batch#: " + lcBatchNo + " Style/Color: " +ALLTRIM(Style)+"/"+COLOR NOWAIT
    IF Row >= lnMaxRow
      PageNo = PageNo + 1
      DO RPT_HDR WITH lcReport,'',R_WIDTH
      Row = 5
      @ ROW+0,1 SAY 'BATCH :' + lcBatchNo + '  ' + TmpInvH.cBatDesc
      @ ROW+1,1 SAY 'SHEET  STYLE        COLOR  '+IIF(llMultiWH,'WAREHOUSE ','')+IIF(llDyeLot,'DYELOT     ','')+;
                    ' SIZE1  SIZE2  SIZE3  SIZE4  SIZE5  SIZE6  SIZE7  SIZE8  '+' TOT ORG TOT ADJ BALANCE'
      @ Row+2,1 SAY REPLICATE('-',130)
      Row = Row + 3
    ENDIF     
    @ Row,01 SAY cSheet
    @ Row,08 SAY LEFT(Style,12)
    @ Row,21 SAY Color  
    lnCol = 28
    IF llMultiWH
      @ Row,lnCol SAY cWareCode
      lnCol = 38
    ENDIF  
    IF llDyeLot
      @ Row,lnCol SAY Dyelot
      lnCol = lnCol + 11
    ENDIF
    @ Row,lnCol+00 SAY Qty1 PICTURE '999999'
    @ Row,lnCol+07 SAY Qty2 PICTURE '999999'  
    @ Row,lnCol+14 SAY Qty3 PICTURE '999999'
    @ Row,lnCol+21 SAY Qty4 PICTURE '999999'
    @ Row,lnCol+28 SAY Qty5 PICTURE '999999'
    @ Row,lnCol+35 SAY Qty6 PICTURE '999999'
    @ Row,lnCol+42 SAY Qty7 PICTURE '999999'
    @ Row,lnCol+49 SAY Qty8 PICTURE '999999'          
    @ Row,lnCol+58 SAY nTotOrg   PICTURE '9999999'
    @ Row,lnCol+66 SAY TotQty   PICTURE '9999999'
    @ Row,lnCol+74 SAY nBalance  PICTURE '9999999'  
    Row = Row + 1
  ENDSCAN
ENDDO
WAIT CLEAR
DO EndReport
SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : lfvStyle
*! Developer : Khalid Mhoi El-Din
*! Date      : 03/23/99
*! Purpose   : To validate the style's range.
*!*************************************************************
*! Example            :  lfvStyle()
*!*************************************************************
FUNCTION lfvStyle

PRIVATE lnAlias,lcStyOrder

lnAlias = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 

lcObjName = SYS(18)
lcObjVal  = EVALUATE(SYS(18))

IF lcObjName = "LCOGVALUEF" 
  IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE')
    lcObjVal   = gfStyBrw('M',"","",.F.)
    &lcObjName = IIF(!EMPTY(lcObjVal),lcObjVal,"")
  ENDIF
  lcLowStyle = lcObjVal
ELSE
  IF !EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE')
    lcObjVal   = gfStyBrw('M',"","",.F.)
    &lcObjName = IIF(!EMPTY(lcObjVal),lcObjVal,"")
  ENDIF
  lcHigStyle = lcObjVal
ENDIF

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfRptWhen
*! Developer : Khalid Mhoi El-Din
*! Date      : 03/23/99
*! Purpose   : To initialize the width of the report.
*!*************************************************************
*! Example            :  lfRptWhen()
*!*************************************************************
FUNCTION lfRptWhen

R_Width  = 'W'