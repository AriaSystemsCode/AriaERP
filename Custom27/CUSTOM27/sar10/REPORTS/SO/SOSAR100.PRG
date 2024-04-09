*:***************************************************************************
*: Program file  : SOSAR100.PRG
*: Program desc. : CUSTOMIZED SHIPPING LABLE PRINT FROM ORDERLINE FOR SAR100.
*: Date          : 01/12/2003
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER (SO)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tester        : 
*: Tracking Job Number: C102794
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO SOSAR100
*:***************************************************************************
*: Modifications :
***************************************************************************

*--The Color length
STORE 0 TO lnClrLnLb , lnClrPosLb
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnLb  = LEN(laItemSeg[lnCount,3])
    lnClrPosLb = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--Section Initial the variables.
XFILTER = ' '

*--Section to check if the data is empty return.
IF EMPTY(ldlDate) .AND. EMPTY(ldhDate)
  WAIT WINDOW 'Must enter one data at least !' NOWAIT
  RETURN
ENDIF

*--Section to add the data to the filter.
DO CASE
  CASE EMPTY(ldlDate) .AND. !EMPTY(ldhDate)
    XFILTER = ' ORDHDR.Entered <= ldhDate'

  CASE !EMPTY(ldlDate) .AND. EMPTY(ldhDate)
    XFILTER = ' ORDHDR.Entered >= ldlDate'

  CASE !EMPTY(ldlDate) .AND. !EMPTY(ldhDate)     
    XFILTER = ' BETWEEN (ORDHDR.Entered,ldlDate,ldhDate)'
ENDCASE

XFILTER = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp) + " .AND. " + XFILTER

SELECT ORDHDR
LOCATE FOR &XFILTER

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

SELECT ORDHDR
LOCATE
IF !('ORDHDR.ORDER INTO ORDLINE' $ SET('RELATION'))
  SET RELATION TO "O" + ORDHDR.ORDER INTO ORDLINE ADDITIVE
ENDIF

*--PREPARE DATA TO PRINT THE LABELS.
R_WIDTH   = 'N'
lnOldMemW = SET("MEMOWIDTH")
SET CONSOLE OFF

*-- lcTempFile  Variable that hold the temporary file name
lcTempFile = gfTempName()
CREATE TABLE (lcTempFile) (PrnOrdr C(6)  , Lineno N(6)    , Style C(7)   , StyDesc C(15) ,;
                           Date1 C(8)    , ScalSz C(5)    , PcsCnt N(10) , TotQty N(7)   ,;
                           Date2 C(8)    , ColorDes C(15) , BName C(30)  , NotOrd1 C(35) ,;
                           NotOrd2 C(35) , NotOrd3 C(35))
ZAP
INDEX ON PrnOrdr TAG Scale OF (gcWorkDir + lcTempFile + ".CDX")

SELECT ORDHDR
SCAN FOR &XFILTER
  lnOrder = ORDHDR.ORDER
  WAIT WINDOW 'Selecting Records For the Report ...' + lnOrder NOWAIT

  SELECT ORDLINE
  lnRecNo = RECNO()
  SUM TotQty WHILE "O" + ORDER = "O" + lnOrder TO lnTotQty
  GOTO lnRecNo

  SCAN WHILE CORDTYPE + ORDER +STR(LINENO,6) = "O" + lnOrder
    DIMENSION laScale(8,2)
    =SEEK('S'+SCALE,'SCALE')

    laScale(1,1) = SCALE.SZ1
    laScale(2,1) = SCALE.SZ2
    laScale(3,1) = SCALE.SZ3
    laScale(4,1) = SCALE.SZ4
    laScale(5,1) = SCALE.SZ5
    laScale(6,1) = SCALE.SZ6
    laScale(7,1) = SCALE.SZ7
    laScale(8,1) = SCALE.SZ8

    laScale(1,2) = QTY1
    laScale(2,2) = QTY2
    laScale(3,2) = QTY3
    laScale(4,2) = QTY4
    laScale(5,2) = QTY5
    laScale(6,2) = QTY6
    laScale(7,2) = QTY7
    laScale(8,2) = QTY8

    lcName   = IIF(SEEK('M'+ACCOUNT,'CUSTOMER'),CUSTOMER.BTNAME,SPACE(5))
    lcCount  = ALEN(laScale) / 2
    lnPcsCnt = 0

    FOR I = 1 TO lcCount
      FOR X = 1 TO laScale(I,2)
        ldDate1   = DTOC(Ordhdr.Start)
        lcStyle   = SUBSTR(ORDLINE.STYLE,1,7)
        lcStyDesc = IIF(SEEK(Style,'Style'),SUBSTR(STYLE.DESC,1,15),SPACE(15))
        ldDate2   = DTOC(ORDHDR.Complete)
        lcScale   = IIF(SEEK('S'+ STYLE.SCALE,'SCALE'),SCALE.CNT,0)
        lcColor   = SUBSTR(gfCodDes(SUBSTR(ORDLINE.STYLE,lnClrPosLb,lnClrLnLb),'COLOR'),1,15)
        lnOrd     = ORDER
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 29
        lmNote1 = MLINE(NOTE_MEM,1)
        lmNote2 = MLINE(NOTE_MEM,2)
        lmNote3 = MLINE(NOTE_MEM,3)
        lnPcsCnt = lnPcsCnt + 1

        *--Section to add the records to the Temp. file.
        PRIVATE lcAlasPrnt
        lcAlasPrnt = SELECT(0)
        SELECT (lcTempFile)
        APPEND BLANK
        REPLACE PrnOrdr  WITH lnOrd          ,;
                Lineno   WITH lnPcsCnt       ,;
                Style    WITH lcStyle        ,;
                StyDesc  WITH lcStyDesc      ,;
                Date1    WITH ldDate1        ,;
                ScalSz   WITH laScale[I,1]   ,;
                PcsCnt   WITH lnPcsCnt       ,;
                TotQty   WITH ORDLINE.TotQty ,;
                Date2    WITH ldDate2        ,;
                ColorDes WITH lcColor        ,;
                BName    WITH lcName         ,;
                NotOrd1  WITH lmNote1        ,;
                NotOrd2  WITH lmNote2        ,;
                NotOrd3  WITH lmNote3
        SELECT(lcAlasPrnt)

      ENDFOR
    ENDFOR 
  ENDSCAN
ENDSCAN

SELECT (lcTempFile)
LOCATE
lcRpForm = "SOSAR100"

*kk on 04/24/2003
*=gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)
*=lfRepPltFr(lcRpForm)
*SELECT (lcTempFile)
*kk on 04/24/2003

DO gfDispRe WITH (lcRpForm),'',.F.,'L' 
SET DEVICE TO SCREEN
WAIT CLEAR
=lfBasToClr(lcTempFile , 'F')
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfsrOrder
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 01/12/2003
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrOrder
PARAMETERS lcParm

IF lcParm = "S"
  SELECT ORDHDR
  LOCATE
ENDIF  

*--End of lfsrOrder.
*!*************************************************************
*! Name      : lfvDateRng
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 01/12/2003
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : DateRng.Spx
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************

FUNCTION lfvDateRng
PRIVATE ldFrom,ldTo

ldFrom = ldlDate
ldTo   = ldhDate
lcTitle = 'Enter the date range...'

DO DateRng.Spx

ldlDate = ldFrom
ldhDate = ldTo

*--End of lfvDateRng.
*!*************************************************************
*! Name      : lfvpbDateOk
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 01/12/2003
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbDateOk()
*!*************************************************************
FUNCTION lfvPbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*--End of lfvpbDateOk.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 01/12/2003
*! Purpose   : Deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.