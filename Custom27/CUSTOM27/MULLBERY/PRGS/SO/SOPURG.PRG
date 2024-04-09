*-----------------------------------------------------------------------------
*: Program file        : SOPURG
*: Program desc.       : Purg Order Lines
*: System              : Aria Advantage Series VER. 2.7
*: Module              : Manufucturing (MF)
*: Developer           : RAMY MABROUK (RAM)
*: Tracking Job Number : C101938
*: Date                : August/2000
*-----------------------------------------------------------------------------
*: Calls :
*:    Procedures : ....
*:    Functions  : gfOpenFile()
*:                 gfModalGen()
*:                 gfOpGrid()
*:                 gfTempName()
*-----------------------------------------------------------------------------
*: Called From         : System Menu (SO --> Transaction-->Purg Order Lines)
*-----------------------------------------------------------------------------
*: Passed Parameters  : None
*-----------------------------------------------------------------------------
*: Example : DO SOPURG
*-----------------------------------------------------------------------------
*: Modification        :
*B603811 Amin 08/10/00 Fix error file does not exist.
*B603833,1 RAMY 08/17/2000 Enhance the program speed
*-----------------------------------------------------------------------------

*lcTmpHdr  = gfTempName()  && Var Holds the Order Header Temp. file name
lcStatus  = ''            && Var holds the order status in the selection grid
lcOrdType = 'A'
llAllo    = .F.
llFound   = .F.
*--Open the used data files
= gfOpenFile(gcDataDir + 'ORDHDR'  , 'ORDHDR'  , 'SH')



DO WHILE .T.

  lcExpr = gfOpGrid('SOPURG',.T.)  && Run selection grid.

  SELECT ORDHDR
  IF TYPE('lcExpr') = 'C'
    lcExpr = IIF(lcStatus = 'A' , lcExpr , lcExpr + ' .AND. STATUS = ' + "'" + lcStatus + "'")
    lcExpr = IIF(lcOrdType = 'A' , lcExpr , lcExpr + ' .AND. cOrdType = ' + "'" + lcOrdType + "'")
    lcExpr = STRTRAN(lcExpr , 'ORDHDR.' , '')
  ELSE
    EXIT
  ENDIF


  WAIT WINDOW 'Opennig Data Fiels..' NOWAIT
  = gfOpenFile(gcDataDir + 'ORDLINE' , 'ORDLINE' , 'SH')
  = gfOpenFile(gcDataDir + 'STYLE'   , 'STYLE'   , 'SH')
  = gfOpenFile(gcDataDir + 'STYDYE'  , 'STYDYE'  , 'SH')
  = gfOpenFile(gcDataDir + 'POSHDR'  , 'POSHDR'  , 'SH')
  = gfOpenFile(gcDataDir + 'POSLN'   , 'POSLN'   , 'SH')
  = gfOpenFile(gcDataDir + 'CUTPICK' , 'CUTORD'  , 'SH')

  *B-603811  Amin [Start]
  * Check whether the following files are installed or not based on the
  * installed modules. (MF)
  PRIVATE lcMfInstal
  PRIVATE lcAlInstal
  llMfInstal = IIF('MF' $ gcCmpModules , .T., .F.)
  llAlInstal = IIF('AL' $ gcCmpModules , .T., .F.)

  IF llMfInstal
    = gfOpenFile(gcDataDir + 'CUTTKTH' , 'CUTTKTH' , 'SH')
    = gfOpenFile(gcDataDir + 'CUTTKTL' , 'CUTLIN'  , 'SH')
  ENDIF

  IF llAlInstal
    = gfOpenFile(gcDataDir + 'PIKTKT'  , 'ORDPIK'  , 'SH')
  ENDIF
  *B-603811  Amin [End]


  SELECT ORDHDR
  llFound   = .F.
  *B603833,1 RAMY Remove this line to use SEEK function insted [start]
  *SET RELATION TO 'O' + ORDER INTO ORDLINE
  *B603833,1 RAMY [end]

  SCAN FOR &lcExpr
    llFound = .T.
    WAIT "Purging Order No. " + ORDER WINDOW NOWAIT
    *B603833,1 RAMY [start]
    IF SEEK(cOrdType + ORDER , 'ORDLINE')
      SELECT ORDLINE
      *B603833,1 RAMY [end]

      *B603833,1 RAMY [start]
      *SCAN FOR ORDER = &lcTmpHdr..Order
      SCAN REST WHILE ORDER = OrdHdr.Order

        *B603833,1 RAMY [END]

        IF SEEK('1' + ORDER + STR(LINENO,6) , 'CUTPICK') .OR. ;
            SEEK('2' + ORDER + STR(LINENO,6) , 'CUTPICK')

          llAllo = .T.
          =lfClrAloc()
          *B-603811  Amin [Start]
          IF llAlInstal
            SELECT ORDLINE
            IF !EMPTY(PIKTKT) .AND. SEEK(ORDER + PIKTKT , 'PIKTKT')
              SELECT PIKTKT
              REPLACE STATUS WITH 'X'
            ENDIF
          ENDIF
          *B-603811  Amin [End]
          SELECT ORDLINE
          DELETE
        ELSE
          llAllo = .F.
          =lfUpdtSty()
          SELECT ORDLINE

          *B-603811  Amin [Start]
          IF llAlInstal
            IF !EMPTY(PIKTKT) .AND. SEEK(ORDER + PIKTKT , 'PIKTKT')
              SELECT PIKTKT
              REPLACE STATUS WITH 'X'
            ENDIF
          ENDIF
          *B-603811  Amin [End]

          SELECT ORDLINE
          DELETE
        ENDIF
      ENDSCAN
    ENDIF

    SELECT OrdHdr
    DELETE
  ENDSCAN
  IF !llFound
    *--Message :  No orders found in the selected criteria
    *--Button  :                 <Ok>
    =gfModalGen('TRM38129B34000' , 'DIALOG' , 'Orders')
  ENDIF
ENDDO

*-----------------------------------------------------------------------------
*! Name      : lfClrAloc
*! Developer : RAMY MABROUK (RAM)
*! Date      : 08/06/2000
*! Purpose   : To clear the order allocation information
*-----------------------------------------------------------------------------
*! Calls       : None.
*-----------------------------------------------------------------------------
*! Passed Parameters : None.
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : =lfClrAloc()
*-----------------------------------------------------------------------------

FUNCTION lfClrAloc

SELECT CUTPICK

SCAN FOR ORDER = ORDLINE.ORDER .AND. ALLTRIM(CUTPICK.CORDLINE) = ALLTRIM(STR(ORDLINE.LINENO))

  IF CUTPICK.TRANCD = '1'
    *B-603811  Amin [Start]
    IF llMfInstal
      *B-603811  Amin [End]
      IF SEEK(CUTPICK.CTKTNO,'CUTTKTH')
        *--Update Cut Ticket
        SELECT CUTTKTL
        IF SEEK (CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.cTktLineNo + '1')
          WAIT WINDOW 'Updating CT# '+ ALLTRIM(CUTPICK.CTKTNO) NOWAIT
          REPLACE ORD1  WITH ORD1   - CUTPICK.QTY1,;
            ORD2  WITH ORD2   - CUTPICK.QTY2,;
            ORD3  WITH ORD3   - CUTPICK.QTY3,;
            ORD4  WITH ORD4   - CUTPICK.QTY4,;
            ORD5  WITH ORD5   - CUTPICK.QTY5,;
            ORD6  WITH ORD6   - CUTPICK.QTY6,;
            ORD7  WITH ORD7   - CUTPICK.QTY7,;
            ORD8  WITH ORD8   - CUTPICK.QTY8,;
            TOTORD WITH TOTORD- CUTPICK.TOTQTY
          =lfUpdtSty()
        ENDIF
      ENDIF
      *B-603811  Amin [Start]
    ENDIF
    *B-603811  Amin [End]

    *--Releas The Allocation
    SELECT ORDHDR
    *=SEEK ('O'+CUTPICK.ORDER)
    REPLACE TOTCUT WITH TOTCUT - CutPick.TotQty
    SELECT ORDLINE
    *--Zero out Cut1,.....TotCut Fields in OrdLine
    REPLACE CUT1   WITH 0,;
      CUT2   WITH 0,;
      CUT3   WITH 0,;
      CUT4   WITH 0,;
      CUT5   WITH 0,;
      CUT6   WITH 0,;
      CUT7   WITH 0,;
      CUT8   WITH 0,;
      TOTCUT WITH 0
    SELECT CUTPICK
    BLANK
    DELETE
  ENDIF

  IF TRANCD = '2'
    IF SEEK('P' + CUTPICK.CTKTNO,'POSHDR') OR SEEK('C' + CUTPICK.CTKTNO,'POSHDR')
      *--Update PO
      WAIT WINDOW 'Updating PO# '+ ALLTRIM(CUTPICK.CTKTNO) NOWAIT
      IF SEEK ('P' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.cTktLineNo + '1' , 'POSLN') .OR. ;
          SEEK ('C' + CUTPICK.CTKTNO + CUTPICK.Style + CUTPICK.cTktLineNo + '1' , 'POSLN')
        SELECT POSLN
        REPLACE ORD1  WITH ORD1   - CUTPICK.QTY1,;
          ORD2  WITH ORD2   - CUTPICK.QTY2,;
          ORD3  WITH ORD3   - CUTPICK.QTY3,;
          ORD4  WITH ORD4   - CUTPICK.QTY4,;
          ORD5  WITH ORD5   - CUTPICK.QTY5,;
          ORD6  WITH ORD6   - CUTPICK.QTY6,;
          ORD7  WITH ORD7   - CUTPICK.QTY7,;
          ORD8  WITH ORD8   - CUTPICK.QTY8,;
          TOTORD WITH TOTORD- CUTPICK.TOTQTY

        =lfUpdtSty()
      ENDIF
    ENDIF

    IF SEEK ('P' + CUTPICK.CTKTNO) OR SEEK('C' + CUTPICK.CTKTNO)
      =RLOCK()
      SELECT POSLN
      REPLACE nICost1 WITH (POSLN.nCost1 * POSHDR.OPEN),;
        nICost2 WITH (POSLN.nCost2 * POSHDR.OPEN),;
        nICost3 WITH (POSLN.nCost3 * POSHDR.OPEN),;
        nICost4 WITH (POSLN.nCost4 * POSHDR.OPEN),;
        nICost5 WITH (POSLN.nCost5 * POSHDR.OPEN)
      UNLOCK
    ENDIF
  ENDIF
  SELECT ORDLINE
  REPLACE CUT1   WITH 0,;
    CUT2   WITH 0,;
    CUT3   WITH 0,;
    CUT4   WITH 0,;
    CUT5   WITH 0,;
    CUT6   WITH 0,;
    CUT7   WITH 0,;
    CUT8   WITH 0,;
    TOTCUT WITH 0
  SELECT CUTPICK
  BLANK
  DELETE
ENDSCAN

*-----------------------------------------------------------------------------
*! Name      : lfUpdtSty
*! Developer : RAMY MABROUK (RAM)
*! Date      : 08/06/2000
*! Purpose   : To clear the order allocation information in the Style and Stydye files
*-----------------------------------------------------------------------------
*! Calls       : None.
*-----------------------------------------------------------------------------
*! Passed Parameters : None.
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : =lfUpdtSty()
*-----------------------------------------------------------------------------

FUNCTION lfUpdtSty


IF ORDHDR.Status $ 'OH'
  IF SEEK(STYLE,'STYLE')
    SELECT STYLE
    WAIT WINDOW 'Updating Order# ' + ORDHDR.Order + ' Style# '+ ALLTRIM(cStyMajor) NOWAIT
    =RLOCK()
    REPLACE ORD1   WITH ORD1   - ORDLINE.QTY1  ,;
      ORD2   WITH ORD2   - ORDLINE.QTY2  ,;
      ORD3   WITH ORD3   - ORDLINE.QTY3  ,;
      ORD4   WITH ORD4   - ORDLINE.QTY4  ,;
      ORD5   WITH ORD5   - ORDLINE.QTY5  ,;
      ORD6   WITH ORD6   - ORDLINE.QTY6  ,;
      ORD7   WITH ORD7   - ORDLINE.QTY7  ,;
      ORD8   WITH ORD8   - ORDLINE.QTY8
    REPLACE TOTORD WITH ORD1 + ORD2 + ORD3 + ORD4 + ORD5 + ORD6 + ORD7 + ORD8


    IF ORDLINE.Picked
      REPLACE ALO1   WITH ALO1   - ORDLINE.PIK1  ,;
        ALO2   WITH ALO2   - ORDLINE.PIK2  ,;
        ALO3   WITH ALO3   - ORDLINE.PIK3  ,;
        ALO4   WITH ALO4   - ORDLINE.PIK4  ,;
        ALO5   WITH ALO5   - ORDLINE.PIK5  ,;
        ALO6   WITH ALO6   - ORDLINE.PIK6  ,;
        ALO7   WITH ALO7   - ORDLINE.PIK7  ,;
        ALO8   WITH ALO8   - ORDLINE.PIK8
      REPLACE TOTALO WITH ALO1 + ALO2 + ALO3 + ALO4 + ALO5 + ALO6 + ALO7 + ALO8
    ENDIF
    UNLOCK
  ENDIF

  IF SEEK(ORDLINE.STYLE + ORDLINE.CWARECODE + ORDLINE.DYELOT , 'STYDYE')
    SELECT STYDYE
    =RLOCK()
    REPLACE ORD1   WITH ORD1   - ORDLINE.QTY1  ,;
      ORD2   WITH ORD2   - ORDLINE.QTY2  ,;
      ORD3   WITH ORD3   - ORDLINE.QTY3  ,;
      ORD4   WITH ORD4   - ORDLINE.QTY4  ,;
      ORD5   WITH ORD5   - ORDLINE.QTY5  ,;
      ORD6   WITH ORD6   - ORDLINE.QTY6  ,;
      ORD7   WITH ORD7   - ORDLINE.QTY7  ,;
      ORD8   WITH ORD8   - ORDLINE.QTY8
    REPLACE TOTORD WITH ORD1 + ORD2 + ORD3 + ORD4 + ORD5 + ORD6 + ORD7 + ORD8
    IF ORDLINE.Picked
      REPLACE ALO1   WITH ALO1   - ORDLINE.PIK1  ,;
        ALO2   WITH ALO2   - ORDLINE.PIK2  ,;
        ALO3   WITH ALO3   - ORDLINE.PIK3  ,;
        ALO4   WITH ALO4   - ORDLINE.PIK4  ,;
        ALO5   WITH ALO5   - ORDLINE.PIK5  ,;
        ALO6   WITH ALO6   - ORDLINE.PIK6  ,;
        ALO7   WITH ALO7   - ORDLINE.PIK7  ,;
        ALO8   WITH ALO8   - ORDLINE.PIK8
      REPLACE TOTALO WITH ALO1 + ALO2 + ALO3 + ALO4 + ALO5 + ALO6 + ALO7 + ALO8
    ENDIF
    UNLOCK
  ENDIF
ENDIF
