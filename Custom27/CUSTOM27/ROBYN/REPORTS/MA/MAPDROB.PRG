*:***************************************************************************
*: Program File        : MAPDROB.PRG
*: Program desc.       : PO Detail Report For ROB.
*: Date                : 02/11/2001
*: System              : ARIA ADVANTAGE SERIES
*: Module              : MATERIALS (MA)
*: Developer           : RANIA ABDEL RAZIK (RAE)			
*: Tracking Job Number : C102527
*:***************************************************************************
*: Calls               : 						
*:    Functions        : 
*:    Procedures       : 
*:***************************************************************************
*: Passed Parameters   : None
*:***************************************************************************
*: Example             : DO MAPDROB
*:***************************************************************************
*  This program prints the PO notepade line starting with '*' If the user:
*      (1) Choose to print form "B" 
*      (2) Choose 'YES' to print the Notes.
*          But if the user choose "NO" the program doesn't print any
*          notepade line
*:***************************************************************************

llMultiWH   = IIF(gfGetMemVar("M_WareHouse",gcAct_Comp)='Y', .T. , .F.)
llApLink    = IIF(gfGetMemVar("M_ApLink",gcAct_Comp)='Y', .T. , .F.)
llIsCanada  = IIF(gcContcode = 'CANADA', .T., .F.)
llIsEngland = IIF(gcContcode = 'ENGLAND', .T., .F.)

DO lpPRINT
  IF USED(lcWorkFile)
    USE IN (lcWorkFile)
  ENDIF
  IF FILE(gcWorkDir+lcWorkFile+'.DBF')
    ERASE (gcWorkDir+lcWorkFile+'.DBF')
  ENDIF
  IF FILE(gcWorkDir+lcWorkFile+'.CDX')  
    ERASE (gcWorkDir+lcWorkFile+'.CDX')
  ENDIF
RETURN

****************************************************************************
* PROG: lpPRINT.PRG
* DESC: OPEN PURCHASE ORDER DETAIL REPORT
****************************************************************************
PROCEDURE lpPRINT

XREPORT  = 'PO Detail Report For ROB'
R_WIDTH  = 'W'    && STANDARD REPORT IS 'WIDE'
R_TITLE  = 'MATERIAL PO DETAIL REPORT'


*WAB
IF  lcRpPrnt = 'S'
  lcRpForm = 'A'
ENDIF
*WAB

lcDyelot = IIF(gfGetMemVar("M_DYELOT",gcAct_Comp)='Y', .T. , .F.)  && (M_DYELOT='Y')

lcFilter = lcRpExp
lcFilter = STRTRAN(lcFilter,'APVENDOR.CVENDCODE','POFHDR.VENDOR')

*-- Divisions case
IF lcRpDiv <> 'A'
  lcFilter = lcFilter + " AND POFHDR.CDIVISION = '&lcRpDiv'"
ENDIF

*-- Report Status cases
DO CASE
  CASE lcRpPrSt = 'O'
    lcFilter = lcFilter + " AND POFLN.TranCd = '1' "
  CASE lcRpPrSt = 'R'
    lcFilter = lcFilter + " AND POFLN.TranCd = '2' "
  CASE lcRpPrSt = 'D'
    lcFilter = lcFilter + " AND POFLN.TranCd = '3' "
  CASE lcRpPrSt = 'C'
    lcFilter = lcFilter + " AND POFLN.TranCd = '4' "    
  CASE lcRpPrSt = 'A'
    lcFilter = lcFilter + " AND POFLN.TranCd $ '1234' "
ENDCASE
*wab
lcFilter = lcFilter + "AND cmattype = 'P'"
*wab
IF lcRpPrnt = 'S'
  lcRpForm = 'A'
ENDIF  

*-- Sort Sequence according to Print case (Detail or Summary)
SORTFB    = ' '
SORTFIELD = ' '
BREAK     = ' '
BREAKD    = ' '
XCANPRT   = .T.      && To know if we're going to print
                     && the CANCEL sub total & grand total or not
                     
DO CASE
  *-- Detail case for Format A or Summary case
  CASE lcRpPrnt = 'D' .AND. lcRpForm = 'A' .OR. lcRpPrnt = 'S'
    DO CASE
      * SUMMARY BY ORDER#
      CASE lcRpASrt = "P"
        SORTFIELD = 'POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
        SORTFB    = 'POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7)'
        BREAK     = 'POMAT'
        BREAKD    = 'POMAT'

      * SUMMARY BY VENDOR ACCOUNT
      CASE lcRpASrt = "V"
        SORTFIELD = 'POFHDR.VENDOR+POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
        SORTFB    = 'POFHDR.VENDOR+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7)'
        BREAK     = 'POFHDR.VENDOR'
        BREAKD    = 'VENDOR'

      * SUMMARY BY ITEM
      CASE lcRpASrt = "I"
        SORTFIELD = 'FABRIC+TRANCD+COLOR+POMAT+STR(RECNO(),7)'
        BREAK     = 'FABRIC'
        BREAKD    = 'FABRIC'
        XCANPRT   = .F.     && To know if we're going to print
                            && the CANCEL sub total & grand total or not
      * SUMMARY BY COLOR
      CASE lcRpASrt = "C"
        SORTFIELD = 'FABRIC+COLOR+TRANCD+POMAT+STR(RECNO(),7)'
        BREAK     = 'FABRIC+COLOR'
        BREAKD    = 'COLOR'
        XCANPRT   = .F.      && To know if we're going to print
                               && the CANCEL sub total & grand total or not
      ENDCASE
  *-- Detail case for Format B
  CASE lcRpPrnt = 'D' .AND. lcRpForm = 'B'
    DO CASE
      * SUMMARY BY ORDER#
      CASE lcRpBSrt = "P"
        SORTFIELD = 'POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
        SORTFB    = 'POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7)'
        BREAK     = 'POMAT'
        BREAKD    = 'POMAT'

      * SUMMARY BY VENDOR ACCOUNT
      CASE lcRpBSrt = "V"
        SORTFIELD = 'POFHDR.VENDOR+POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
        SORTFB    = 'POFHDR.VENDOR+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7)'
        BREAK     = 'POFHDR.VENDOR'
        BREAKD    = 'VENDOR'
    ENDCASE
ENDCASE

*-- Print Notes in case of Format B
IF lcRpNote = 'Y'
  llComNotes = .T.
ELSE
  llComNotes = .F.
ENDIF

*--Print Format B
IF lcRpForm = 'B'
  DO lpFormB
  RETURN
ENDIF
*------------------------------------------------------------------
* SELECT REPORT FILE & INITIALIZE MEMORY   (F O R M A T   'A' )
*------------------------------------------------------------------

DO WHILE lcRpForm='A'
  SELECT POFLN
  SET RELATION TO Cmattype+POMAT INTO POFHDR
  SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
  WAIT WINDOW 'Selecting records for report ...' NOWAIT

  LOCATE ALL FOR &lcFilter
  IF EOF() .OR. cMatType <> 'P'
    *-- 'No Record Selected for the report..!'
    =gfModalGen('TRM00052B00000','DIALOG')
    SET DEVICE TO SCREEN
    RETURN
  ENDIF
  
  IF !USED(lcWorkFile)
    COPY REST TO (gcWorkDir+lcWorkFile) FOR cMatType = 'P' .AND. &lcFilter
  ENDIF
  SELECT POFLN
  SET RELATION TO

  =gfOpenFile(gcWorkDir+lcWorkFile,' ','EX')
  SELECT (lcWorkFile)
  SET RELATION TO cMatType+POMAT INTO POFHDR
  SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE

  IF SORTFIELD<>' '
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW 'Sorting &Z records for material purchase order detail report ..' NOWAI
    INDEX ON &SORTFIELD TAG (lcWorkFile)
    SET ORDER TO TAG (lcWorkFile)
  ENDIF

*!    0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!                    COMPLETE                        
*!    PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     $PRICE        AMOUNT
*!    PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     $PRICE        AMOUNT
*!..  ±±±±±± ±±±±±±±± ±±/±±/±± ± ±±±±±±± ±±±±±± ±±±±±±±±± ±±±±±±±±±± ±±±±±±±±± ±±±±±± ±±±±±±±.±±± ±±± ±±±±±±± ±±± ±±±±±±±.±± ±±±±±±±±±±±±±
*!..  123456 12345678 12345678 1 1234567 123456 123456789 1234567890 123456789 123456 12345678901 123 1234567 123 1234567890 1234567890123
*!    
*!    --------------------------------------------------------- RECEIVED LINES ----------------------------------------------------------
*!..  ±±±±±± ±±±±±±±± ±±/±±/±± ± ±±±±±±± ±±±±±± ±±±±±±±±± ±±±±±±±±±± ±±±±±±±±± ±±±±±± ±±±±±±±.±±± ±±± ±±±±±±± ±±± ±±±±±±±.±± ±±±±±±±±±±±±±
*!..  123456 12345678 12345678 1 1234567 123456 123456789 1234567890 123456789 123456 12345678901 123 1234567 123 1234567890 1234567890123

* *SUB TOTALS*    XXXXXXXXXXXX
*  ORDERED................................                                            99999999.999     99999999          99999999.99
*  RECEIVED...............................                                            99999999.999     99999999          99999999.99
*  DAMAGED................................                                            99999999.999     99999999          99999999.99
*  CANCELD................................                                            99999999.999     99999999          99999999.99      && ARH 10/28/93
*  OPEN...................................                                            99999999.999     99999999          99999999.99
*
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

  DIMENSION XTOTAL(5,3),XTOTAL1(5,3)
  lcClrDesc = ''
  XTOTAL = 0.00
  XTOTAL1= 0.00
  PAGENO = 0
  ROW    = 99
  XTIME  = TIME()
  SELECT (lcWorkFile)
  GOTO TOP

  IF LEN(TRIM(BREAK)) <>0
    HBREAK = &BREAK
  ENDIF

  SET DEVICE TO SCREEN
  WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
  SET DEVICE TO PRINT
  SELECT (lcWorkFile)
  PTRANCD = SPACE(1)

  *-- [REPORT] LOOP

  DO WHILE INKEY() <>32
    IF ROW >=53
      PAGENO = PAGENO+1
      DO RPT_HDR WITH XREPORT,lcRpTitle,R_WIDTH
        @ 05,00 SAY '                COMPLETE'
        IF !lcDyelot
          IF llIsEngland
            @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     œPRICE        AMOUNT'
          ELSE
            @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     $PRICE        AMOUNT'
          ENDIF
        ELSE
          IF llIsEngland 
            @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     œPRICE        AMOUNT'
          ELSE
            @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     $PRICE        AMOUNT'
          ENDIF
        ENDIF
        @ 07,00 SAY REPLICATE('=',132)
        ROW = 08
    ENDIF
    DO WHILE LEN(TRIM(BREAK)) <>0
      IF &BREAK = HBREAK
        EXIT
      ENDIF
      ROW=ROW + 1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW + 1
      @ ROW,000 SAY '* SUB TOTAL * '
      @ ROW,015 SAY HBREAK
      
      IF BREAKD = 'COLOR'
        @ ROW,35 SAY lcClrDesc
      ENDIF
      
      ROW=ROW+1
      IF ('A'$lcRpPrSt) .OR. ('O'$lcRpPrSt)
        @ ROW,000 SAY '  ORDERED................................'
        @ ROW,080 SAY XTOTAL(1,1)  PICTURE '99999999.999'
        @ ROW,96 SAY XTOTAL(1,2)  PICTURE '99999999'
        @ ROW,119 SAY XTOTAL(1,3) PICTURE '9999999999.99'
        ROW = ROW+1
      ENDIF
  
      IF ('A'$lcRpPrSt) .OR. ('R'$lcRpPrSt)
        @ ROW,000 SAY '  RECEIVED...............................'
        @ ROW,080 SAY XTOTAL(2,1)  PICTURE '99999999.999'
        @ ROW,96 SAY XTOTAL(2,2)  PICTURE '99999999'
        @ ROW,119 SAY XTOTAL(2,3) PICTURE '9999999999.99'
        ROW = ROW+1
      ENDIF
         
      IF ('A'$lcRpPrSt) .OR. ('D'$lcRpPrSt)
        @ ROW,000 SAY '  DAMAGED................................'
        @ ROW,080 SAY XTOTAL(3,1)  PICTURE '99999999.999'
        @ ROW,096 SAY XTOTAL(3,2)  PICTURE '99999999'
        @ ROW,119 SAY XTOTAL(3,3) PICTURE '9999999999.99'
        ROW = ROW+1
      ENDIF

      *-- Print the sub total for CANCELLED
      *-- pieces from the array elements if the PO was cancelled
      IF ('A'$lcRpPrSt) .OR. ('C'$lcRpPrSt)) 
        @ ROW,000 SAY '  CANCELD................................'
        @ ROW,080 SAY XTOTAL(4,1)  PICTURE '99999999.999'
        @ ROW,096 SAY XTOTAL(4,2)  PICTURE '99999999'
        @ ROW,119 SAY XTOTAL(4,3)  PICTURE '9999999999.99'
        ROW = ROW+1
      ENDIF

      IF ('A'$lcRpPrSt)
        @ ROW,000 SAY '  OPEN...................................'
        *-- Print the sub total for
        *-- OPEN pieces from the new array elements instead
        *-- of printing it on the fly because there may be
        *-- some cancelled pieces

        @ ROW,080 SAY MAX(XTOTAL(5,1),0)  PICTURE '99999999.999'
        @ ROW,096 SAY MAX(XTOTAL(5,2),0)  PICTURE '99999999'
        @ ROW,119 SAY MAX(XTOTAL(5,3),0) PICTURE '9999999999.99'
        ROW = ROW+1
      ENDIF
        @ ROW,00 SAY REPLICATE('=',132)
        ROW = ROW+1
        X = 1

        DO WHILE X<=3
          XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
          XTOTAL1(2,X) = XTOTAL1(2,X) + XTOTAL(2,X)
          XTOTAL1(3,X) = XTOTAL1(3,X) + XTOTAL(3,X)
     
          *-- Update the grand totals for
          *-- the CANCELLED & OPENED pieces after printing
          *-- the sub totals
          XTOTAL1(4,X) = XTOTAL1(4,X) + MAX(XTOTAL(4,X),0)
          XTOTAL1(5,X) = XTOTAL1(5,X) + MAX(XTOTAL(5,X),0)
          XTOTAL(1,X) = 0.00
          XTOTAL(2,X) = 0.00
          XTOTAL(3,X) = 0.00
      
          *-- Initialize the cancelled
          *-- and open pieces sub total variables
          XTOTAL(4,X) = 0
          XTOTAL(5,X) = 0
          X = X+1
        ENDDO
        HBREAK = &BREAK
        PTRANCD = SPACE(1)
        EXIT
    ENDDO
          *--------------------- END SUBTOTALS ----------------------------
    IF EOF()
      EXIT
    ENDIF
    IF ROW >= 53
      ROW = 99
      LOOP
    ENDIF

    SELECT (lcWorkFile)
    DO WHILE lcRpPrnt='D'
      ROW=ROW+1
      IF (TRANCD='1') .AND. (TRANCD<>PTRANCD)
        PTRANCD=TRANCD
        @ ROW,000 SAY '--------------------------------------------------------- ORDERED LINES ------------------------------------------------------------'
        ROW=ROW+1
      ENDIF
      IF (TRANCD='2') .AND. (TRANCD<>PTRANCD)
        PTRANCD=TRANCD
        @ ROW,000 SAY '--------------------------------------------------------- RECEIVED LINES -----------------------------------------------------------'
        ROW=ROW+1
      ENDIF
      IF (TRANCD='3') .AND. (TRANCD<>PTRANCD)
        PTRANCD=TRANCD
        @ ROW,000 SAY '--------------------------------------------------------- DAMAGED LINES ------------------------------------------------------------'
        ROW=ROW+1
      ENDIF  
      IF (TRANCD = '4' ) .AND. ( TRANCD <> PTRANCD )
        PTRANCD = TRANCD
        @ ROW,000 SAY '--------------------------------------------------------- CANCELED LINES ----------------------------------------------------------'
        ROW=ROW+1
      ENDIF
      @ ROW,000 SAY POMAT
      @ ROW,007 SAY POFHDR.VENDOR

      IF (TRANCD='2') .OR. (TRANCD='3') .OR. (TRANCD='4')
        @ ROW,016 SAY DATE
      ENDIF
      IF TRANCD='1'
        @ ROW,016 SAY POFHDR.COMPLETE
      ENDIF
      @ ROW,025 SAY POFHDR.STATUS
      @ ROW,027 SAY FABRIC
      @ ROW,035 SAY COLOR
      @ ROW,042 SAY SUBSTR(FABRIC.DESC,1,09)
      IF lcDyelot             && IF DYELOT SYSTEM
        @ ROW,052 SAY DYELOT
      ELSE
        @ ROW,052 SAY SUBSTR(REFERENCE,1,10)
      ENDIF
      @ ROW,063 SAY SUBSTR(PATTERN,1,9)
      @ ROW,073 SAY WIDTH
      @ ROW,080 SAY NFabTotQty PICTURE '9999999.999'
      @ ROW,092 SAY FABRIC.UOMBUY
      @ ROW,096 SAY (NFabTotQty*FABRIC.CONV)  PICTURE '9999999'
      @ ROW,104 SAY FABRIC.UOMUSE
      @ ROW,108 SAY ncost1 PICTURE '9999999.99'
      @ ROW,119 SAY (NFabTotQty*ncost1) PICTURE '9999999999.99'
      EXIT
    ENDDO
    lcClrDesc = gfCodDes(&lcWorkFile..Color,PADR('COLOR',10))
    DO CASE
      CASE TRANCD='1'
        XTRNCD=1
      CASE TRANCD='2'
        XTRNCD=2
      CASE TRANCD='3'
        XTRNCD=3
      CASE TRANCD='4'
        XTRNCD=4
    ENDCASE

    XTOTAL(XTRNCD,1)=XTOTAL(XTRNCD,1)+NFabTotQty
    XTOTAL(XTRNCD,2)=XTOTAL(XTRNCD,2)+(NFabTotQty*FABRIC.CONV)
    XTOTAL(XTRNCD,3)=XTOTAL(XTRNCD,3)+(NFabTotQty*(ncost1/POFHDR.NPRICERAT))

    *-- Calculate the open pces.
    xTotal( 5, 1 ) = xTotal( 5, 1 ) + ( IIF( xTrnCd = 1 , 1, -1) * NFabTotQty )
    xTotal( 5, 2 ) = xTotal( 5, 2 ) + ( IIF( xTrnCd = 1 , 1, -1) * ( NFabTotQty * FABRIC.CONV) )
    xTotal( 5, 3 ) = xTotal( 5, 3 ) + ( IIF( xTrnCd = 1 , 1, -1) * ( NFabTotQty * (ncost1/POFHDR.NPRICERAT)))
   
    SELECT (lcWorkFile)
    SKIP
ENDDO 
   *------------------ END MAIN REPORT LOOP --------------------

 X = 2
 IF LEN(TRIM(BREAK)) =0
   X =1
 ENDIF
 ROW=ROW+2
 @ ROW,00 SAY REPLICATE('*',132)
 ROW = ROW+1
 @ ROW,000 SAY '* GRAND TOTAL *'
 ROW=ROW+1
 IF ('A'$lcRpPrSt) .OR. ('O'$lcRpPrSt)
   @ ROW,000 SAY '  ORDERED................................'
   @ ROW,079 SAY XTOTAL1(1,1)  PICTURE '999999999.999'
   @ ROW,094 SAY XTOTAL1(1,2)  PICTURE '999999999'
   @ ROW,117 SAY XTOTAL1(1,3)  PICTURE '999999999999.99'
   ROW = ROW+1
 ENDIF
 IF ('A'$lcRpPrSt) .OR. ('R'$lcRpPrSt)
   @ ROW,000 SAY '  RECEIVED...............................'
   @ ROW,079 SAY XTOTAL1(2,1)  PICTURE '999999999.999'
   @ ROW,094 SAY XTOTAL1(2,2)  PICTURE '999999999'
   @ ROW,117 SAY XTOTAL1(2,3)  PICTURE '999999999999.99'
   ROW = ROW+1
 ENDIF
 IF ('A'$lcRpPrSt) .OR. ('D'$lcRpPrSt)
   @ ROW,000 SAY '  DAMAGED................................'
   @ ROW,079 SAY XTOTAL1(3,1)  PICTURE '999999999.999'
   @ ROW,094 SAY XTOTAL1(3,2)  PICTURE '999999999'
   @ ROW,117 SAY XTOTAL1(3,3)  PICTURE '999999999999.99'
   ROW = ROW+1
 ENDIF

 *-- Print the GRAND TOTAL for the CANCELLED pieces
 IF ('A'$lcRpPrSt) .OR. ('C'$lcRpPrSt)) 
   @ ROW,000 SAY '  CANCELD................................'
   @ ROW,079 SAY XTOTAL1(4,1) PICTURE '999999999.999'
   @ ROW,094 SAY XTOTAL1(4,2)  PICTURE '999999999'
   @ ROW,117 SAY XTOTAL1(4,3)  PICTURE '999999999999.99'
   ROW = ROW+1
 ENDIF
   
 IF ('A'$lcRpPrSt)
   @ ROW,000 SAY '  OPEN...................................'
   *-- Print the GRAND TOTAL for OPEN pieces
   *-- from the new array elements instead of printing it on
   *-- the fly because there may be some cancelled pieces
   @ ROW,079 SAY XTOTAL1(5,1)  PICTURE '999999999.999'
   @ ROW,094 SAY XTOTAL1(5,2)  PICTURE '999999999'
   @ ROW,117 SAY XTOTAL1(5,3)  PICTURE '999999999999.99'
   ROW = ROW+1
 ENDIF
 @ ROW,00 SAY REPLICATE('*',132)
 EXIT
 ENDDO

 DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
 RETURN
****************************************************************************
* PROG: lpFormB.PRG
* DESC: FORMAT B OF OPEN PURCHASE ORDER DETAIL REPORT
****************************************************************************
PROCEDURE lpFormB
Xcancel = 0
*------------------------------------------------------------------
* [3] SELECT REPORT FILE & INITIALIZE MEMORY   (F O R M A T   'B' )
*------------------------------------------------------------------

DO WHILE lcRpForm='B'
  SELECT POFLN
  SET RELATION TO Cmattype+POMAT INTO POFHDR
  SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE

  WAIT WINDOW 'Selecting records for report ...' NOWAIT
  LOCATE ALL FOR &lcFilter
    IF EOF() .OR. cMatType <> 'P'
      *-- 'No Record Selected for the report..!'
      =gfModalGen('TRM00052B00000','DIALOG')
      SET DEVICE TO SCREEN
      RETURN
    ENDIF
    IF !USED(lcWorkFile)
      COPY REST TO (gcWorkDir+lcWorkFile) FOR cMatType = 'P' .AND. &lcFilter
    ENDIF

    SELECT POFLN
    SET RELATION TO
    
    =gfOpenFile(gcWorkDir+lcWorkFile,' ','EX')
    SET RELATION TO Cmattype+POMAT INTO POFHDR
    SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE

    IF SORTFB<>' '
      Z = LTRIM(STR(RECCOUNT(),7))
      WAIT WINDOW 'Sorting &Z records for material purchase order detail report ...' NOWAIT
      INDEX ON &SORTFB TAG (lcWorkFile)
      SET ORDER TO TAG (lcWorkFile)
    ENDIF

*!    0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*!04  P.O.#   VENDOR   COMPLETE       S                                                RECEIVED
*!05  ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     REFERENCE  RECEIVD DAMAGED CANCELD    OPEN
*!..  ±±±±±±  ±±±±±±±± ±±/±±/±±         ±                                                         
*!..  123456  12345678 12345678         1                                                         
*!..  ±±±±±±± ±±±±±± ±±±±±±±±±±±± ±±±±±±±±± ±±±±±± ±±±±±±± ±±± ±±±±±±±.±± ±±±±±±±±.±±± ±±/±±/±± ±±±±±±±±±± ±±±±±±± ±±±±±±± ±±±±±±± ±±±±±±±
*!..  1234567 123456 123456789012 123456789 123456 1234567 123 1234567890 123456789012 12345678 1234567890 1234567 1234567 1234567 1234567
*!
*!    -------------------------------------------------------------------------------------------------------------------------------------

    PAGENO = 0
    ROW    = 99
    XTIME  = TIME()
    IF LEN(TRIM(BREAK)) <>0
       HBREAK = &BREAK
    ENDIF
    CLEAR TYPEAHEAD
    SET DEVICE TO SCREEN
    WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
    SET DEVICE TO PRINT
    SELECT (lcWorkFile)
    GO TOP

    XCONT=.F.       && flag is .T. if continued in a new page
    XPOMAT=SPACE(6)
    STORE 0 TO XTOTORD,XTOTAMT,XTOTREC,XTOTDAM,XTOTOPN,XTOTCAN
    STORE 0 TO XGTOTORD,XGTOTAMT,XGTOTREC,XGTOTDAM,XGTOTOPN,XGTOTCAN

    *---------------------------------------------------------
    * [REPORT] LOOP
    *---------------------------------------------------------

    DO WHILE INKEY() <>32
      IF ROW >=55
        PAGENO = PAGENO+1
        DO RPT_HDR WITH XREPORT,lcRpTitle,R_WIDTH
        @ 05,00 SAY 'P.O.#   VENDOR   COMPLETE       S                                                RECEIVED'

        IF !lcDyelot
          IF llIsEngland 
            @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     œPRICE       AMOUNT DATE     REFERENCE  RECEIVD DAMAGED CANCELD    OPEN'
          ELSE
            @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     REFERENCE  RECEIVD DAMAGED CANCELD    OPEN'
          ENDIF
        ELSE
          IF llIsEngland 
            @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     œPRICE       AMOUNT DATE     DYELOT #   RECEIVD DAMAGED CANCELD    OPEN'
          ELSE
            @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     DYELOT #   RECEIVD DAMAGED CANCELD    OPEN'
          ENDIF
        ENDIF
        @ 07,00 SAY REPLICATE('=',132)
        ROW = 08
      ENDIF

      ******** SUB TOTAL *********
      DO WHILE LEN(TRIM(BREAK)) <>0
        IF lcRpPrnt='D' .AND. lcRpForm='B' .AND. llComNotes
          IF POMAT <> XPOMAT .AND. XPOMAT <> SPACE(6) 
            DO lpPrntNote WITH 'M',IIF(BREAKD = 'POMAT',HBREAK,XPOMAT),llComNotes
          ENDIF
        ENDIF

        IF &BREAK = HBREAK
          EXIT
        ENDIF
        ROW=ROW+1 
        @ ROW,00 SAY REPLICATE('=',132)
        ROW = ROW+1
        @ ROW,000 SAY '* SUB TOTAL * '
        @ ROW,015 SAY HBREAK
        IF XTOTORD<>0
          @ ROW,044 SAY XTOTORD  PICTURE '9999999.999'
        ENDIF
        IF XTOTAMT<>0
          @ ROW,070 SAY XTOTAMT  PICTURE '999999999.999'
        ENDIF
        IF XTOTREC<>0
          @ ROW,101 SAY XTOTREC  PICTURE '9999999'
        ENDIF
          IF XTOTDAM<>0
            @ ROW,109 SAY XTOTDAM  PICTURE '9999999'
          ENDIF
          @ ROW,117 SAY IIF(XTOTCAN=0,XCANCEL,XTOTCAN) PICTURE '9999999'
          @ ROW,125 SAY XTOTOPN PICTURE '9999999'
          ROW = ROW+1
          @ ROW,00 SAY REPLICATE('=',132)
          IF SUBSTR(BREAKD,1,1)='V' 
            ROW = ROW+1
          ENDIF    
          HBREAK = &BREAK
          XGTOTORD=XGTOTORD+XTOTORD
          XGTOTAMT=XGTOTAMT+XTOTAMT
          XGTOTREC=XGTOTREC+XTOTREC
          XGTOTDAM=XGTOTDAM+XTOTDAM
          XGTOTOPN=XGTOTOPN+XTOTOPN
          XGTOTCAN=XGTOTCAN+IIF(XTOTCAN=0,XCANCEL,XTOTCAN)
          STORE 0 TO xCancel, XTOTORD,XTOTAMT,XTOTREC,XTOTDAM,XTOTCAN,XTOTOPN
          EXIT
      ENDDO
      *--------------------- END SUBTOTALS ----------------------------
      IF EOF()
        EXIT
      ENDIF
      IF ROW >=54  && 55
        IF POMAT<>XPOMAT  .OR. XCONT
          ROW=ROW+1 
          XPOMAT=POMAT
          @ ROW,000 SAY POMAT
          @ ROW,008 SAY POFHDR.VENDOR
          @ ROW,017 SAY POFHDR.COMPLETE
          @ ROW,032 SAY POFHDR.STATUS
          ROW=ROW+1
        ENDIF
        ROW = 99
        LOOP
      ENDIF

      IF POMAT<>XPOMAT  .OR. XCONT
        IF XPOMAT<>SPACE(6) .AND. !XCONT
          ROW=ROW+1
        ENDIF  
        XPOMAT=POMAT
        @ ROW,000 SAY POMAT
        @ ROW,008 SAY POFHDR.VENDOR
        @ ROW,017 SAY POFHDR.COMPLETE
        @ ROW,032 SAY POFHDR.STATUS
        ROW=ROW+1
      ENDIF

      IF !XCONT
        XOPEN  = 0
        XCNCLD = 0
      ELSE
        XCONT = .F. 
      ENDIF
      XFABRIC=FABRIC
      XCOLOR=COLOR
      XOPENFLAG=.T.     && Flag to check if it more than one trancd
      SCAN WHILE XPOMAT=POMAT .AND. XFABRIC=FABRIC .AND. XCOLOR=COLOR
        IF ROW >=57
          ROW = 99
          XOPENFLAG=.F.
          XCONT=.T.
          EXIT
        ENDIF
        XCAN_OP = IIF(POFHDR.STATUS='X','XCNCLD','XOPEN')
        IF TRANCD='1'
          @ ROW,000 SAY FABRIC
          @ ROW,008 SAY COLOR
          @ ROW,015 SAY SUBSTR(FABRIC.DESC,1,12)
          @ ROW,028 SAY SUBSTR(PATTERN,1,9)
          @ ROW,038 SAY WIDTH
          @ ROW,045 SAY NFabTotQty PICTURE '9999999'
          @ ROW,053 SAY FABRIC.UOMUSE    &&UOMBYE
          @ ROW,057 SAY ncost1  PICTURE '9999999.99'
          @ ROW,068 SAY (NFabTotQty*ncost1) PICTURE '99999999.999'
          &XCAN_OP = &XCAN_OP + NFabTotQty 

        ELSE   && (TRANCD='2') .OR. (TRANCD='3')

          XOPENFLAG=.F.
          @ ROW,081 SAY DATE
          IF lcDyelot             && IF DYELOT SYSTEM
            @ ROW,090 SAY DYELOT
          ELSE
            @ ROW,090 SAY SUBSTR(REFERENCE,1,10)
          ENDIF
          IF TRANCD='2'
            @ ROW,101 SAY NFabTotQty PICTURE '9999999'
          ENDIF
          IF TRANCD='3'
            @ ROW,109 SAY NFabTotQty PICTURE '9999999'
          ENDIF
          
          IF TranCd = '4'
            @ ROW,117 SAY NFabTotQty PICTURE '9999999'
          ENDIF
          
          &XCAN_OP = MAX((&XCAN_OP - NFabTotQty),0)
          IF XCNCLD <> 0
            @ ROW,117 SAY XCNCLD PICTURE '9999999'
          ENDIF
          @ ROW,125 SAY XOPEN  PICTURE '9999999'
          ROW=ROW+1
        ENDIF
        
        DO CASE
          CASE TRANCD='1'
            XTOTORD=XTOTORD+NFabTotQty
            XTOTAMT=XTOTAMT+(NFabTotQty*ncost1)
          CASE TRANCD='2'
            XTOTREC=XTOTREC+NFabTotQty
          CASE TRANCD='3'
            XTOTDAM=XTOTDAM+NFabTotQty
          CASE TRANCD='4'
            xCancel = xCancel + NFabTotQty
        ENDCASE
        *XCANCEL = POFHDR.CANCEL
        XCAN_OPTOT  = IIF(POFHDR.STATUS='X','XTOTCAN','XTOTOPN')
        &XCAN_OPTOT = IIF(TRANCD='1',(&XCAN_OPTOT+NFabTotQty),MAX((&XCAN_OPTOT-NFabTotQty),0))
      ENDSCAN
      
      IF XOPENFLAG
        *-- Print the cancelled or the open
        *-- value if there is only one line with TRANCD=1 for
        *-- that fabric/color
        IF XCAN_OP = 'XCNCLD'
          @ ROW,117 SAY XCNCLD PICTURE '9999999'
        ELSE
          @ ROW,125 SAY XOPEN PICTURE '9999999'
        ENDIF
        ROW=ROW+1
      ENDIF
   ENDDO
   *------------------ END MAIN REPORT LOOP --------------------

   ROW=ROW+1
   @ ROW,00 SAY REPLICATE('*',132)
   ROW = ROW+1
   @ ROW,000 SAY '* GRAND TOTAL *'
   IF XGTOTORD<>0
     @ ROW,044 SAY XGTOTORD  PICTURE '9999999.999'
   ENDIF
   IF XGTOTAMT<>0
     @ ROW,067 SAY XGTOTAMT  PICTURE '999999999.999'
   ENDIF
   IF XGTOTREC<>0
     @ ROW,101 SAY XGTOTREC  PICTURE '9999999'
   ENDIF
   IF XGTOTDAM<>0
     @ ROW,109 SAY XGTOTDAM  PICTURE '9999999'
   ENDIF
   @ ROW,117 SAY XGTOTCAN PICTURE '9999999'
   @ ROW,125 SAY XGTOTOPN PICTURE '9999999'
   ROW=ROW+1
   @ ROW,00 SAY REPLICATE('*',132)
   EXIT
ENDDO

DO ENDREPORT        && END THE REPORT OR DISPLAY ON SCREEN
RETURN

**************************************************************
* Name : lpPrntNote
* Synopsis : Print the *Notepade.
**************************************************************

PROCEDURE lpPrntNote

PARAMETER XTYPE,XKEY,llComment
PRIVATE lnAlias, lnMemoWdth, lnMemoLnNo, lnMemolins
lnAlias = SELECT()

XKEY = 'P'+XKEY
SELECT NOTEPAD
IF SEEK (XTYPE+XKEY)
  ROW = ROW + 1
  lnMemoWdth = SET('MEMOWIDTH')
  SET MEMOWIDTH TO 75
  lnMemoLnNo = 0      
  lnMemolins = 0
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,lcRpTitle,R_WIDTH
    ROW = 5
  ENDIF
  lnMemolins = MEMLINES(mNotes)
  IF lnMemolins > 0
    @ ROW,00 SAY REPLICATE('|',80 )
    ROW = ROW + 1
    DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
      IF ROW > 55
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH XREPORT,lcRpTitle,R_WIDTH
        ROW = 5
      ENDIF
      lnMemoLnNo = lnMemoLnNo + 1
      IF llComment
        IF SUBSTR(MLINE(mNotes,lnMemoLnNo),1,1) <> ''
          LOOP
        ENDIF
      ENDIF
      
      @ ROW,00 SAY '|'+MLINE(mNotes,lnMemoLnNo)
      ROW = ROW + 1
    ENDDO
    @ ROW,00 SAY REPLICATE('|',80 )
    ROW = ROW + 1
  ENDIF
  SET MEMOWIDTH TO (lnMemoWdth)
ELSE
  ROW = ROW - 1
ENDIF
SELECT (lnAlias)
RETURN

*********************************************************************
FUNCTION lfvVend

lcVenFld = VARREAD()
lcVendor = EVAL(lcVenFld)

SELECT APVENDOR
SET ORDER TO TAG VenCode 

IF !EMPTY(lcVendor) .AND. ('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF

&lcVenFld = lcVendor 

****************************************************************************  
FUNCTION lfsrvPo

PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO Vencode IN APVENDOR
    SELECT POFHDR
    SET ORDER TO TAG POFHDR
    SET RELATION TO VENDOR INTO APVENDOR
    GO TOP IN POFHDR

  CASE lcParm = 'R'  && Reset code
    SELECT POFHDR
    SET RELATION TO 
ENDCASE
****************************************************************************  
FUNCTION lfSrvFab

PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC
    SET ORDER TO TAG cFabric
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
    *llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
****************************************************************************
FUNCTION lfFabSum

PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
****************************************************************************
FUNCTION lfclearrd

CLEAR READ