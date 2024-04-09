*:*****************************************************************************************
*: PROG         : MaCReqTo.PRG 
*: DATE         : 01/15/2001
*: Developer    : Ahmed Abdel Naby (AAN)
*: DESC         : Open order line item reports customized from ORD920 
*:                For  TAMOTSO (Cust. Prog. # 102124)
*:*****************************************************************************************
*: Calls        : 
*:         Functions  : 
*:         Procedures : 
*:*****************************************************************************************
*: Example      : DO MaCReqTo
*:*****************************************************************************************
*:Modifications :
*:B604429,1 HBG 07/05/2001 The report Should give yields for 1st 2 Materials on the style 
*:*****************************************************************************************
*-- Rework 102124,6 Add varible[Begin]
PRIVATE llNew
llNew = .F.
*-- Rework 102124,6[End]
= (TYPE("lnMajSeg") = "C") AND lfEvalSegs()

LFCR = CHR(10) + CHR(13)

lcSavSort   = ' '

lcTempLine = GFTEMPNAME()

*--CREATE WORK FILE
SELECT ORDLINE
COPY STRUCTURE TO (gcWorkDir+'&lcTempLine')
=gfOpenFile(gcWorkDir+'&lcTempLine','','EX')

*-- Arrays to hold a total values
DIMENSION laTotal(3,10)
DIMENSION laClrTot(10)
DIMENSION laOrdTot(10)

R_WIDTH  = 'W'          && STANDARD REPORT 'WIDE'

*** proceed report ******************************************************

SELECT ORDHDR
SET ORDER TO TAG ORDHDR

SELECT &lcTempLine
SELECT ORDLINE
SET RELATION TO 'O'+ORDER INTO ORDHDR
SET RELATION TO STYLE INTO STYLE ADDITIVE
SCAN FOR &lcRpExp
  WAIT WINDOW "Printing sales order : " + Order NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTempLine) FROM MEMVAR
ENDSCAN
SELECT &lcTempLine
SET RELATION TO 'O'+ORDER INTO ORDHDR
SET RELATION TO STYLE  INTO STYLE ADDITIVE
SELECT &lcTempLine
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF

*------ SET UP RELATIONS --------*
SELECT &lcTempLine
IF LcRpSort='S' .AND. !(LcRpSumm="Y")
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
ELSE
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER  ADDITIVE
ENDIF

LOCATE

*--[BREAK] SET UP SORT FILEDS & CONTROL BREAKS
XSORT = ' '
DO CASE
  *--ACCOUNT NUMBER SEQUENCE
  CASE LcRpsort='A'
    XSORT  = 'ACCOUNT+ORDER+STYLE+STR(LINENO,6)'
    BREAK  = 'CUSTOMER.ACCOUNT + SPACE(3) + CUSTOMER.BTNAME + LFCR + CUSTOMER.PHONE1'
    CTLSEQ = 'ACCOUNT NUMBER'

  *--ORDER NUMBER SEQUENCE
  CASE LcRpsort='O'
    XSORT  = 'ORDER+STYLE+STR(LINENO,6)'
    BREAK  = 'ORDER + SPACE(3) + ACCOUNT' && ACCOUNT IS INCLUDED HERE
                                          && ONLY TO DISPLAY ON SUBTOTAL.
                                          && IT IS NEEDED TO BREAK
    CTLSEQ = 'ORDER'

    *--FABRIC GROUP
    *--Change break and ctlseq
  CASE LcRpsort='F'
    XSORT  = 'STYLE+ORDER+STR(LINENO,6)'
    BREAK = "  "
    CTLSEQ = 'PRIMARY FABRIC/COLOR'
    *--STYLE/COLOR
  CASE LcRpsort = 'S'
    XSORT    = 'STYLE+DTOS(ORDHDR.COMPLETE)+ORDER+STR(LINENO,6)'
    BREAK    = ' '
    CTLSEQ   = 'STYLE/COLOR'

  *--Date
  CASE LcRpsort = 'D'
    XSORT  = 'DTOS(ORDHDR.COMPLETE)+STYLE+ORDER+STR(LINENO,6)'
    BREAK  = 'DTOC(ORDHDR.COMPLETE)'
    CTLSEQ = 'COMPLETION DATE'

  *--Sales Rep.
  CASE LcRpsort = 'R'    &&TAK 06/20/94
    XSORT  = 'ORDHDR.REP1+STYLE+ORDER+STR(LINENO,6)'
    BREAK  = 'ORDHDR.REP1'
    CTLSEQ = 'SALESREP'
ENDCASE

*--SORT TEMPORARY FILE
SELECT &lcTempLine
IF XSORT<>' '
  IF XSORT <> lcSavSort
    lcSavSort = XSORT
    Z = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW "Sorting &Z selected records for report ..." NOWAIT
    INDEX ON &XSORT TAG &lcTempLine
  ENDIF
  SET ORDER TO TAG &lcTempLine
ENDIF
*--PRINT REPORT
SELECT &lcTempLine
LOCATE

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*ORD920                                  ORDER DETAIL REPORT - 123456789012345678901234567890                             PAGE: 1234
*MM/DD/YY                                                                                                                 TIME: HH:SS
*
*1234567 12345678901234567890 1234.99
*
*ACCT  ORDER    START  COMPLETE SP STYLE   CLR    (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8)   PIECES   PRICE PIKTKT DIFF.
*12345 123456 MM/DD/YY MM/DD/YY XX 1234567 123 111111 222222 333333 444444 555555 666666 777777 888888 12345678 1234.99 123456 12345
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

IOXFILE = SELECT()
IF BREAK <> ' '
  HBREAK = &BREAK
ENDIF

CTLSTYLE   = SUBSTR(Style,1,LnMajorlen)
CTLCOLOR   = STYLE
CTLOR      = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)   
XORDER     = ORDER
STYDESC    = STYLE.DESC
STYFAB     = STYLE.FABRIC
XSCALE     = STYLE.SCALE
NEWSTYLE   = .T.
XSUM_STORE = .F.    && Flag to multi order style sort.

PAGENO = 0
ROW    = 99
XTIME  = SUBSTR(TIME(),1,5)
XPRV_KEY=SPACE(1)

*--------------------------------------------------------
*  (1) STYLE
*  (2) USER DEFINED CONTROL BREAK
*  (3) GRAND TOTALS
*--------------------------------------------------------
laTotal  = 0.00   &&-ARRAY(3,10)
laClrTot  = 0.00   &&-ARRAY(10)
laOrdTot  = 0.00   &&-ARRAY(10)

*--[REPORT] BEGIN REPORT LOOP

R_TITLE = " ORDER DETAIL REPORT "

SET DEVICE TO PRINT

**********************************************************************

IF LcRpSort = 'F'
  *wab
  *Dimension laSetups[5,2]
  Dimension laSetups[10,2]
  *wab
  laSetups[1,1]  = 'M_cMType1'
  laSetups[2,1]  = 'M_cMType2'
  laSetups[3,1]  = 'M_cMType3'
  laSetups[4,1]  = 'M_cMType4'
  laSetups[5,1]  = 'M_cMType5'

  *wab
  laSetups[6,1]  = 'M_cIType1'
  laSetups[7,1]  = 'M_cIType2'
  laSetups[8,1]  = 'M_cIType3'
  laSetups[9,1]  = 'M_cIType4'
  laSetups[10,1] = 'M_cIType5'
  *wab

  *-- Use gfGetMemVar once per all variables.
  =gfGetMemVar(@laSetups,gcAct_Comp)
  lnFabtyp = 0
  FOR lnCheck = 1 TO 5
    IF laSetups[lnCheck,2]  = 'F'
      lnFabtyp = lnCheck
    ENDIF
  ENDFOR
  *WAB
  lnPFabTyp = 0
  FOR lnCheck = 6 TO 10
    IF laSetups[lnCheck,2]  = 'F'
      lnPFabtyp = lnCheck - 5 
    ENDIF
  ENDFOR
  *WAB
  FabColor = GFTempName()
  CREATE TABLE &FabColor (Style C(19) , Fabric C(7) , FColor C(6) , TotQty N(7,3))
  INDEX ON Style+Fabric+FColor TAG &FabColor
  DO lpSortFab
  DO ENDREPORT
  RETURN
ENDIF

SELECT &lcTempLine
LOCATE
DO WHILE INKEY() <>32
WAIT WINDOW "Report printing - Account: " + Account +"Order: " + Order +" <SPACE BAR> to abort" NOWAIT
  IF ROW >=54
    PAGENO = PAGENO +1
    DO RPT_HDR WITH 'MaCReqTo','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
    IF LcRpSort = 'S'
      @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
    ELSE
      @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
    ENDIF
    @ 06,000 SAY REPLICATE('-',132)
    ROW = 7
  ENDIF
  SELECT &lcTempLine
  LEVEL = 0
  IF &lcTempLine..STYLE <> CTLSTYLE
    LEVEL = 1
  ENDIF
  DOCLR = IIF(LcRpSort='S' .AND. STYLE<>CTLCOLOR, .T., .F.)
  DOORD = IIF(LcRpSort='A' .AND. ;
  ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)<>CTLOR, .T., .F.)   
  IF LEN(TRIM(BREAK)) <>0
    IF HBREAK <> &BREAK
      LEVEL = 2
    ENDIF
  ENDIF

  *--[DOCLR] STYLE/CLR SUB TOTALS

  IF DOCLR
    IF (LcRpRepFrm = 'S' .AND. LcRpPrnStk = 'N') 
      @ ROW,054 SAY laClrTot(1,1)  PICTURE '@Z 99999'
      @ ROW,060 SAY laClrTot(1,2)  PICTURE '@Z 99999'
      @ ROW,066 SAY laClrTot(1,3)  PICTURE '@Z 99999'
      @ ROW,072 SAY laClrTot(1,4)  PICTURE '@Z 99999'
      @ ROW,078 SAY laClrTot(1,5)  PICTURE '@Z 99999'
      @ ROW,084 SAY laClrTot(1,6)  PICTURE '@Z 99999'
      @ ROW,090 SAY laClrTot(1,7)  PICTURE '@Z 99999'
      @ ROW,096 SAY laClrTot(1,8)  PICTURE '@Z 99999'
      @ ROW,102 SAY laClrTot(1,9)  PICTURE '9999999'
      @ ROW,118 SAY laClrTot(1,10)  PICTURE '$9999999.99'
    ELSE
      ROW = ROW+1
      @ ROW,030 SAY '***'
      @ ROW,034 SAY LEFT(CTLCOLOR,12)
      @ ROW,047 SAY RIGHT(CTLCOLOR,6)
      @ ROW,054 SAY laClrTot(1,1)  PICTURE '@Z 99999'
      @ ROW,060 SAY laClrTot(1,2)  PICTURE '@Z 99999'
      @ ROW,066 SAY laClrTot(1,3)  PICTURE '@Z 99999'
      @ ROW,072 SAY laClrTot(1,4)  PICTURE '@Z 99999'
      @ ROW,078 SAY laClrTot(1,5)  PICTURE '@Z 99999'
      @ ROW,084 SAY laClrTot(1,6)  PICTURE '@Z 99999'
      @ ROW,090 SAY laClrTot(1,7)  PICTURE '@Z 99999'
      @ ROW,096 SAY laClrTot(1,8)  PICTURE '@Z 99999'
      @ ROW,102 SAY laClrTot(1,9)  PICTURE '9999999'
      @ ROW,118 SAY laClrTot(1,10)  PICTURE '$9999999.99'
    ENDIF
    ROW = ROW+2
    CTLCOLOR = STYLE
    laClrTot = 0.00
  ENDIF

  *--[DOORD] ACCOUNT/ORDER SUB TOTALS

  IF DOORD
    @ ROW,000 SAY '***'
    @ ROW,006 SAY LEFT(CTLOR,5)
    @ ROW,012 SAY SUBSTR(CTLOR,6,6)
    @ ROW,019 SAY 'CUST. PO# :'
    @ ROW,031 SAY RIGHT(CTLOR,10)
    @ ROW,054 SAY laOrdTot(1,1)  PICTURE '@Z 99999'
    @ ROW,060 SAY laOrdTot(1,2)  PICTURE '@Z 99999'
    @ ROW,066 SAY laOrdTot(1,3)  PICTURE '@Z 99999'
    @ ROW,072 SAY laOrdTot(1,4)  PICTURE '@Z 99999'
    @ ROW,078 SAY laOrdTot(1,5)  PICTURE '@Z 99999'
    @ ROW,084 SAY laOrdTot(1,6)  PICTURE '@Z 99999'
    @ ROW,090 SAY laOrdTot(1,7)  PICTURE '@Z 99999'
    @ ROW,096 SAY laOrdTot(1,8)  PICTURE '@Z 99999'
    @ ROW,102 SAY laOrdTot(1,9)  PICTURE '9999999'
    @ ROW,118 SAY laOrdTot(1,10)  PICTURE '$9999999.99'
    ROW = ROW+2
    CTLOR = ACCOUNT+ORDER+IIF(ORDHDR.MULTIPO,'*MULTI-PO*',ORDHDR.CUSTPO)  
    laOrdTot = 0.00
    IF llRpOrdNot
      XPRV_ROW=ROW
      XREPORT='MaCReqTo'
      DO PRT_NOTE WITH 'B',XORDER
      ROW=IIF(ROW=XPRV_ROW,ROW,ROW+1)   && INCREMNET ROW ONLY IF NOTEPAD PRINTED
      XORDER=ORDER
    ENDIF
  ENDIF

  *--[LEVEL 1] STYLE SUB TOTALS

  IF LEVEL >= 1
    NEWSTYLE = .F.
    *--PRINT STYLE SUBTOTAL ONLY FOR REPORTS S,F
    IF LcRpSort = 'S'
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+1
      @ ROW,000 SAY SUBSTR(STYDESC,1,12)
      @ ROW,013 SAY '('+STYFAB+')'          
      @ ROW,026 SAY CTLSTYLE
      @ ROW,039 SAY '******'
      @ ROW,054 SAY laTotal(1,1)  PICTURE '@Z 99999'
      @ ROW,060 SAY laTotal(1,2)  PICTURE '@Z 99999'
      @ ROW,066 SAY laTotal(1,3)  PICTURE '@Z 99999'
      @ ROW,072 SAY laTotal(1,4)  PICTURE '@Z 99999'
      @ ROW,078 SAY laTotal(1,5)  PICTURE '@Z 99999'
      @ ROW,084 SAY laTotal(1,6)  PICTURE '@Z 99999'
      @ ROW,090 SAY laTotal(1,7)  PICTURE '@Z 99999'
      @ ROW,096 SAY laTotal(1,8)  PICTURE '@Z 99999'
      @ ROW,102 SAY laTotal(1,9)  PICTURE '9999999'
      @ ROW,118 SAY laTotal(1,10)  PICTURE '$9999999.99'
      ROW = ROW+1
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+2
      CTLSTYLE = STYLE
      STYDESC  = STYLE.DESC
      STYFAB   = STYLE.FABRIC
      XSCALE   = STYLE.SCALE
      NEWSTYLE = .T.
    ENDIF
    X = 1
    DO WHILE X<=10
      laTotal(2,X) = laTotal(2,X) + laTotal(1,X)
      laTotal(3,X) = laTotal(3,X) + laTotal(1,X)
      laTotal(1,X) =0.00
      X = X+1
    ENDDO
  ENDIF
  *------------------ END USER DEFINED SUB TOTALS -------------------
  *--[LEVEL 2] USER DEFINED SUB TOTALS
  IF BREAK<>' ' .AND. LEVEL >=2
    @ ROW,000 SAY REPLICATE('-',132)
    ROW = ROW+1
    IF LcRpSort='O'
      @ ROW,000 SAY SUBSTR(HBREAK,10,5)
      @ ROW,006 SAY SUBSTR(HBREAK,1,6)
    ELSE
      IF LcRpSort='R'  
        SELECT SALESREP
        SEEK HBREAK
        XNAME=NAME
        SELECT &lcTempLine
      ELSE
        XNAME=''
      ENDIF
        @ ROW,000 SAY HBREAK+' '+XNAME
    ENDIF
      IF LFCR $ HBREAK
        ROW = ROW+1
      ENDIF
      @ ROW,054 SAY laTotal(2,1)  PICTURE '@Z 99999'
      @ ROW,060 SAY laTotal(2,2)  PICTURE '@Z 99999'
      @ ROW,066 SAY laTotal(2,3)  PICTURE '@Z 99999'
      @ ROW,072 SAY laTotal(2,4)  PICTURE '@Z 99999'
      @ ROW,078 SAY laTotal(2,5)  PICTURE '@Z 99999'
      @ ROW,084 SAY laTotal(2,6)  PICTURE '@Z 99999'
      @ ROW,090 SAY laTotal(2,7)  PICTURE '@Z 99999'
      @ ROW,096 SAY laTotal(2,8)  PICTURE '@Z 99999'
      @ ROW,102 SAY laTotal(2,9)  PICTURE '9999999'
      @ ROW,118 SAY laTotal(2,10)  PICTURE '$9999999.99'
      ROW = ROW+1
      @ ROW,000 SAY REPLICATE('-',132)
      ROW = ROW+2
      HBREAK = &BREAK
      X = 1
      DO WHILE X<=10
        laTotal(2,X) =0.00
        X = X+1
      ENDDO
    ENDIF
    *------------------ END USER DEFINED SUB TOTALS -------------------
    IF EOF(IOXFILE)
      EXIT
    ENDIF
    *------------------------------------------------
    * PROCESS DETAIL RECORD
    *------------------------------------------------
    IF NEWSTYLE
      IF ROW >=54
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'MaCReqTo','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
        IF LcRpSort = 'S'
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
        ELSE
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
        ENDIF
        @ 06,000 SAY REPLICATE('-',132)
        ROW = 7
      ENDIF
      NEWSTYLE = .F.
      SELECT SCALE
      SEEK 'S'+XSCALE
      *--start a new page to print size descriptions in top of page.
      IF ROW >=53
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'MaCReqTo','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
        IF LcRpSort = 'S'
           @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
        ELSE
          @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
        ENDIF
        @ 06,000 SAY REPLICATE('-',132)
        ROW = 7
      ENDIF
      @ ROW,054 SAY SZ1
      @ ROW,060 SAY SZ2
      @ ROW,066 SAY SZ3
      @ ROW,072 SAY SZ4
      @ ROW,078 SAY SZ5
      @ ROW,084 SAY SZ6
      @ ROW,090 SAY SZ7
      @ ROW,096 SAY SZ8
      ROW=ROW+1
    ENDIF
    SELE &lcTempLine
    IF LcRpSort='S' .AND. XPRV_KEY <> STYLE
      XPRV_KEY=STYLE
      SELE STYLE
      SEEK XPRV_KEY
      IF FOUND()
        *--start a new page to get style and its quantities in one page.
        IF ROW >=54
          PAGENO = PAGENO +1
          DO RPT_HDR WITH 'MaCReqTo','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
          IF LcRpSort = 'S'
            @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER              (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
          ELSE
            @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   PRICE PIKTKT DIFF.'
          ENDIF
          @ 06,000 SAY REPLICATE('-',132)
          ROW = 7
        ENDIF

        SELECT STYLE
        @ ROW,000 SAY STYLE
		*--Print color desc.
        @ ROW,020 SAY lfGetClrD()
        DO CASE
          CASE LcRpPrnStk = 'S'           
            @ ROW,032 SAY 'STOCK :'
            @ ROW,054 SAY STK1  PICTURE '@Z 99999'
            @ ROW,060 SAY STK2  PICTURE '@Z 99999'
            @ ROW,066 SAY STK3  PICTURE '@Z 99999'
            @ ROW,072 SAY STK4  PICTURE '@Z 99999'
            @ ROW,078 SAY STK5  PICTURE '@Z 99999'
            @ ROW,084 SAY STK6  PICTURE '@Z 99999'
            @ ROW,090 SAY STK7  PICTURE '@Z 99999'
            @ ROW,096 SAY STK8  PICTURE '@Z 99999'
            @ ROW,102 SAY TOTSTK  PICTURE '9999999'
          CASE LcRpPrnStk = 'W'           
            @ ROW,032 SAY 'WIP + STOCK :'
            @ ROW,054 SAY WIP1+STK1  PICTURE '@Z 999999'
            @ ROW,060 SAY WIP2+STK2  PICTURE '@Z 999999'
            @ ROW,066 SAY WIP3+STK3  PICTURE '@Z 999999'
            @ ROW,072 SAY WIP4+STK4  PICTURE '@Z 999999'
            @ ROW,078 SAY WIP5+STK5  PICTURE '@Z 999999'
            @ ROW,084 SAY WIP6+STK6  PICTURE '@Z 999999'
            @ ROW,090 SAY WIP7+STK7  PICTURE '@Z 999999'
            @ ROW,096 SAY WIP8+STK8  PICTURE '@Z 999999'
            @ ROW,102 SAY TOTWIP+TOTSTK  PICTURE '9999999'
          CASE LcRpPrnStk = 'I'           
            @ ROW,032 SAY 'WIP :'
            @ ROW,054 SAY WIP1  PICTURE '@Z 99999'
            @ ROW,060 SAY WIP2  PICTURE '@Z 99999'
            @ ROW,066 SAY WIP3  PICTURE '@Z 99999'
            @ ROW,072 SAY WIP4  PICTURE '@Z 99999'
            @ ROW,078 SAY WIP5  PICTURE '@Z 99999'
            @ ROW,084 SAY WIP6  PICTURE '@Z 99999'
            @ ROW,090 SAY WIP7  PICTURE '@Z 99999'
            @ ROW,096 SAY WIP8  PICTURE '@Z 99999'
            @ ROW,102 SAY TOTWIP  PICTURE '9999999'
        ENDCASE
        IF .NOT. (LcRpRepFrm = 'S' .AND. LcRpPrnStk='N')
          ROW = ROW+1
        ENDIF
      ENDIF
      SELE &lcTempLine
    ENDIF
    *--PRINT THE LINE ITEM DETAIL 
    SELECT &lcTempLine
    IF LcRpRepFrm = 'D'
      @ ROW,000 SAY ACCOUNT
      @ ROW,006 SAY ORDER
      @ ROW,013 SAY ORDHDR.START
      @ ROW,022 SAY ORDHDR.COMPLETE
      @ ROW,031 SAY ORDHDR.STATUS
      @ ROW,032 SAY ORDHDR.PRIORITY
      IF LcRpSort = 'S'
        @ ROW,034 SAY SUBSTR(CUSTOMER.STNAME,1,10)
      ELSE
        @ ROW,034 SAY STYLE
      ENDIF
      *--Check condition of printing
      XSUM_STORE=IIF(LcRpSumm="Y" .AND. ORDHDR.MULTI='Y',.T.,.F.)
      IF XSUM_STORE
         STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,;
                    XTOTQTY,XAMT
         XSTYLE=STYLE
         XCOMDT=DTOS(ORDHDR.COMPLETE)
         XORDER=ORDER
         SCAN WHILE STYLE+DTOS(ORDHDR.COMPLETE)+ORDER = XSTYLE+;
                    XCOMDT+XORDER
           FOR XMCOUNT = 1 TO 8
             Z=STR(XMCOUNT,1)
             XQTY&Z=XQTY&Z+QTY&Z
           ENDFOR
           XTOTQTY=XTOTQTY+TOTQTY
           XAMT   =XAMT   +(TOTQTY*PRICE)
         ENDSCAN
         XTOTQTY=XQTY1+XQTY2+XQTY3+XQTY4+XQTY5+XQTY6+XQTY7+XQTY8
         @ ROW,054 SAY XQTY1    PICTURE '@Z 99999'
         @ ROW,060 SAY XQTY2    PICTURE '@Z 99999'
         @ ROW,066 SAY XQTY3    PICTURE '@Z 99999'
         @ ROW,072 SAY XQTY4    PICTURE '@Z 99999'
         @ ROW,078 SAY XQTY5    PICTURE '@Z 99999'
         @ ROW,084 SAY XQTY6    PICTURE '@Z 99999'
         @ ROW,090 SAY XQTY7    PICTURE '@Z 99999'
         @ ROW,096 SAY XQTY8    PICTURE '@Z 99999'
         @ ROW,102 SAY XTOTQTY  PICTURE '9999999'
       ELSE
         @ ROW,054 SAY QTY1    PICTURE '@Z 99999'
         @ ROW,060 SAY QTY2    PICTURE '@Z 99999'
         @ ROW,066 SAY QTY3    PICTURE '@Z 99999'
         @ ROW,072 SAY QTY4    PICTURE '@Z 99999'
         @ ROW,078 SAY QTY5    PICTURE '@Z 99999'
         @ ROW,084 SAY QTY6    PICTURE '@Z 99999'
         @ ROW,090 SAY QTY7    PICTURE '@Z 99999'
         @ ROW,096 SAY QTY8    PICTURE '@Z 99999'
         @ ROW,102 SAY TOTQTY  PICTURE '9999999'
         @ ROW,111 SAY PRICE   PICTURE '9999.99'
         @ ROW,119 SAY PIKTKT
         IF .NOT. EMPTY(PIKTKT) .AND. ((TOTQTY - TOTPIK)>0)
           @ ROW,126 SAY TOTQTY - TOTPIK PICTURE '99999'
         ENDIF
      ENDIF
      ROW = ROW+1
    ENDIF
    SELECT &lcTempLine
    IF LlRpOrdNot .AND. LcRpRepFrm = 'D' .AND. !(LcRpSumm="Y")
      IF !EMPTY(NOTE_MEM)
        ROW = ROW + 1
        @ ROW , 00 SAY 'ORDER NOTES  :'
        FOR X = 1 TO MEMLINES(NOTE_MEM)
          @ ROW,15 SAY MLINE(NOTE_MEM,X)
          ROW = ROW + 1
        ENDFOR
      ENDIF
    ENDIF
    ********************* END PRINT THE LINE DETAIL **********************
    *--Sum qty under condition XSUM_STORE
    laTotal(1,1) = laTotal(1,1) +  IIF(XSUM_STORE,XQTY1,QTY1)
    laTotal(1,2) = laTotal(1,2) +  IIF(XSUM_STORE,XQTY2,QTY2)
    laTotal(1,3) = laTotal(1,3) +  IIF(XSUM_STORE,XQTY3,QTY3)
    laTotal(1,4) = laTotal(1,4) +  IIF(XSUM_STORE,XQTY4,QTY4)
    laTotal(1,5) = laTotal(1,5) +  IIF(XSUM_STORE,XQTY5,QTY5)
    laTotal(1,6) = laTotal(1,6) +  IIF(XSUM_STORE,XQTY6,QTY6)
    laTotal(1,7) = laTotal(1,7) +  IIF(XSUM_STORE,XQTY7,QTY7)
    laTotal(1,8) = laTotal(1,8) +  IIF(XSUM_STORE,XQTY8,QTY8)
    laTotal(1,9) = laTotal(1,9) +  IIF(XSUM_STORE,XTOTQTY,TOTQTY)
    laTotal(1,10) = laTotal(1,10) + IIF(XSUM_STORE,XAMT,TOTQTY*PRICE)
    IF LcRpSort = 'S'
       laClrTot(1) = laClrTot(1) +  IIF(XSUM_STORE,XQTY1,QTY1)
       laClrTot(2) = laClrTot(2) +  IIF(XSUM_STORE,XQTY2,QTY2)
       laClrTot(3) = laClrTot(3) +  IIF(XSUM_STORE,XQTY3,QTY3)
       laClrTot(4) = laClrTot(4) +  IIF(XSUM_STORE,XQTY4,QTY4)
       laClrTot(5) = laClrTot(5) +  IIF(XSUM_STORE,XQTY5,QTY5)
       laClrTot(6) = laClrTot(6) +  IIF(XSUM_STORE,XQTY6,QTY6)
       laClrTot(7) = laClrTot(7) +  IIF(XSUM_STORE,XQTY7,QTY7)
       laClrTot(8) = laClrTot(8) +  IIF(XSUM_STORE,XQTY8,QTY8)
       laClrTot(9) = laClrTot(9) +  IIF(XSUM_STORE,XTOTQTY,TOTQTY)
       laClrTot(10) = laClrTot(10) + IIF(XSUM_STORE,XAMT,TOTQTY*PRICE)
    ENDIF
    IF LcRpSort = 'A'
       laOrdTot(1) = laOrdTot(1) +  QTY1
       laOrdTot(2) = laOrdTot(2) +  QTY2
       laOrdTot(3) = laOrdTot(3) +  QTY3
       laOrdTot(4) = laOrdTot(4) +  QTY4
       laOrdTot(5) = laOrdTot(5) +  QTY5
       laOrdTot(6) = laOrdTot(6) +  QTY6
       laOrdTot(7) = laOrdTot(7) +  QTY7
       laOrdTot(8) = laOrdTot(8) +  QTY8
       laOrdTot(9) = laOrdTot(9) +  TOTQTY
       laOrdTot(10) = laOrdTot(10) +  TOTQTY * PRICE
     ENDIF
    SELE &lcTempLine
    IF .NOT. XSUM_STORE
        SKIP
    ENDIF
  ENDDO
  *---------------------- END OF REPORT -------------------------
  IF  LcRpSort <> 'F'
    SET DEVICE TO PRINT
    *--[LEVEL 3] GRAND TOTALS
    IF ROW >=60
      ROW = 5
    ENDIF
    @ ROW,000 SAY REPLICATE('=',132)
    ROW = ROW+1
    @ ROW,010 SAY 'GRAND TOTALS'
    @ ROW,054 SAY laTotal(3,1)  PICTURE '@Z 99999'
    @ ROW,060 SAY laTotal(3,2)  PICTURE '@Z 99999'
    @ ROW,066 SAY laTotal(3,3)  PICTURE '@Z 99999'
    @ ROW,072 SAY laTotal(3,4)  PICTURE '@Z 99999'
    @ ROW,078 SAY laTotal(3,5)  PICTURE '@Z 99999'
    @ ROW,084 SAY laTotal(3,6)  PICTURE '@Z 99999'
    @ ROW,090 SAY laTotal(3,7)  PICTURE '@Z 99999'
    @ ROW,096 SAY laTotal(3,8)  PICTURE '@Z 99999'
    @ ROW,102 SAY laTotal(3,9)  PICTURE '9999999'
    @ ROW,118 SAY laTotal(3,10)  PICTURE '$9999999.99'
    ROW = ROW+1
    @ ROW,000 SAY REPLICATE('=',132)
    S_CLOSEALL = .F.  && Not to close the opened files because it's possible
                      &&  to reprint the report again
  ENDIF                         
  DO EndReport
  RETURN
*--END OF MaCReqTo.PRG

*!***********************************************************************
*! PROC          : lpSortFab
*: DATE          : 01/15/2001
*: Developer     : Ahmed Abdel Naby (AAN)
*! DESC          : Open order line item reports for sort by fabric only
*!***********************************************************************
*! Calls         : 
*!         Functions  : gfopenfile
*!         Procedures : Lpfabsub
*!                    : Lpstysub
*!                    : RPT_HDR
*!***********************************************************************
*! Example       : DO lpSortFab
*!***********************************************************************
PROCEDURE lpSortFab
*--We put data in  the  temporary file &FabColor
*B604429,1 HBG 07/05/2001 The report Should give yields for 1st 2 Materials on the style not only Fabrics[Begin]
*lcSekBom = "CItmMajor = SUBSTR(lcStyle,1,LnMajorlen) .AND. (SUBSTR(cItmMask,lnNonMajSt,Lncolorlen) = lcStyClr .OR. SUBSTR(cItmMask,lnNonMajSt,Lncolorlen) = '******') .AND. TYP=ALLTRIM(STR(lnFabtyp))" && Seek expr. in bom file
lcSekBom = "CItmMajor = SUBSTR(lcStyle,1,LnMajorlen) .AND. (SUBSTR(cItmMask,lnNonMajSt,Lncolorlen) = lcStyClr .OR. SUBSTR(cItmMask,lnNonMajSt,Lncolorlen) = '******')" && Seek expr. in bom file
*B604429,1 [End]
SELECT &lcTempLine
LOCATE
SCAN
  lcStyle  = SUBSTR(Style,1,LnMajorlen)           && Style code
  lcStyle1  = Style
  lcStyClr = SUBSTR(Style,lnNonMajSt,Lncolorlen)  && Style color
  lcFabric = Style.Fabric    && Fabric code 
  lcFabClr = SPACE(6)        && Fabric color 
  *wab
  lcSekBom = "CItmMajor = SUBSTR(lcStyle,1,LnMajorlen) .AND. (SUBSTR(cItmMask,lnNonMajSt,Lncolorlen) = lcStyClr .OR. SUBSTR(cItmMask,lnNonMajSt,Lncolorlen) = '******')" && Seek expr. in bom file
  lnFabric = IIF(STYLE.MAKE,lnFabtyp,lnPFabTyp)
  lcSekBom = lcSekBom + " .AND. TYP=ALLTRIM(STR(lnFabric)) "
  *wab
  SELECT BOM
  LOCATE
  *B604429,1 HBG 07/05/2001 to get the yields for only 1st 2 Materials [Begin]
  lnMatNo = 1
  *B604429,1 [End]
  SCAN FOR &lcSekBom
    DO CASE
      CASE SUBSTR(cItmMask,lnNonMajSt,Lncolorlen)  = '******' .AND. IClr = '******'
        lcFabClr = lcStyClr
      CASE SUBSTR(cItmMask,lnNonMajSt,Lncolorlen)  = '******' .AND. IClr <> '******'
        lcFabClr = IClr
      CASE SUBSTR(cItmMask,lnNonMajSt,Lncolorlen) <> '******' .AND. IClr <> '******'
        IF SUBSTR(cItmMask,lnNonMajSt,Lncolorlen) = lcStyClr
          lcFabClr = IClr
        ELSE
          LOOP
        ENDIF
    ENDCASE
    lnTotQty = &lcTempLine..TotQty        && The quantity of this fabric in this style

    SELECT &FabColor
    
    *B604429,1 HBG 07/05/2001 IF this style not found add new line to it , if 
    *B604429,1                found Collect the yeilds of 1st 2 Materials [Begin]
    *IF !SEEK(lcStyle1+lcFabric+lcFabClr)
    IF !SEEK(lcStyle1)
    *B604429,1 [End]
    
      APPEND BLANK
      REPLACE  Style   WITH lcStyle1       
    *B604429,1 HBG 07/05/2001 End if this style not found add new line to it , if 
    *B604429,1                found Collect the yeilds of 1st 2 Materials [Begin]
    *REPLACE  Style   WITH lcStyle1,;
             Fabric  WITH lcFabric,;
             FColor  WITH lcFabClr,;
             TotQty  WITH lnTotQty
    ENDIF  
    REPLACE TotQty  WITH TotQty  + BOM.nbomtotqty
    *ENDIF  
    *B604429,1 [End]
    SELECT BOM
    
    *B604429,1 HBG 07/05/2001 If reach the 2nd material exit sacn [Begin]
    IF lnMatNo = 2
      EXIT
    ENDIF
    lnMatNo = lnMatNo + 1
    *EXIT
    *B604429,1 [End]
  ENDSCAN
  SELECT &lcTempLine
ENDSCAN    
SELECT &FabColor
SET ORDER TO
DELETE TAG &FabColor
INDEX ON Fabric+FColor+Style TAG &FabColor
SET RELATION TO STYLE INTO &lcTempLine
SELECT &FabColor
LOCATE
lnYield1 = 0         && The yield of the fabric for the style
lnReq1   = 0         && The required yield of the fabric for the order line
lcFabric = Fabric
lcFabClr = FColor
llStySub = .F.
lcTmpSty = lcStyle
*Intialize var. to hold the style # to control the style break
lcStyle  = Style

DIMENSION laTotal (3,11)     
laTotal = 0
lcPrint = 'F'
SCAN WHILE INKEY() <>32
  * Added to print subtotal if the style changed.
  IF lcStyle <> Style
    DO lpStySub
  ENDIF

  IF Fabric+FColor <> lcFabric+lcFabClr
    
    DO lpFabSub
    @ ROW,000 SAY REPLICATE(' ',132)
    lcFabric = Fabric
    lcFabClr = FColor
  ENDIF
  lcStyle  = Style
  lnYield  = TotQty
  LEVEL    = 0
  * Refresh the relationship.
  GO RECNO()

  * Added to point to the correct record in the 
  * Child file because the relation doesn't work with some styles!!!
  =SEEK(STYLE,lcTempLine)

  SELECT &lcTempLine
  lcTmpSty = Style              && Style code
  SCAN WHILE INKEY() <>32 .AND. Style = lcStyle

    IF ROW >=54
      PAGENO = PAGENO +1
      DO RPT_HDR WITH 'MaCReqTo','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
      SET DEVICE TO PRINT
      @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   YIELD    REQUIRED '
      @ 06,000 SAY REPLICATE('-',132)
      SET DEVICE TO PRINT
      ROW = 7
    ENDIF
    SET DEVICE TO PRINT
    
    LEVEL = 0
    IF &lcTempLine..STYLE <> lcTmpSty
      LEVEL = 1
    ENDIF
    
    * [LEVEL 1] STYLE SUB TOTALS
    SET DEVICE TO PRINT
    IF LEVEL >=1
      DO lpStySub
    ENDIF
    *------------------ END USER DEFINED SUB TOTALS -------------------
    * PROCESS DETAIL RECORD

    IF NEWSTYLE
      IF ROW >=54
        PAGENO = PAGENO +1
        DO RPT_HDR WITH 'MaCReqTo','(SUB TOTALS BY: ' + CTLSEQ +') '+ lcRpMsg1,R_WIDTH
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE        COLOR    (1)   (2)   (3)   (4)   (5)   (6)   (7)   (8)   PIECES   YIELD    REQUIRED '
        @ 06,000 SAY REPLICATE('-',132)
        ROW = 7
      ELSE
        ROW = ROW + 1
      ENDIF
      NEWSTYLE = .F.
      SELECT SCALE
      SEEK 'S'+Style.Scale
      @ ROW,054 SAY SZ1
      @ ROW,060 SAY SZ2
      @ ROW,066 SAY SZ3
      @ ROW,072 SAY SZ4
      @ ROW,078 SAY SZ5
      @ ROW,084 SAY SZ6
      @ ROW,090 SAY SZ7
      @ ROW,096 SAY SZ8
      ROW=ROW+1
*-- Rework 102124,6 If current style is not a new style[Begin]
    ELSE
      IF llNew = .T.
        *ROW = ROW + 1
        llNew = .F.
      ENDIF
*-- Rework 102124,6[End]
    ENDIF

    ****************** PRINT THE LINE ITEM DETAIL *********************** 
    SELECT &lcTempLine
    IF LcRpRepFrm = 'D'
      @ ROW,000 SAY ACCOUNT
      @ ROW,006 SAY ORDER
      @ ROW,013 SAY ORDHDR.START
      @ ROW,022 SAY ORDHDR.COMPLETE
      @ ROW,031 SAY ORDHDR.STATUS
      @ ROW,032 SAY ORDHDR.PRIORITY
      @ ROW,034 SAY STYLE
    ENDIF
    XSUM_STORE=IIF(LcRpSumm="Y" .AND. ORDHDR.MULTI='Y',.T.,.F.)
    IF XSUM_STORE
      STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,;
                 XTOTQTY,XAMT
      XSTYLE=STYLE
      XCOMDT=DTOS(ORDHDR.COMPLETE)
      XORDER=ORDER
      SCAN WHILE STYLE+DTOS(ORDHDR.COMPLETE)+ORDER = XSTYLE+;
                 XCOLOR+XCOMDT+XORDER
        FOR XMCOUNT = 1 TO 8
          Z=STR(XMCOUNT,1)
          XQTY&Z=XQTY&Z+QTY&Z
        ENDFOR
        XTOTQTY=XTOTQTY+TOTQTY
        lnYield1 = lnYield1 + lnYield
        lnReq1   = lnReq1   + (TOTQTY * lnYield)
      ENDSCAN
      XTOTQTY=XQTY1+XQTY2+XQTY3+XQTY4+XQTY5+XQTY6+XQTY7+XQTY8
      IF LcRpRepFrm = 'D'
        @ ROW,054 SAY XQTY1    PICTURE '@Z 99999'
        @ ROW,060 SAY XQTY2    PICTURE '@Z 99999'
        @ ROW,066 SAY XQTY3    PICTURE '@Z 99999'
        @ ROW,072 SAY XQTY4    PICTURE '@Z 99999'
        @ ROW,078 SAY XQTY5    PICTURE '@Z 99999'
        @ ROW,084 SAY XQTY6    PICTURE '@Z 99999'
        @ ROW,090 SAY XQTY7    PICTURE '@Z 99999'
        @ ROW,096 SAY XQTY8    PICTURE '@Z 99999'
        @ ROW,102 SAY XTOTQTY  PICTURE '9999999'
        @ ROW,111 SAY lnYield1 PICTURE '9999.99'
        @ ROW,119 SAY lnReq1   PICTURE '@Z 99999999.99'
      ENDIF
    ELSE

      IF LcRpRepFrm = 'D'
        @ ROW,054 SAY QTY1    PICTURE '@Z 99999'
        @ ROW,060 SAY QTY2    PICTURE '@Z 99999'
        @ ROW,066 SAY QTY3    PICTURE '@Z 99999'
        @ ROW,072 SAY QTY4    PICTURE '@Z 99999'
        @ ROW,078 SAY QTY5    PICTURE '@Z 99999'
        @ ROW,084 SAY QTY6    PICTURE '@Z 99999'
        @ ROW,090 SAY QTY7    PICTURE '@Z 99999'
        @ ROW,096 SAY QTY8    PICTURE '@Z 99999'
        @ ROW,102 SAY TOTQTY  PICTURE '9999999'
        @ ROW,111 SAY lnYield PICTURE '9999.99'
        @ ROW,119 SAY (TOTQTY * lnYield) PICTURE '@Z 99999999.99'
      ENDIF
    ENDIF
    IF LcRpRepFrm = 'D'
      ROW = ROW+1
    ENDIF

    SELECT &lcTempLine

    IF LlRpOrdNot .AND. LcRpRepFrm = 'D' .AND. !(LcRpSumm="Y")
      IF !EMPTY(NOTE_MEM)
        ROW = ROW + 1
        @ ROW , 00 SAY 'ORDER NOTES  :'
        FOR X = 1 TO MEMLINES(NOTE_MEM)
          @ ROW,15 SAY MLINE(NOTE_MEM,X)
          ROW = ROW + 1
        ENDFOR
      ENDIF
    ENDIF
    laTotal(1,1) = laTotal(1,1) +  IIF(XSUM_STORE,XQTY1,QTY1)
    laTotal(1,2) = laTotal(1,2) +  IIF(XSUM_STORE,XQTY2,QTY2)
    laTotal(1,3) = laTotal(1,3) +  IIF(XSUM_STORE,XQTY3,QTY3)
    laTotal(1,4) = laTotal(1,4) +  IIF(XSUM_STORE,XQTY4,QTY4)
    laTotal(1,5) = laTotal(1,5) +  IIF(XSUM_STORE,XQTY5,QTY5)
    laTotal(1,6) = laTotal(1,6) +  IIF(XSUM_STORE,XQTY6,QTY6)
    laTotal(1,7) = laTotal(1,7) +  IIF(XSUM_STORE,XQTY7,QTY7)
    laTotal(1,8) = laTotal(1,8) +  IIF(XSUM_STORE,XQTY8,QTY8)
    laTotal(1,9) = laTotal(1,9) +  IIF(XSUM_STORE,XTOTQTY,TOTQTY)
    laTotal(1,10) = laTotal(1,10) + IIF(XSUM_STORE,lnYield1,lnYield)
    laTotal(1,11) = laTotal(1,11) + IIF(XSUM_STORE,lnReq1,(TOTQTY * lnYield))
    SELECT &lcTempLine
  ENDSCAN
  SET DEVICE TO PRINT
  SELECT &FabColor
ENDSCAN

DO lpStySub
DO lpFabSub
@ ROW,000 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,010 SAY 'GRAND TOTAL'
@ ROW,054 SAY laTotal(3,1)  PICTURE '@Z 99999'
@ ROW,060 SAY laTotal(3,2)  PICTURE '@Z 99999'
@ ROW,066 SAY laTotal(3,3)  PICTURE '@Z 99999'
@ ROW,072 SAY laTotal(3,4)  PICTURE '@Z 99999'
@ ROW,078 SAY laTotal(3,5)  PICTURE '@Z 99999'
@ ROW,084 SAY laTotal(3,6)  PICTURE '@Z 99999'
@ ROW,090 SAY laTotal(3,7)  PICTURE '@Z 99999'
@ ROW,096 SAY laTotal(3,8)  PICTURE '@Z 99999'
@ ROW,102 SAY laTotal(3,9)  PICTURE '9999999'
@ ROW,111 SAY laTotal(3,10) PICTURE '9999.99'
@ ROW,120 SAY laTotal(3,11) PICTURE '@Z 9999999.99'
ROW = ROW+1
@ ROW,000 SAY REPLICATE('=',132)
X = 1
DO WHILE X<=11
  laTotal(3,X) =0.00
  X = X+1
ENDDO
SELECT &FabColor
USE

RETURN

*!***********************************************************************
*! PROC          : lpStySub
*: DATE          : 01/15/2001
*: Developer     : Ahmed Abdel Naby (AAN)
*! DESC          : Printing subtotal of style
*!***********************************************************************
*! Example       : DO lpStySub
*!***********************************************************************

PROCEDURE lpStySub
@ ROW,000 SAY REPLICATE('-',132)
ROW = ROW+1
@ ROW,000 SAY IIF(SEEK(lcTmpSty,'Style'),SUBSTR(Style.DESC,1,12),SPACE(12))
@ ROW,026 SAY lcTmpSty
@ ROW,039 SAY '******'
@ ROW,054 SAY laTotal(1,1)  PICTURE '@Z 99999'
@ ROW,060 SAY laTotal(1,2)  PICTURE '@Z 99999'
@ ROW,066 SAY laTotal(1,3)  PICTURE '@Z 99999'
@ ROW,072 SAY laTotal(1,4)  PICTURE '@Z 99999'
@ ROW,078 SAY laTotal(1,5)  PICTURE '@Z 99999'
@ ROW,084 SAY laTotal(1,6)  PICTURE '@Z 99999'
@ ROW,090 SAY laTotal(1,7)  PICTURE '@Z 99999'
@ ROW,096 SAY laTotal(1,8)  PICTURE '@Z 99999'
@ ROW,102 SAY laTotal(1,9)  PICTURE '9999999'
@ ROW,111 SAY laTotal(1,10) PICTURE '9999.99'
@ ROW,120 SAY laTotal(1,11) PICTURE '@Z 9999999.99'
ROW = ROW+1
@ ROW,000 SAY REPLICATE('-',132)
ROW = ROW+1
lcTmpSty = Style
NEWSTYLE = .T.
X = 1
DO WHILE X<=11
  laTotal(2,X) = laTotal(2,X) + laTotal(1,X)
  laTotal(1,X) =0.00
  X = X+1
ENDDO

*!***********************************************************************
*! PROC          : lpFabSub
*: DATE          : 01/15/2001
*: Developer     : Ahmed Abdel Naby (AAN)
*! DESC          : Printing subtotal of fabric
*!***********************************************************************
*! Example       : DO lpFabSub
*!***********************************************************************

PROCEDURE lpFabSub

@ ROW,000 SAY REPLICATE('=',132)
ROW = ROW+1
@ ROW,013 SAY '('+lcFabric+' '+lcFabClr +')'
@ ROW,039 SAY '******'
@ ROW,054 SAY laTotal(2,1)  PICTURE '@Z 99999'
@ ROW,060 SAY laTotal(2,2)  PICTURE '@Z 99999'
@ ROW,066 SAY laTotal(2,3)  PICTURE '@Z 99999'
@ ROW,072 SAY laTotal(2,4)  PICTURE '@Z 99999'
@ ROW,078 SAY laTotal(2,5)  PICTURE '@Z 99999'
@ ROW,084 SAY laTotal(2,6)  PICTURE '@Z 99999'
@ ROW,090 SAY laTotal(2,7)  PICTURE '@Z 99999'
@ ROW,096 SAY laTotal(2,8)  PICTURE '@Z 99999'
@ ROW,102 SAY laTotal(2,9)  PICTURE '9999999'
@ ROW,111 SAY laTotal(2,10) PICTURE '9999.99'
@ ROW,120 SAY laTotal(2,11) PICTURE '@Z 9999999.99'
ROW = ROW+1
@ ROW,000 SAY REPLICATE('=',132)
ROW = ROW+1

X = 1
DO WHILE X<=11
  laTotal(3,X) = laTotal(3,X) + laTotal(2,X)
  STORE 0 TO laTotal(1,X),laTotal(2,X)
  X = X+1
ENDDO
lcFabric = Fabric
lcFabClr = FColor
*-- Rework 102124,6 Change varible value[Begin]
llNew = .T.
*-- Rework 102124,6[End]
*:****************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:****************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfEvalSegs()
*:****************************************************************
*
FUNCTION lfEvalSegs
lnMajSeg    = gfItemMask('SM')  && No. of major segments.
lnMajorLen  =  LEN(gfItemMask("PM"))
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

*-- Compute Variables titles. [begin]
lcStyMake  = lcStyMajor + ' (Make/Buy)'
*-- Compute Variables titles. [end]

RETURN ''

*-- End of lfEvalSegs.

*:****************************************************************
*: Name        : lfVSelec
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : sum a specific field for the current style in style file
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfVSelec()
*:****************************************************************
*
FUNCTION lfVSelec
CLEAR READ
*-- End of lfVSelec.

*!*************************************************************
*! Name      : lfsrvSty
*: DATE      : 01/15/2001
*: Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : Rise change style flag, in range browse screen.
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
*! Example   : =lfsrvSty()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfSRVSty
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
  OTHERWISE      && Valid code
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*: DATE      : 01/15/2001
*: Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : sum a specific field for the current style in style file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,style browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfStySum()
*!*************************************************************
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0
IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.


*!***************************************************************************
*! Name      : lfGetClrD
*:Developer  : Ahmed Abdel Naby- (AAN)
*:Date       : 04/01/2001
*! Purpose   : Function to Print Color Descreption
*!***************************************************************************
*! Calls     : None.
*!***************************************************************************
*! Passed Parameters  : None.
*!***************************************************************************
*! Returns            : None.
*!***************************************************************************
*! Example            :  lfGetClrD()
*!***************************************************************************
*
FUNCTION lfGetClrD
PRIVATE lnAlias ,lnClrLen ,lcStyNMj ,lcColorNm,lnClrPos
STORE 0 TO lnClrLen ,lnClrPos
STORE '' TO lcStyNMj
*--get color description
lnAlias = SELECT()
SELECT (lcTempLine)

DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
lcStyNMj = SUBSTR(STYLE,lnClrPos,lnClrLen)
*--Get Color Discreption
SELECT CODES
=SEEK('N'+lcStyNMj+'N'+'COLOR     ')
lcColorNm = LEFT(cdiscrep,15)
SELECT(lnAlias)
RETURN (lcColorNm)
*-- End Of lfGetClrD.

*!*************************************************************
*! Name      : lfvCoorGrp
*:Developer  : Ahmed Abdel Naby- (AAN) Due to C#101990
*:Date       : 04/01/2001
*! Purpose   : Rise change print coordinate groups flag.
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
*! Example   : =lfvCoorGrp()
*!*************************************************************
FUNCTION lfvCoorGrp
llChCoord = .T.
*-- end of lfvCoorGrp.

*!*************************************************************
*! Name      : lfvOStatus
*:Developer  : Ahmed Abdel Naby
*:Date       : 04/01/2001
*! Purpose   : - Evaluate Status expression.
*!           : - Rise change status flag. 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    * Add Feature to allow printing the Bid and Complete status.
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Bid','B',;
                              IIF(laRpTarget[lnI] = 'Open','O',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Complete','C',;                                                            
                              IIF(laRpTarget[lnI] = 'Canceled','X','')))))
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'BOHCX',ALLTRIM(lcRpStatus))
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh = .T.

ELSE  && else if length of current selected status equal previous length
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
DO lpChkStat
*-- end of lfvOStatus.

*!*************************************************************
*! Name      : lfSRVOrd
*: DATE      : 02/20/2001
*: Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : control browsing Orders.
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
*! Example   : =lfSRVOrd()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
*!c102024,4 - Add this function for control browsing Orders.
*!*************************************************************
FUNCTION lfSRVOrd
PARAMETERS lcParam

DO CASE
  CASE lcParam = 'S'
    SET ORDER TO CUSTOMER IN CUSTOMER
    SELECT ORDHDR
    SET ORDER TO TAG "ORDHDR"
    SET RELATION TO IIF(STORE=SPACE(8) ,;
        'M'+ACCOUNT, 'S'+ACCOUNT+STORE) INTO CUSTOMER
  CASE lcParam = 'R'
    SELECT ORDHDR
    SET RELATION TO
    llClearOrd = .F.
ENDCASE

*!*************************************************************
*! Name      : lfvFabric
*: DATE      : 02/20/2001
*: Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : VALIDATE THE FABRIC
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
*!c102024,4 - Add this function for validate the fabric.
*!*************************************************************

FUNCTION lfvFabric

PRIVATE lcFabld,lcFabrc,lnSelFile,lcFaTag 

lcFabld   = VARREAD()
lcFabrc  = EVAL(lcFabld)

lnSelFile = SELECT(0)

SELECT FABRIC
lcFaTag   = ORDER('FABRIC')

SET ORDER TO TAG FABRIC IN FABRIC

IF !EMPTY(lcFabrc) .AND. ('?' $ lcFabrc .OR. !SEEK(lcFabrc , 'FABRIC'))
  =FABROW (@lcFabrc,'*')
ENDIF
 
&lcFabld = lcFabrc

SET ORDER TO lcFaTag

SELECT (lnSelFile)

*!*************************************************************
*! Name      : lfSRVFab
*: DATE      : 02/20/2001
*: Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
*!c102024,4 - Add this function for control browsing Fabric.
*!*************************************************************
FUNCTION lfSRVFab
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
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*: DATE      : 02/22/2001
*: Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : sum a specific field for the currunt fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
*!c102024,4 - Add this function for sum a specific field for the currunt fabric in fabric file.
*!*************************************************************
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
*-- end of lfFabSum.
*!*************************************************************
*! Name      : lfvStyle
*: DATE      : 02/22/2001
*: Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : validate style
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************

FUNCTION lfvStyle

lcStyle = VARREAD()

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE
