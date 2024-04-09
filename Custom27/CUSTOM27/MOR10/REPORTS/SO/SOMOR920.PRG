***********************************************************************
*: Program file  : SOMOR920.PRG (Converted from 26 TO 27 FOR MORELLE)
*: Program desc. : OPEN ORDER LINE ITEM REPORTS
*! Developer     : Adel Mohammed El Gazzar (ADEL)
*! Date          : 09/26/2000
*:         System: ARIA APPAREL FOR   M O R E L L E
*:       Refer to: (C101968)
*:**************************************************************************

*--Declare some variables.
lnMajPic   = LEN(gfItemMask("PM"))
NULDATE    = {}
lcSeasnStr = ''
lcDivStr   = ''
DIMENSION XSEASON(6), XDIVISION(6)
STORE SPACE(6) TO LOR,HOR
XSEASON   = SPACE(2)
XDIVISION = SPACE(2)
STORE NULDATE  TO LCO,HCO,LST,HS
XCOORGRP  = 'N'
lcStatus  = SPACE(3)
XGR       = SPACE(2)
STORE SPACE(6) TO CLR1,CLR2,CLR3,CLR4,CLR5,CLR6,CLR7,CLR8
STORE SPACE(1) TO lnPer1,lnPer2,lnPer3,lnPer4,lnPer5,lnPer6,lnPer7,lnPer8,lnPer9,;
                  lnAllPer,lcFilter,ZST,lcAccAls,lcStyAls
STORE 0 TO XQTY1,XQTY2,XQTY3,XQTY4,XQTY5,XQTY6,XQTY7,XQTY8,XTOTQTY,XAMT
xTitle     = SPACE(40)
llStyRange = .T.
llAccRange = .T.
lcNote     = .F.
lcSortCode = ' '
llSummarize= 'N'
XPRTWPSTK  = 'N'
xTitle     = SPACE(40)
lcCancel   = .F.
llNotes    = (gfGetMemVar('M_LN_NOTE',gcAct_Comp) = 'Y')
lcNotes    = ''
lcSele     = ''
TCLR       = ''
lcAccount = SPACE(5)
lcFabric  = SPACE(7)
STORE SPACE(12) TO lStyle,hStyle
STORE SPACE(6) TO LOR,HOR
LFCR = CHR(10) + CHR(13)

IF !lfUpdFltVar()
  DO CASE
    CASE lcRpSelcBy = 'S' AND !llStyRange
      WAIT WINDOW "Must select Style range...!"
      SET DEVICE TO SCREE
      RETURN
    CASE lcRpSelcBy = 'A' AND !llAccRange
      WAIT WINDOW "Must select Account range...!"
      SET DEVICE TO SCREE
      RETURN
  ENDCASE
ENDIF
*------------------------
* CREATE lcWorkTmp FILE
*------------------------
ldCompDate = {}
                 
PRIVATE XFILE

DIMENSION lnTotal(3,10)
DIMENSION lnClrTot(10)
DIMENSION lnOrdTot(10)

R_WIDTH  = 'W'          && STANDARD REPORT 'WIDE'
XREPORT  = 'SOMOR920'
XCOMPANY = TRIM(QCOMPANY)
STORE 0 TO lnPerCent,lnTotalQty,lnTotalPik

*-- If user change filter criteria then you must collect data again.
IF llOGFltCh
  IF RECCOUNT(lcAllTmp) > 0
    SELECT (lcAllTmp) 
    ZAP
  ENDIF
  *-----------------------------------------------
  * [ACCOUNT] APPEND BY ACCOUNT#
  *-----------------------------------------------
  IF lcSele='A'
     SELECT OrdHdr
     SET ORDER TO TAG ORDACCT
     SELECT ORDLINE
     SET ORDER TO TAG ORDLINE
     SELECT (lcAccAls)
     SCAN
       lcAccount = Account
       SELECT OrdHdr
       =SEEK(lcAccount)
       SCAN REST WHILE account+cordtype+order = lcAccount
         lcOrder = ORDER
         WAIT WINDOW "SELECTING ORDER: &lcOrder  ACCT: &lcAccount ..." NOWAIT
         SELECT ORDLINE
         =SEEK('O'+lcOrder)
         COPY REST TO (gcWorkDir+lcWorkTmp) ;
              WHILE cordtype+order+STR(lineno,6) = 'O'+lcOrder FOR TOTQTY > 0
         SELECT (lcAllTmp)
         APPEND FROM (gcWorkDir+lcWorkTmp)
       ENDSCAN
     ENDSCAN
  ENDIF
  *-------------------- END SELECTION BY ACCOUNT# -------------------
  *-------------------------------------------------------
  * [STYLE] APPEND BY STYLE NUMBER
  *-------------------------------------------------------
  IF lcSele = 'S'
    SELECT ORDLINE
    SET ORDER TO TAG ORDLINES
    SELECT (lcStyAls)
    SCAN
      WAIT WINDOW "Selecting orders for styles: "+PADR(&lcStyAls..cStyMajor,lnMajPic)+" ..." NOWAIT
      SELECT ORDLINE
      =SEEK(PADR(&lcStyAls..cStyMajor,lnMajPic))
      COPY REST TO (gcWorkDir+lcWorkTmp) ;
      WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) = PADR(&lcStyAls..cStyMajor,lnMajPic) FOR TOTQTY > 0          
      SELECT (lcAllTmp)
      APPEND FROM (gcWorkDir+lcWorkTmp)
    ENDSCAN
  ENDIF
  *----------------- END SELECTION BY STYLE NUMBER -------------------

  *--------------------------------------------------
  * [FABRIC] APPEND BY FABRIC CODE
  *--------------------------------------------------
  IF lcSele = 'F'
     SELECT ORDLINE
     SET ORDER TO TAG ORDLINES
     SELECT STYLE
     STYFILTER = "FABRIC=lcFabric"
     LOCATE ALL FOR &STYFILTER
     IF EOF()
       WAIT WINDOW "NO STYLES IN FABRIC GROUP &lcFabric" NOWAIT
       SET DEVICE TO SCREE
       RETURN
     ENDIF
     SCAN REST FOR FABRIC = lcFabric
       STYKEY = STYLE
       WAIT WINDOW "SELECTING ORDERS FOR STYLE: &STYKEY ..." NOWAIT
       SELECT ORDLINE
       =SEEK(STYKEY)
       COPY REST TO (gcWorkDir+lcWorkTmp) ;
       WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =;
       STYKEY FOR TOTQTY > 0
       SELECT (lcAllTmp)
       APPEND FROM (gcWorkDir+lcWorkTmp)
     ENDSCAN
     lcSele = ' '  &&-ONLY ONE FABRIC SELECTION PERMITTED
  ENDIF
  *----------------- END SELECTION BY FABRIC -------------------
ENDIF  

WAIT WINDOW "Selecting records. Please stand by" NOWAIT
SELE OrdHdr
SET ORDER TO TAG OrdHdr
SELE (lcAllTmp)
*-- If user change filter criteria then you must collect data again.
IF llOGFltCh
  IF lcSele = 'E'
    USE
    SELECT ORDLINE
    SET RELATION TO 'O'+ORDER INTO OrdHdr
    SET RELATION TO STYLE  INTO STYLE ADDITIVE
    GO TOP
    SET TALK ON
    LOCATE FOR &lcFilter
    COPY REST TO (gcWorkDir+lcAllTmp) FOR &lcFilter
    SET TALK OFF
    SELECT ORDLINE
    SET RELATION TO
    =gfOpenFile(gcWorkDir+lcAllTmp,'','EX')
    SET RELATION TO 'O'+ORDER INTO OrdHdr
    SET RELATION TO STYLE  INTO STYLE ADDITIVE
  ELSE
    SET RELATION TO 'O'+ORDER INTO OrdHdr
    SET RELATION TO STYLE  INTO STYLE ADDITIVE
    SET FILTER TO &lcFilter
  ENDIF
ENDIF  
SELECT (lcAllTmp)
GOTO TOP
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  RETURN
ENDIF

SELECT (lcAllTmp)
*------ SET UP RELATIONS --------*
SET RELATION TO STYLE  INTO STYLE
SET RELATION TO 'O'+ORDER  INTO ORDHDR  ADDITIVE
IF lcSortCode ='S'
  SET RELATION TO IIF(EMPTY(STORE),'M'+ACCOUNT,'S'+ACCOUNT+STORE) INTO CUSTOMER ADDITIVE
ELSE
  SET RELATION TO 'M'+ACCOUNT INTO CUSTOMER  ADDITIVE
ENDIF
GO TOP

*-----------------------------------------------------------
* [BREAK] SET UP SORT FILEDS & CONTROL BREAKS
*-----------------------------------------------------------
lcSort = ' '
DO CASE
  * ACCOUNT NUMBER SEQUENCE
  CASE lcSortCode='A'
    lcSort  = 'ACCOUNT+ORDER+STYLE+STR(LINENO,4)'
    BREAK  = 'CUSTOMER.ACCOUNT + SPACE(3) + CUSTOMER.BTNAME + LFCR + CUSTOMER.PHONE1'
    CTLSEQ = 'ACCOUNT NUMBER'
  * ORDER NUMBER SEQUENCE
  CASE lcSortCode='O'
    lcSort  = 'ORDER+STYLE+STR(LINENO,4)'
    BREAK  = 'ORDER + SPACE(3) + ACCOUNT'    && ACCOUNT IS INCLUDED HERE ONLY TO
    CTLSEQ = 'ORDER'
  * FABRIC GROUP
  CASE lcSortCode='F'
    lcSort  = 'STYLE.FABRIC+STYLE'
    BREAK  = 'STYLE.FABRIC'
    CTLSEQ = 'PRIMARY FABRIC'
  * STYLE/COLOR
  CASE lcSortCode = 'S'
    lcSort  = 'STYLE+DTOS(ORDHDR.COMPLETE)+ORDER+STR(LINENO,4)'
    BREAK  = ' '
    CTLSEQ = 'STYLE/COLOR'
  CASE lcSortCode = 'D'
    lcSort  = 'DTOS(ORDHDR.COMPLETE)+STYLE+ORDER+STR(LINENO,4)'
    BREAK  = 'DTOC(ORDHDR.COMPLETE)'
    CTLSEQ = 'COMPLETION DATE'
ENDCASE

*----------------------------------------------
* SORT TEMPORARY FILE
*----------------------------------------------
SELECT (lcAllTmp)
Z = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW "Sorting &Z selected records for report ..." NOWAIT
INDEX ON &lcSort TAG (lcAllTmp)
SET ORDER TO TAG (lcAllTmp)

*=========== P R I N T   R E P O R T============*
SELECT (lcAllTmp)
GOTO TOP
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*MOR920                                  ORDER DETAIL REPORT - 123456789012345678901234567890                             PAGE: 1234
*MM/DD/YY                                                                                                                 TIME: HH:SS
*
*1234567 12345678901234567890 1234.99 
*
*ACCT  ORDER    START  COMPLETE SP STYLE   CLR    (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8)   PIECES   PRICE PIKTKT DIFF.
*12345 123456 MM/DD/YY MM/DD/YY XX 1234567 123 111111 222222 333333 444444 555555 666666 777777 888888 12345678 1234.99 123456 12345
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
DO WHILE .T.
   lnAlias = SELECT()
   IF BREAK <> ' '
      HBREAK = &BREAK
   ENDIF
   lcStyle  = SUBSTR(Style,1,lnMajPic)
   CTLCOLOR = STYLE
   CTLORDER = ACCOUNT+ORDER+ORDHDR.CUSTPO
   STYDESC  = STYLE.DESC
   STYFAB   = STYLE.FABRIC
   XSCALE   = STYLE.SCALE
   NEWSTYLE = .T.
   lcNotes  = ALLTRIM(ORDHDR.NOTE1)+' '+ALLTRIM(ORDHDR.NOTE2)
   lcOrder  = ORDHDR.ORDER
   PageNo = 0
   lnRow    = 99
   XTIME    = SUBSTR(TIME(),1,5)
   XPRV_KEY=SPACE(1)
   *--------------------------------------------------------
   *  (1) STYLE
   *  (2) USER DEFINED CONTROL BREAK
   *  (3) GRAND TOTALS
   *--------------------------------------------------------
   lnTotal  = 0.00   &&-ARRAY(3,10)
   lnClrTot = 0.00   &&-ARRAY(10)
   lnOrdTot = 0.00   &&-ARRAY(10)
   *-----------------------------------------------------
   * [REPORT] BEGIN REPORT LOOP
   *-----------------------------------------------------
   R_TITLE = " ORDER DETAIL REPORT "
   WAIT WINDOW "Report printing - <SPACE BAR> to abort" NOWAIT
   SET DEVICE TO PRINT
   DO WHILE INKEY() <>32
      IF lnRow >=54
         PageNo = PageNo +1
         DO RPT_HDR WITH 'SOMOR920','(SUB TOTALS BY: ' + CTLSEQ +') '+ XTITLE,R_WIDTH
         IF lcSortCode = 'S'
           @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER       (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8)   PIECES   PRICE PIKTKT DIFF.'
         ELSE
           @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE   CLR    (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8)   PIECES   PRICE PIKTKT DIFF.'
         ENDIF
         @ 06,000 SAY REPLICATE('-',132)
         lnRow = 7
      ENDIF

      SELECT (lcAllTmp)
      LEVEL = 0
      IF STYLE <> lcStyle
         LEVEL = 1
      ENDIF
      DOCLR = (lcSortCode='S' .AND. STYLE<>CTLCOLOR)
      DOORD = (lcSortCode='A' .AND. ACCOUNT+ORDER+ORDHDR.CUSTPO<>CTLORDER)
      IF !EMPTY(BREAK)
         IF HBREAK <> &BREAK
            LEVEL = 2
         ENDIF
      ENDIF
      *----------------------------------------------------
      * [DOCLR] STYLE/CLR SUB TOTALS
      *----------------------------------------------------
      IF DOCLR
         lnRow = lnRow+1
         @ lnRow,030 SAY '***'
         @ lnRow,034 SAY SUBSTR(LEFT(CTLCOLOR,7),1,7)
         @ lnRow,042 SAY SUBSTR(RIGHT(CTLCOLOR,3),1,3)
         @ lnRow,046 SAY lnClrTot(1,1)  PICTURE '@Z 999999'
         @ lnRow,053 SAY lnClrTot(1,2)  PICTURE '@Z 999999'
         @ lnRow,060 SAY lnClrTot(1,3)  PICTURE '@Z 999999'
         @ lnRow,067 SAY lnClrTot(1,4)  PICTURE '@Z 999999'
         @ lnRow,074 SAY lnClrTot(1,5)  PICTURE '@Z 999999'
         @ lnRow,081 SAY lnClrTot(1,6)  PICTURE '@Z 999999'
         @ lnRow,088 SAY lnClrTot(1,7)  PICTURE '@Z 999999'
         @ lnRow,095 SAY lnClrTot(1,8)  PICTURE '@Z 999999'
         @ lnRow,102 SAY lnClrTot(1,9)  PICTURE '9999999'
         @ lnRow,118 SAY lnClrTot(1,10)  PICTURE '$9999999.99'
         lnRow = lnRow+2
         CTLCOLOR = STYLE
         lnClrTot = 0.00
      ENDIF

      *----------------------------------------------------
      * [DOORD] ACCOUNT/ORDER SUB TOTALS
      *----------------------------------------------------
      IF DOORD
         @ lnRow,000 SAY '***'
         @ lnRow,006 SAY LEFT(CTLORDER,5)
         @ lnRow,012 SAY SUBSTR(CTLORDER,6,6)
         @ lnRow,019 SAY 'CUST. PO# :'
         @ lnRow,031 SAY RIGHT(CTLORDER,10)
         @ lnRow,046 SAY lnOrdTot(1,1)  PICTURE '@Z 999999'
         @ lnRow,053 SAY lnOrdTot(1,2)  PICTURE '@Z 999999'
         @ lnRow,060 SAY lnOrdTot(1,3)  PICTURE '@Z 999999'
         @ lnRow,067 SAY lnOrdTot(1,4)  PICTURE '@Z 999999'
         @ lnRow,074 SAY lnOrdTot(1,5)  PICTURE '@Z 999999'
         @ lnRow,081 SAY lnOrdTot(1,6)  PICTURE '@Z 999999'
         @ lnRow,088 SAY lnOrdTot(1,7)  PICTURE '@Z 999999'
         @ lnRow,095 SAY lnOrdTot(1,8)  PICTURE '@Z 999999'
         @ lnRow,102 SAY lnOrdTot(1,9)  PICTURE '9999999'
         @ lnRow,118 SAY lnOrdTot(1,10)  PICTURE '$9999999.99'
         @ lnRow+1,000 SAY lcNotes
         lnRow = lnRow+1
         =lfPrnNotPd()
         lnRow = lnRow+2
         CTLORDER = ACCOUNT+ORDER+ORDHDR.CUSTPO
         lcNotes   = ALLTRIM(ORDHDR.NOTE1)+' '+ALLTRIM(ORDHDR.NOTE2)
         lcOrder   = ORDHDR.ORDER
         lnOrdTot = 0.00
      ENDIF
      *----------------------------------------------------
      * [LEVEL 1] STYLE SUB TOTALS
      *----------------------------------------------------
      IF LEVEL >=1
         NEWSTYLE = .F.
         * PRINT STYLE SUBTOTAL ONLY FOR REPORTS S,F
         IF lcSortCode $ 'SF'
            @ lnRow,000 SAY REPLICATE('-',132)
            lnRow = lnRow+1
            @ lnRow,000 SAY STYDESC
            @ lnRow,021 SAY '(&STYFAB.)'
            @ lnRow,034 SAY SUBSTR(lcStyle,1,7)
            @ lnRow,042 SAY '***'
            @ lnRow,046 SAY lnTotal(1,1)  PICTURE '@Z 999999'
            @ lnRow,053 SAY lnTotal(1,2)  PICTURE '@Z 999999'
            @ lnRow,060 SAY lnTotal(1,3)  PICTURE '@Z 999999'
            @ lnRow,067 SAY lnTotal(1,4)  PICTURE '@Z 999999'
            @ lnRow,074 SAY lnTotal(1,5)  PICTURE '@Z 999999'
            @ lnRow,081 SAY lnTotal(1,6)  PICTURE '@Z 999999'
            @ lnRow,088 SAY lnTotal(1,7)  PICTURE '@Z 999999'
            @ lnRow,095 SAY lnTotal(1,8)  PICTURE '@Z 999999'
            @ lnRow,102 SAY lnTotal(1,9)  PICTURE '9999999'
            @ lnRow,118 SAY lnTotal(1,10)  PICTURE '$9999999.99'
            lnRow = lnRow+1
            @ lnRow,000 SAY REPLICATE('-',132)
            lnRow = lnRow+2
            lcStyle = STYLE
            STYDESC  = STYLE.DESC
            STYFAB   = STYLE.FABRIC
            XSCALE   = STYLE.SCALE
            NEWSTYLE = .T.
         ENDIF
         X = 1
         DO WHILE X<=10
            lnTotal(2,X) = lnTotal(2,X) + lnTotal(1,X)
            lnTotal(3,X) = lnTotal(3,X) + lnTotal(1,X)
            lnTotal(1,X) =0.00
            X = X+1
         ENDDO
      ENDIF
      *------------------ END USER DEFINED SUB TOTALS -------------------

      *----------------------------------------------------
      * [LEVEL 2] USER DEFINED SUB TOTALS
      *----------------------------------------------------
      IF BREAK<>' ' .AND. LEVEL >=2
         @ lnRow,000 SAY REPLICATE('-',132)
         lnRow = lnRow+1
         IF lcSortCode='O'
           @ lnRow,000 SAY SUBSTR(HBREAK,10,5)
           @ lnRow,006 SAY SUBSTR(HBREAK,1,6)
         ELSE
           @ lnRow,000 SAY HBREAK
         ENDIF
         IF LFCR $ HBREAK
            lnRow = lnRow+1
         ENDIF
         @ lnRow,046 SAY lnTotal(2,1)  PICTURE '@Z 999999'
         @ lnRow,053 SAY lnTotal(2,2)  PICTURE '@Z 999999'
         @ lnRow,060 SAY lnTotal(2,3)  PICTURE '@Z 999999'
         @ lnRow,067 SAY lnTotal(2,4)  PICTURE '@Z 999999'
         @ lnRow,074 SAY lnTotal(2,5)  PICTURE '@Z 999999'
         @ lnRow,081 SAY lnTotal(2,6)  PICTURE '@Z 999999'
         @ lnRow,088 SAY lnTotal(2,7)  PICTURE '@Z 999999'
         @ lnRow,095 SAY lnTotal(2,8)  PICTURE '@Z 999999'
         @ lnRow,102 SAY lnTotal(2,9)  PICTURE '9999999'
         @ lnRow,118 SAY lnTotal(2,10)  PICTURE '$9999999.99'
         lnRow = lnRow+1
         @ lnRow,000 SAY REPLICATE('-',132)
         lnRow = lnRow+2
         HBREAK = &BREAK
         X = 1
         DO WHILE X<=10
            lnTotal(2,X) =0.00
            X = X+1
         ENDDO
      ENDIF
      *------------------ END USER DEFINED SUB TOTALS -------------------
      IF EOF(lnAlias)
         EXIT
      ENDIF
      *------------------------------------------------
      * PROCESS DETAIL RECORD
      *------------------------------------------------
      IF NEWSTYLE
         NEWSTYLE = .F.
         SELECT SCALE
         SEEK 'S'+XSCALE
         @ lnRow,046 SAY PADL(ALLTRIM(Sz1),6)
         @ lnRow,053 SAY PADL(ALLTRIM(Sz2),6)
         @ lnRow,060 SAY PADL(ALLTRIM(Sz3),6)
         @ lnRow,067 SAY PADL(ALLTRIM(Sz4),6)
         @ lnRow,074 SAY PADL(ALLTRIM(Sz5),6)
         @ lnRow,081 SAY PADL(ALLTRIM(Sz6),6)
         @ lnRow,088 SAY PADL(ALLTRIM(Sz7),6)
         @ lnRow,095 SAY PADL(ALLTRIM(Sz8),6)
         lnRow=lnRow+1
      ENDIF
      SELECT (lcAllTmp)
      ****  SHOW STOCK+WIP FOR NEXT STYLE/COLOR ****
      IF lcSortCode='S' .AND. XPRV_KEY <> STYLE
        XPRV_KEY=STYLE
        SELECT STYLE
        SEEK XPRV_KEY
        IF FOUND()
          SELE STYLE
          @ lnRow,000 SAY SUBSTR(STYLE,1,7)
          @ lnRow,008 SAY SUBSTR(STYLE.STYLE,lnNonMajSt,3)
          lcColorDesc = gfCodDes(SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen) , 'COLOR')  
          @ lnRow,012 SAY lcColorDesc
          @ lnRow,032 SAY 'WIP + STOCK :'
          @ lnRow,046 SAY WIP1+STK1  PICTURE '@Z 999999'
          @ lnRow,053 SAY WIP2+STK2  PICTURE '@Z 999999'
          @ lnRow,060 SAY WIP3+STK3  PICTURE '@Z 999999'
          @ lnRow,067 SAY WIP4+STK4  PICTURE '@Z 999999'
          @ lnRow,074 SAY WIP5+STK5  PICTURE '@Z 999999'
          @ lnRow,081 SAY WIP6+STK6  PICTURE '@Z 999999'
          @ lnRow,088 SAY WIP7+STK7  PICTURE '@Z 999999'
          @ lnRow,095 SAY WIP8+STK8  PICTURE '@Z 999999'
          @ lnRow,102 SAY TOTWIP+TOTSTK  PICTURE '9999999'
          lnRow = lnRow+1
        ENDIF
        SELE (lcAllTmp)
      ENDIF

      *********************************************************************
      ****************** PRINT THE LINE ITEM DETAIL ***********************
      *********************************************************************
      SELECT (lcAllTmp)
      IF lcRpPrint = 'D'
         @ lnRow,000 SAY ACCOUNT
         @ lnRow,006 SAY ORDER
         @ lnRow,013 SAY ORDHDR.START
         @ lnRow,022 SAY ORDHDR.COMPLETE
         @ lnRow,031 SAY ORDHDR.STATUS
         @ lnRow,032 SAY ORDHDR.PRIORITY
         IF lcSortCode = 'S'
           @ lnRow,034 SAY SUBSTR(CUSTOMER.STNAME,1,10)
         ELSE
           @ lnRow,000 SAY SUBSTR(STYLE,1,7)
           @ lnRow,008 SAY SUBSTR(STYLE.STYLE,lnNonMajSt,3)
         ENDIF
         @ lnRow,046 SAY QTY1    PICTURE '@Z 999999'
         @ lnRow,053 SAY QTY2    PICTURE '@Z 999999'
         @ lnRow,060 SAY QTY3    PICTURE '@Z 999999'
         @ lnRow,067 SAY QTY4    PICTURE '@Z 999999'
         @ lnRow,074 SAY QTY5    PICTURE '@Z 999999'
         @ lnRow,081 SAY QTY6    PICTURE '@Z 999999'
         @ lnRow,088 SAY QTY7    PICTURE '@Z 999999'
         @ lnRow,095 SAY QTY8    PICTURE '@Z 999999'
         @ lnRow,102 SAY TOTQTY  PICTURE '9999999'
         @ lnRow,111 SAY PRICE   PICTURE '9999.99'
         @ lnRow,119 SAY PIKTKT
         IF .NOT. EMPTY(PIKTKT) .AND. ((TOTQTY - TOTPIK)>0)
           @ lnRow,126 SAY TOTQTY - TOTPIK PICTURE '99999'
         ENDIF
        lnRow = lnRow+1
      ENDIF
      **********************************************************************
      ********************* END PRINT THE LINE DETAIL **********************
      **********************************************************************

      lnTotal(1,1) = lnTotal(1,1) +  QTY1
      lnTotal(1,2) = lnTotal(1,2) +  QTY2
      lnTotal(1,3) = lnTotal(1,3) +  QTY3
      lnTotal(1,4) = lnTotal(1,4) +  QTY4
      lnTotal(1,5) = lnTotal(1,5) +  QTY5
      lnTotal(1,6) = lnTotal(1,6) +  QTY6
      lnTotal(1,7) = lnTotal(1,7) +  QTY7
      lnTotal(1,8) = lnTotal(1,8) +  QTY8
      lnTotal(1,9) = lnTotal(1,9) +  TOTQTY
      lnTotal(1,10) = lnTotal(1,10) +  TOTQTY * PRICE

      IF lcSortCode = 'S'
         lnClrTot(1) = lnClrTot(1) +  QTY1
         lnClrTot(2) = lnClrTot(2) +  QTY2
         lnClrTot(3) = lnClrTot(3) +  QTY3
         lnClrTot(4) = lnClrTot(4) +  QTY4
         lnClrTot(5) = lnClrTot(5) +  QTY5
         lnClrTot(6) = lnClrTot(6) +  QTY6
         lnClrTot(7) = lnClrTot(7) +  QTY7
         lnClrTot(8) = lnClrTot(8) +  QTY8
         lnClrTot(9) = lnClrTot(9) +  TOTQTY
         lnClrTot(10) = lnClrTot(10) +  TOTQTY * PRICE
      ENDIF
      IF lcSortCode = 'A'
         lnOrdTot(1) = lnOrdTot(1) +  QTY1
         lnOrdTot(2) = lnOrdTot(2) +  QTY2
         lnOrdTot(3) = lnOrdTot(3) +  QTY3
         lnOrdTot(4) = lnOrdTot(4) +  QTY4
         lnOrdTot(5) = lnOrdTot(5) +  QTY5
         lnOrdTot(6) = lnOrdTot(6) +  QTY6
         lnOrdTot(7) = lnOrdTot(7) +  QTY7
         lnOrdTot(8) = lnOrdTot(8) +  QTY8
         lnOrdTot(9) = lnOrdTot(9) +  TOTQTY
         lnOrdTot(10) = lnOrdTot(10) +  TOTQTY * PRICE
      ENDIF
      SELE (lcAllTmp)
      SKIP
   ENDDO
   *---------------------- END OF REPORT -------------------------

   *----------------------------------------------------
   * [LEVEL 3] GRAND TOTALS
   *----------------------------------------------------
   IF lnRow >=60
      lnRow = 5
   ENDIF
   @ lnRow,000 SAY REPLICATE('=',132)
   lnRow = lnRow+1

   @ lnRow,010 SAY 'GRAND TOTALS'
   @ lnRow,046 SAY lnTotal(3,1)  PICTURE '@Z 999999'
   @ lnRow,053 SAY lnTotal(3,2)  PICTURE '@Z 999999'
   @ lnRow,060 SAY lnTotal(3,3)  PICTURE '@Z 999999'
   @ lnRow,067 SAY lnTotal(3,4)  PICTURE '@Z 999999'
   @ lnRow,074 SAY lnTotal(3,5)  PICTURE '@Z 999999'
   @ lnRow,081 SAY lnTotal(3,6)  PICTURE '@Z 999999'
   @ lnRow,088 SAY lnTotal(3,7)  PICTURE '@Z 999999'
   @ lnRow,095 SAY lnTotal(3,8)  PICTURE '@Z 999999'
   @ lnRow,102 SAY lnTotal(3,9)  PICTURE '9999999'
   @ lnRow,118 SAY lnTotal(3,10)  PICTURE '$9999999.99'
   lnRow = lnRow+1
   @ lnRow,000 SAY REPLICATE('=',132)
   DO ENDREPORT
   EXIT
ENDDO

RETURN
*---------------------------
*   END SOMOR920.PRG
*---------------------------


*!*************************************************************
*! Name      : lfUpdFltVar
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Cut filter.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUpdFltVar()
*!*************************************************************
FUNCTION lfUpdFltVar

lcSele      = lcRpSelcBy
lcSortCode  = lcRpSort
xTitle      = lcRpOpTitl
XCOORGRP    = lcRpCorGrp
FOR lnInd  = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
  
*--- Add Fabric# to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.FABRIC' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcFabric = ALLTRIM(laOgFxFlt[lnInd,6])
      
*--- Add Style Temp File to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.STYLE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      IF !USED(laOgFxFlt[lnInd,6]))
        llStyRange = .F.
        RETURN .F.
      ENDIF
      lcStyAls = laOgFxFlt[lnInd,6]
      
*--- Add Order# to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.ORDER' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LOR = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,6))
      HOR = ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],8,20))
      
*--- Add Season list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSeasnStr = laOgFxFlt[lnInd,6]
      
*--- Add Order Status list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.STATUS' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      ZST = laOgFxFlt[lnInd,6]

*--- Add Division list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcDivStr = laOgFxFlt[lnInd,6]
      
*--- Add Prioroty list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDHDR.PRIORITY' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      lnAllPer = laOgFxFlt[lnInd,6]
      
*--- Add Color list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'SUBSTR(STYLE.STYLE,lnNonMajSt,lnColorLen)' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      TCLR = laOgFxFlt[lnInd,6]

*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      XGR = laOgFxFlt[lnInd,6]

*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDLINE.COMPLETE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LCO =  CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1, ATC('|',laOgFxFlt[lnInd,6])-1 )) )
      HCO =  CTOD( ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],ATC('|',laOgFxFlt[lnInd,6])+1,10)) )
*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'ORDLINE.START' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      LST = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,ATC('|',laOgFxFlt[lnInd,6])-1)) )
      HS  = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],ATC('|',laOgFxFlt[lnInd,6])+1,10) ))
*--- Add GROUP list to the filter
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.ACCOUNT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      IF !USED(laOgFxFlt[lnInd,6]))
        llAccRange = .F.
        RETURN .F.
      ENDIF
      lcAccAls = laOgFxFlt[lnInd,6]
  ENDCASE
ENDFOR
lcFilter = 'TOTQTY>0'
IF !EMPTY(LOR)
  lcFilter = lcFilter + ".AND.BETWEEN(ORDER,LOR,HOR)"
ENDIF
IF !EMPTY(ZST)
  lcFilter = lcFilter + '.AND.OrdHdr.STATUS $ ZST'
ENDIF
IF !EMPTY(XGR)
  lcFilter = lcFilter + '.AND.STYLE.GROUP = XGR'
ENDIF
IF !EMPTY( lcSeasnStr )
  lcFilter = lcFilter + ".AND. ORDLINE.SEASON $ lcSeasnStr "
ENDIF
IF !EMPTY( lcDivStr )
  lcFilter = lcFilter + ".AND. OrdHdr.cDIVISION $ lcDivStr "
ENDIF
IF !EMPTY(LCO)
  lcFilter = lcFilter + '.AND.BETWEEN(OrdHdr.COMPLETE,LCO,HCO)'
ENDIF
IF !EMPTY(LST)
  lcFilter = lcFilter + '.AND.BETWEEN(OrdHdr.START,LST,HS)'
ENDIF
IF !EMPTY(TCLR)
  lcFilter = lcFilter + '.AND.SUBSTR(Style,14,6) $ TCLR'
ENDIF
IF XCOORGRP = 'Y'
  lcFilter  = lcFilter + ".AND. !EMPTY(GROUP)"
ENDIF


*!*************************************************************
*! Name      : lfvOrder  (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
FUNCTION lfvOrder
PRIVATE lcVar , lcObj , laTemp,lcAlias

lcAlias = ALIAS()
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*--IF Statment to check if we are going to Browse
SELECT ORDHDR
SET ORDER TO TAG ORDHDR
IF !EMPTY(lcObj) AND ('?' $ lcObj OR !SEEK ('O'+lcObj))
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value
  lcBrFields = "ORDER     :R :H= 'ORDER#' , "    +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "Terms=gfCodDes(CTERMCODE , 'CTERMCODE') :R :H= 'Terms' ,"  +;
               "ShipV=gfCodDes(ShipVia , 'SHIPVIA')   :R :H= 'ShipVia' ,"  +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "
  lcFile_Ttl = "Orders..."
  lcBrowCond = [FOR STATUS != "X" AND OPEN >= 1 ]
  = gfBrows(lcBrowCond,'ORDER','laTemp')
  *--IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = ''
  ENDIF
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
SELECT (lcAlias)


*!*************************************************************
*! Name      : lfvAccount  (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvAccount
PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user wants to Browse or the Account is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF


*!*************************************************************
*! Name      : lfvSelcBy  (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Validate select by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : 
*!           : 
*!           : 
*!*************************************************************
*! Example   : =lfvSelcBy()
*!*************************************************************
FUNCTION lfvSelcBy

STORE .T. TO llChSelect,llClearAcc,llClearSty,llClearFab,llClearLoc,llClearRep
CLEAR READ


*!*************************************************************
*! Name      : lfvSortBy  (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Validate Sort by option in option grid.
*!           : [Simply it enable and disable selecting buttons]
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : lfSelcObjs
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Note      : 
*!           : 
*!           : 
*!*************************************************************
*! Example   : =lfvSortBy()
*!*************************************************************
FUNCTION lfvSortBy
*--- llRpSupr
llRpSupr = .F.
DO CASE
  CASE lcRPSort $ 'OSFCR'
    llRpSupr = .T.
  OTHERWISE
    llRpSupr = .F.
ENDCASE
CLEAR READ


*!*************************************************************
*! Name      : lfvFabric  (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
FUNCTION lfvFabric

lcFabObj = VARREAD()
lcFab    = &lcFabObj
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC
IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC


*!*************************************************************
*! Name      : lfEvalSegs    (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Evaluate NonMajor Type and variables.
*!*************************************************************
*! Called from : [Option Grid] lcDummy variable.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEvalSegs()
*!*************************************************************
FUNCTION lfEvalSegs
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
  
*-- if you does not find Non Major Type Color Code.
IF !lfNMajType('C',lnMajSeg)  
  = lfNMajType('F',lnMajSeg)  && Check for Non Major Type Free code.
ENDIF  && end if you does not find Non Major Type Color Code.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''
*-- end of lfEvalSegs.


*!*************************************************************
*! Name      : lfNMajType   (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Mask NonMajor segments .
*!*************************************************************
*! Called from : lfEvalSegs.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNMajType()
*!*************************************************************
FUNCTION lfNMajType
PARAMETERS lcNMajType,lnMajSegs

*-- Loop Around Non Major elements.
FOR lnI = lnMajSegs + 1 TO ALEN(laMajSegs,1)

  IF laMajSegs[lnI,1] = lcNMajType

    lcFree_Clr = IIF(EMPTY(lcFree_Clr),laMajSegs[lnI,1],lcFree_Clr)
    lnNonMajSt = IIF(lnNonMajSt = 0,laMajSegs[lnI,4],lnNonMajSt)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSegs[lnI,3],;
                     lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])

    lcNonMajTl = IIF(EMPTY(lcNonMajTl),PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                     lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

RETURN !EMPTY(lcFree_Clr)
*-- end of lfNMajType. 

*!*************************************************************
*! Name      : lfwRepWhen    (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

R_WIDTH  = 'W'          && STANDARD REPORT 'WIDE'
IF !USED(lcAllTmp)
  SELECT ORDLINE
  COPY STRUCTURE TO (gcWorkDir+lcAllTmp)
  =gfOpenFile(gcWorkDir+lcAllTmp,'','EX')
ENDIF
=gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')




*!*************************************************************
*! Name      : lfsrvSty   (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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
ENDCASE
*-- end of lfsrvSty.


*!*************************************************************
*! Name      : lfStySum   (C#101404)
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
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
lnStyRec = IIF(RECNO('STYLE') <= RECCOUNT('STYLE'),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)


*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 09/26/2000
*! Purpose   : Rise change account flag, in range browse screen.
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
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.


*!*************************************************************
*! Name      : lfPrnNotPd
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 22/04/98
*! Purpose   : Printing the Order notepad
*!*************************************************************
*! Example            :  lfPrnNotPd()
*!*************************************************************
FUNCTION lfPrnNotPd
PRIVATE lnAlias, lnOldMemW, lnMemLins, lnNotLine

lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
llQuit    = .T.
SET MEMOWIDTH TO 66
SELECT NotePad
IF SEEK ('B' + lcOrder)
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  DO WHILE lnNotLine <= lnMemLins
    IF lnRow >=54
      PageNo = PageNo +1
      DO RPT_HDR WITH 'SOMOR920','(SUB TOTALS BY: ' + CTLSEQ +') '+ XTITLE,R_WIDTH
      IF lcSortCode = 'S'
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP CUSTOMER       (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8)   PIECES   PRICE PIKTKT DIFF.'
      ELSE
        @ 05,000 SAY 'ACCT  ORDER    START  COMPLETE SP STYLE   CLR    (1)    (2)    (3)    (4)    (5)    (6)    (7)    (8)   PIECES   PRICE PIKTKT DIFF.'
      ENDIF
      @ 06,000 SAY REPLICATE('-',132)
      lnRow = 7
    ENDIF
    IF !EMPTY(MLINE(MNOTES,lnNotLine)) .AND. ;
         (UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) <>'P';
         .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01))
      IF  SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1) <> '*';
         .OR. SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1) <> SPACE(01)
        @ lnRow,00 SAY IIF(UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),1,1)) = 'I' .AND.;
                         UPPER(SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,1)) = SPACE(01) ,;
                         SUBSTR(ALLTRIM(MLINE(MNOTES,lnNotLine)),2,LEN(ALLTRIM(MLINE(MNOTES,lnNotLine)))), ALLTRIM(MLINE(MNOTES,lnNotLine)))
        lnRow = lnRow + 1
      ENDIF
    ENDIF   
    lnNotLine = lnNotLine + 1
  ENDDO     
ENDIF
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)
