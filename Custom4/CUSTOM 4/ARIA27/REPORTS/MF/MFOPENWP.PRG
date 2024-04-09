*:***************************************************************************
*: Program file  : MFOPENWP
*: Program desc. : OPEN WORK IN PROCESS REEPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : AHMED MOHAMMED Ibrahim
*: Reference     : *E301115,1 AMM
*: Notes         : This reports is built from the A26 report (MFG950)
*:***************************************************************************
*: Calls : 
*:    Procedures : MFG950A(), MFG950B()
*:    Functions  : lfvFormAB(), lfvOStatus(), lfwOGWhen(), lfGetTit(), 
*:                 lfAdjSeg(), lfvSrtBy(), lfvVend()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO MFOPENWP
*:***************************************************************************
*: Modifications:
*B603160,1 AMM 09/20/1999 Fix some bugs in reports
*B803940,1 AMH 01/07/2001 Adding the option sort by customer and customer delivery date.
*B605227,1 KHM 12/23/2001 Fix the bug of printing MULTI in the case of
*B605227,1                generating CT from multi sales orders.
*:***************************************************************************
*IF gcDevice = 'PRINTER' .AND. _PDPARMS[1] 
*  WAIT WINDOW 'Change the papaer to landscape.'
*  RETURN
*ENDIF
IF lcRpNO = 'B' .AND. EMPTY(XMFG)
  *-- Message 'You have to enter the MFG code'
  =gfModalGen('TRM00250B00000','DIALOG','MFG code')
  RETURN
ENDIF

R_TITLE  = 'OPEN WORK IN PROCESS REPORT'
XREPORT  = 'MFOPENWP'
STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT31,XTOT4,XTOT5,XTOT6,XTOTA1,XTOTA2,XTOTA3,XTOTA31,XTOTA4,XTOTA5,XTOTA6
STORE 0 TO XGTOT1,XGTOT2,XGTOT3,XGTOT31,XGTOT4,XGTOT5,XGTOT6
PAGENO = 0
ROW    = 99
XTIME  = TIME()
llVend = .F.
STORE ' ' TO lcFabtype,lcTrmtype
R_WIDTH = 'XW'       && XTRA WIDE REPORT

* Get the cost elements types and titles
DIMENSION laCost[10,2]
laCost[1,1]  = 'M_CMTYPE1 '
laCost[2,1]  = 'M_CMTYPE2 '
laCost[3,1]  = 'M_CMTYPE3 '
laCost[4,1]  = 'M_CMTYPE4 '
laCost[5,1]  = 'M_CMTYPE5 '
laCost[6,1]  = 'M_CMSLBL1 '
laCost[7,1]  = 'M_CMSLBL2 '
laCost[8,1]  = 'M_CMSLBL3 '
laCost[9,1]  = 'M_CMSLBL4 '
laCost[10,1] = 'M_CMSLBL5 '
=gfGetMemvar(@laCost,gcAct_Comp)
lcTmpOPH = GFTEMPNAME()
CREATE CURSOR (lcTmpOPH) (CTKTNO C(6), cOperSeq C(2),cOprCode C(6), ;
                          cContCode C(8) )
INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH
WORKFILE  = GFTEMPNAME()
SORTFIELD = ' '
BREAK     = ' '

QCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')

SELECT CUTTKTH
SET RELATION TO STYLE INTO STYLE
lcRpExp = IIF(EMPTY(lcRpStatus),lcRpExp+".AND. STATUS $ 'AOHS' " , lcRpExp+".AND. STATUS $ lcRpStatus")
LOCATE ALL FOR &lcRpExp

IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*XPGBREAK =.F.       && USED TO FLAG IF NEW PAGE ON EVERY BREAK
                    && REQUESTED BY ROBYN ON 10/29/91

DO CASE
  *B803940,1 AMH [Start] Adding the option (Customer + Delivery date) to the
  *B803940,1 AMH         sort options.
  *CASE lnRpSrtCd = 1
  CASE lnRpSrtCd = 1
    SORTFIELD = 'ACCOUNT+DTOS(DEL_DATE)+CUTTKT'
    BREAK     = 'ACCOUNT'
    TOTALD    = 'ACCOUNT + CUSTOMER DELIVERY DATE'

  CASE lnRpSrtCd = 2
  *B803940,1 AMH [End]
    SORTFIELD = 'CUTTKT'

  *B803940,1 AMH [Start]
  *CASE lnRpSrtCd = 2
  CASE lnRpSrtCd = 3
  *B803940,1 AMH [End]
    SORTFIELD = 'CONTR1+CUTTKT'
    BREAK     = 'CONTR1'
    TOTALD    = 'FIRST CONTRACTOR + CUTTKT #'

  *B803940,1 AMH [Start]
  *CASE lnRpSrtCd = 3
  CASE lnRpSrtCd = 4
  *B803940,1 AMH [End]
    SORTFIELD = 'PATTERN+CUTTKT'
    BREAK     = 'PATTERN'
    TOTALD    = 'PATTERN '

  *B803940,1 AMH [Start]
  *CASE lnRpSrtCd = 4
  CASE lnRpSrtCd = 5
  *B803940,1 AMH [End]
    SORTFIELD = 'CONTR2+DTOS(COMPLETE)+CUTTKT'
    BREAK     = 'CONTR2'
    TOTALD    = 'SECOND CONTRACTOR + COMPLETION DATE'

  *B803940,1 AMH [Start]
  *CASE lnRpSrtCd = 5
  CASE lnRpSrtCd = 6
  *B803940,1 AMH [End]
    SORTFIELD = 'CONTR2+CUTTKT'
    BREAK     = 'CONTR2'
    TOTALD    = 'SECOND CONTRACTOR + CUTTKT #'

  *B803940,1 AMH [Start]
  *CASE lnRpSrtCd = 6
  CASE lnRpSrtCd = 7
  *B803940,1 AMH [End]
    SORTFIELD = 'CONTR1+STATUS+CUTTKT'
    BREAK     = 'CONTR1+STATUS'
    TOTALD    = 'FIRST CONTRACTOR + STATUS'

ENDCASE

SELECT CUTTKTH
=AFIELDS(laFileStru)
FOR lnCount=1 TO 3
  lcCount = STR(lnCount,1)
  lnPos   = ASCAN(laFileStru,'MFG_OPR'+lcCount,1)
  
  *-- If MFG_OPR1,MFG_OPR2,MFG_OPR3 fields removed from the file for any 
  *-- reason, create them in the temporary FILE
  IF lnPos = 0
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'MFG_OPR'+lcCount
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 6
    laFileStru[lnFileStru,4] = 0
  ELSE
    laFileStru[lnPos+2] = 6
  ENDIF

  lnPos   = ASCAN(laFileStru,'CONTR'+lcCount,1)
  IF lnPos = 0
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'CONTR'+lcCount
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 8
    laFileStru[lnFileStru,4] = 0
  ENDIF
ENDFOR

*B803940,1 AMH [Start] Added some fields to meet the new sort requirements.
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]

  laFileStru[lnFileStru+1,1] = 'ACCOUNT'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 5
  laFileStru[lnFileStru+1,4] = 0

  laFileStru[lnFileStru+2,1] = 'CUSTPO'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 10
  laFileStru[lnFileStru+2,4] = 0
*B803940,1 AMH [End]

Create Table (gcWorkDir+WORKFILE) FROM ARRAY laFileStru
INDEX ON &SORTFIELD TAG &WORKFILE
*B803940,1 AMH [Start] Opening the CutPick file in order to be able to get
*B803940,1 AMH         the account code and delivery date for a cut ticket.
=gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTPICK','SH')
*B803940,1 AMH [End]
SELECT CUTTKTH
SCAN FOR &lcRpExp
  llVend = .F.
  *--Get first 3 operations in sequence applied to the CUTTKT, and get their 
  *--contractors
  IF SEEK('M'+CUTTKTH.CUTTKT,'MFGOPRHD')
    SELECT (lcTmpOPH)
    ZAP  
    SELECT MFGOPRHD
    *-- Put all operations in this cuttkt cost sheet in the temprorary file
    SCAN WHILE cTktNo = CUTTKTH.CUTTKT
      SCATTER FIELDS CTKTNO,cOperSeq,cOprCode,cContCode MEMVAR
      INSERT INTO (lcTmpOPH) FROM MEMVAR
    ENDSCAN
    SELECT (lcTmpOPH)
    GO TOP
    IF !EOF()
      lnCount = 1
      STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, ;
                        m.MFG_OPR2, m.MFG_OPR3
      *-- Operations is sorted by sequence number, so get first 3 of them 
      *-- to put in temporary file of CUTTKTH
      SCAN
        lcCOUNT = STR(lnCount,1)
        *-- If contractor of any operation match the required contractor set the
        *-- flag to add this cuttkt to the report data.
        llVend  = IIF(!llVend .AND. !EMPTY(lcRpVend),lcRpVend = cContCode,llVend)
        IF lnCount <= 3 
          m.CONTR&lcCount   = cContCode
          m.MFG_OPR&lcCount = COPRCODE
        ENDIF
        lnCount = lnCount+1
      ENDSCAN
    ELSE
      STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, ;
                        m.MFG_OPR2, m.MFG_OPR3
    ENDIF
  ELSE
    STORE SPACE(0) TO m.CONTR1, m.CONTR2, m.CONTR3, m.MFG_OPR1, ;
                      m.MFG_OPR2, m.MFG_OPR3
  ENDIF
  IF EMPTY(lcRpVend) .OR. llVend
    SELECT CUTTKTH
    SCATTER MEMVAR MEMO FIELDS EXCEPT CONTR1,CONTR2,CONTR3,MFG_OPR1,MFG_OPR2,MFG_OPR3
    *B803940,1 AMH [Start] Initializing the following variables
    m.Account = SPACE(0)
    m.CustPo = SPACE(0)
    m.Del_Date = {  /  /  }
    =lfAdCustDt()
    *B803940,1 AMH [End]
    INSERT INTO (WORKFILE) FROM MEMVAR
  ENDIF
ENDSCAN
SET RELATION TO

SELECT (WORKFILE)
*B603160,1 AMM Check if there is data
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF
*B603160,1 AMM end

SET RELATION TO STYLE INTO STYLE

GOTO TOP
XCTLCUST=ACCOUNT+DTOC(DEL_DATE)
XCTLPATT=PATTERN

HBREAK=SPACE(1)
IF LEN(TRIM(BREAK)) <>0
  HBREAK = &BREAK
ENDIF

SET DEVICE TO PRINT

SELE &WORKFILE
DO CASE 						
  CASE lcRpNO  = 'A'
    DO MFG950A
    ROW=ROW+1
    *B803940,1 AMH [Start] Increase the replication.
    *@ ROW,00 SAY REPLICATE('*',187)
    @ ROW,00 SAY REPLICATE('*',225)
    *B803940,1 AMH [End]
    ROW = ROW+1
    *B803940,1 AMH [Start] Change the Posation of grand totals.
    @ ROW,00 SAY '* GRAND TOTAL *'
    *@ ROW,127 SAY XGTOT1 PICTURE '9999999'
    *@ ROW,135 SAY XGTOT2 PICTURE '9999999'
    *@ ROW,143 SAY XGTOT3 PICTURE '9999999'
    *@ ROW,151 SAY XGTOT6 PICTURE '9999999'
    *@ ROW,160 SAY XGTOT4 PICTURE '9999999'
    @ ROW,160 SAY XGTOT1 PICTURE '9999999'
    @ ROW,168 SAY XGTOT2 PICTURE '9999999'
    @ ROW,176 SAY XGTOT3 PICTURE '9999999'
    @ ROW,184 SAY XGTOT6 PICTURE '9999999'
    @ ROW,193 SAY XGTOT4 PICTURE '9999999'
    IF qCostPrv          && COSTING ACCESS
      *@ ROW,175 SAY XGTOT5 PICTURE '999999999.99'
      @ ROW,208 SAY XGTOT5 PICTURE '999999999.99'
    ENDIF
    *B803940,1 AMH [End]

  CASE lcRpNO  = 'B'
    DO MFG950B
    ROW=ROW+1
    *B803940,1 AMH [Start] Increase the replication.
    *@ ROW,00 SAY REPLICATE('*',187)
    @ ROW,00 SAY REPLICATE('*',225)
    *B803940,1 AMH [End]
    ROW = ROW+1
    @ ROW,00 SAY '* GRAND TOTA*'
    *B803940,1 AMH [Start] Change the Posation of grand totals.
    *@ ROW,102 SAY XGTOT1 PICTURE '9999999'
    *@ ROW,110 SAY XGTOT2 PICTURE '9999999'
    *@ ROW,118 SAY XGTOT3 PICTURE '9999999'
    *@ ROW,126 SAY XGTOT31 PICTURE '9999999'
    *@ ROW,134 SAY XGTOT4 PICTURE '9999999'
    @ ROW,131 SAY XGTOT1 PICTURE '9999999'
    @ ROW,139 SAY XGTOT2 PICTURE '9999999'
    @ ROW,147 SAY XGTOT3 PICTURE '9999999'
    @ ROW,155 SAY XGTOT31 PICTURE '9999999'
    @ ROW,163 SAY XGTOT4 PICTURE '9999999'
    IF qCostPrv        && COSTING ACCESS.
      *@ ROW,150 SAY XGTOT5 PICTURE '999999999.99'
      *@ ROW,174 SAY XGTOT6 PICTURE '999999999.99'  && GRAND TOTAL LABOR COST
      @ ROW,180 SAY XGTOT5 PICTURE '999999999.99'
      @ ROW,204 SAY XGTOT6 PICTURE '999999999.99'  && GRAND TOTAL LABOR COST
    ENDIF
    *B803940,1 AMH [End]

ENDCASE
ROW = ROW+1
*B803940,1 AMH [Start] Increase the replication.
*@ ROW,00 SAY REPLICATE('*',187)
@ ROW,00 SAY REPLICATE('*',225)
*B803940,1 AMH [End]

DO ENDREPORT       && END THE REPORT OR DISPLAY ON SCREEN

*B803940,1 AMH [Start] Close the cutpick file.
IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF
*B803940,1 AMH [End]

SET DEVICE TO SCREEN

*!*************************************************************
*! Name      : MFG950A
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/04/98
*! Purpose   : OPEN WORK IN PROCESS REEPORT FORMAT A
*!*************************************************************
*! Called from : MFOPENWP.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : DO MFG950A
*!*************************************************************
PROCEDURE MFG950A
SET DECIMALS TO 3

DO WHILE INKEY() <>32
  IF ROW >=51
    PAGENO = PAGENO+1
    IF BREAK <> ' '
      XREPORT='MFG950A'+'  '+'TOTAL :'+TOTALD
    ELSE
      XREPORT='MFG950A'
    ENDIF
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    
    *B803940,1 AMH [Start] Changing the layout by adding the account, custpo
    *B803940,1 AMH         and delivery date.
    *@ 005,000 SAY 'ISS DATE CTKT # DIV    STYLE               EST QTY VENDOR    WIDTH  TYPE   PATTERN #  ACT DATE CONTR1    CONTR2    COMPDATE S.     QTY QTY REC QTY DAM CAN QTY OPEN WIP  UNT CST TOTAL COST'
    @ 005,000 SAY 'ISS DATE CUSTOMER     CTKT # CUST. PO#  DEL DATE DIV    STYLE               EST QTY VENDOR    WIDTH  TYPE   PATTERN #  ACT DATE CONTR1    CONTR2    COMPDATE S.     QTY QTY REC QTY DAM CAN QTY OPEN WIP  UNT CST TOTAL COST'
    *@ 006,00 SAY REPLICATE('=',187)
    @ 006,00 SAY REPLICATE('=',225)
    *B803940,1 AMH [End] 
    
    ROW = 7
  ENDIF

  DOCUST = IIF(lnRpSrtCd=1 .AND. (ACCOUNT+DTOC(DEL_DATE)<>XCTLCUST),.T.,.F.)
   ******************************************************************
   ***  PRINT SUBTOTAL FOR DEL_dATE
   *****************************************************************
   IF DOCUST
     @ ROW,030 SAY '*** TOTALS FOR '
     @ ROW,046 SAY SUBSTR(XCTLCUST,6,LEN(XCTLCUST))

     @ ROW,160 SAY XTOTA1 PICTURE '9999999'
     @ ROW,168 SAY XTOTA2 PICTURE '9999999'
     @ ROW,176 SAY XTOTA3 PICTURE '9999999'
     @ ROW,184 SAY XTOTA6 PICTURE '9999999'
     @ ROW,193 SAY XTOTA4 PICTURE '9999999'

     IF qCostPrv           && COSTING ACCESS.
       @ ROW,208 SAY XTOTA5 PICTURE '999999999.99'
     ENDIF

     ROW=ROW+2
     XCTLCUST=ACCOUNT+DTOC(DEL_DATE)
     STORE 0 TO XTOTA1,XTOTA2,XTOTA3,XTOTA4,XTOTA5,XTOTA6
   ENDIF

   ******************************************************************
   ***  PRINT SUBTOTAL AT ACCOUNT
   *****************************************************************
   *B803940,1 AMH [Start]
   *IF lnRpSrtCd=3 .AND. PATTERN # XCTLPATT
   IF lnRpSrtCd=4 .AND. PATTERN # XCTLPATT
   *B803940,1 AMH [End]
     @ ROW,030 SAY '*** TOTALS FOR '
     @ ROW,046 SAY SUBSTR(XCTLPATT,11,5)
     SELE CUSTOMER
     XKEY='M'+SUBSTR(XCTLPATT,11,5)
     SEEK XKEY
     IF FOUND()
       @ ROW,053 SAY BTNAME+'   =======>'
     ELSE
       IF EMPTY(SUBSTR(XCTLPATT,11,5))
         @ ROW,053 SAY 'CUT TO STOCK   =======>'
       ENDIF
     ENDIF
     SELE &WORKFILE
     *B803940,1 AMH [Start] Change the Posation of subtotals.
     *@ ROW,127 SAY XTOT1 PICTURE '9999999'
     *@ ROW,135 SAY XTOT2 PICTURE '9999999'
     *@ ROW,143 SAY XTOT3 PICTURE '9999999'
     *@ ROW,151 SAY XTOT6 PICTURE '9999999'
     *@ ROW,160 SAY XTOT4 PICTURE '9999999'
     @ ROW,160 SAY XTOT1 PICTURE '9999999'
     @ ROW,168 SAY XTOT2 PICTURE '9999999'
     @ ROW,176 SAY XTOT3 PICTURE '9999999'
     @ ROW,184 SAY XTOT6 PICTURE '9999999'
     @ ROW,193 SAY XTOT4 PICTURE '9999999'
     IF qCostPrv         && COSTING ACCESS.
       *@ ROW,175 SAY XTOT5 PICTURE '999999999.99'
       @ ROW,208 SAY XTOT5 PICTURE '999999999.99'
     ENDIF
     *B803940,1 AMH [End]

     ROW=ROW+2
     XCTLPATT=PATTERN
     STORE 0 TO XTOTA1,XTOTA2,XTOTA3,XTOTA4,XTOTA5,XTOTA6
   ENDIF

   DO WHILE .NOT. EMPTY(BREAK)
     IF &BREAK = HBREAK
       EXIT
     ENDIF
     *B803940,1 AMH [Start] Increase the replication.
     *@ ROW,00 SAY REPLICATE('=',187)
     @ ROW,00 SAY REPLICATE('=',225)
     *B803940,1 AMH [End]
     ROW = ROW+ 1
     DO CASE
       *B803940,1 AMH [Start]
       *CASE lnRpSrtCd = 2
       CASE lnRpSrtCd = 1
         SELE CUSTOMER
         XKEY='M'+SUBSTR(HBREAK,1,5)
         SEEK XKEY
         IF FOUND()
           SELE &WORKFILE
           @ ROW,001 SAY '**** SUB TOTAL **** '+SUBSTR(HBREAK,1,5)
           @ ROW,027 SAY CUSTOMER->BTNAME
         ELSE
           SELE &WORKFILE
           IF EMPTY(SUBSTR(HBREAK,1,5))
             @ ROW,001 SAY '**** SUB TOTAL ****  CUT TO STOCK   '
           ELSE
             @ ROW,001 SAY '**** SUB TOTAL ****  '+HBREAK
           ENDIF
         ENDIF
       CASE lnRpSrtCd = 3
         @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,5)+'  '+SUBSTR(HBREAK,6,LEN(HBREAK))
       *CASE INLIST(lnRpSrtCd,2,4,6,7)
       CASE INLIST(lnRpSrtCd,2,5,6)
         @ ROW,001 SAY '**** SUB TOTAL ****  '+HBREAK
       *CASE lnRpSrtCd = 3
       CASE lnRpSrtCd = 4
         @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,10)
       *CASE lnRpSrtCd = 6
       CASE lnRpSrtCd = 7
         DO CASE
           CASE SUBSTR(HBREAK,6,1)='A'
             @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,5)+'  '+'ACTUAL'
           CASE SUBSTR(HBREAK,6,1)='O'
             @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,5)+'  '+'OPEN'
           CASE SUBSTR(HBREAK,6,1)='H'
             @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,5)+'  '+'HOLD'
         ENDCASE
       *B803940,1 AMH [End]
     ENDCASE

      *B803940,1 AMH [Start] Change the Posation of subtotals.
      *@ ROW,127 SAY XTOT1 PICTURE '9999999'
      *@ ROW,135 SAY XTOT2 PICTURE '9999999'
      *@ ROW,143 SAY XTOT3 PICTURE '9999999'
      *@ ROW,151 SAY XTOT6 PICTURE '9999999'
      *@ ROW,160 SAY XTOT4 PICTURE '9999999'
      @ ROW,160 SAY XTOT1 PICTURE '9999999'
      @ ROW,168 SAY XTOT2 PICTURE '9999999'
      @ ROW,176 SAY XTOT3 PICTURE '9999999'
      @ ROW,184 SAY XTOT6 PICTURE '9999999'
      @ ROW,193 SAY XTOT4 PICTURE '9999999'
      IF qCostPrv         && COSTING ACCESS.
        @ ROW,208 SAY XTOT5 PICTURE '999999999.99'
      ENDIF
      *B803940,1 AMH [End]
      ROW = ROW+1

      *B803940,1 AMH [Start] Increase the replication.
      *@ ROW,00 SAY REPLICATE('=',187)
      @ ROW,00 SAY REPLICATE('=',225)
      *B803940,1 AMH [End]
      ROW = ROW+1

      ******************** INITIALIZE SUBTOTALS ************
      STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT4,XTOT5,XTOT6
      STORE 0 TO XTOTA1,XTOTA2,XTOTA3,XTOTA4,XTOTA5,XTOTA6
      HBREAK = &BREAK
      *B803940,1 AMH [Start]
      *IF lnRpSrtCd = 4  .AND. llrpPBrk
      IF lnRpSrtCd = 5  .AND. llrpPBrk
      *B803940,1 AMH [End]
        ROW=99
      ENDIF
      EXIT
   ENDDO
   *--------------------- END SUBTOTALS ----------------------------*
   IF EOF()
     EXIT
   ENDIF

   IF ROW >=50
     ROW = 99
     LOOP
   ENDIF

   SELECT &WORKFILE
   **** GET VENDOR AND WIDTH FROM FIRST FABRIC ****
   XVENDOR=SPACE(1)
   XWIDTH=SPACE(1)
   XFAB=SPACE(1)

   SELE CTKTBOM
   IF SEEK('M'+&WORKFILE->CUTTKT+lcFabtype)
     XFAB = SUBSTR(ITEM,1,7)
   ENDIF

   SELE FABRIC
   IF SEEK( XFAB )
     XVENDOR=VENDOR
     XWIDTH=WIDTH
   ENDIF

   **** GET UNIT COST OF FABRIC+TRIM+MFG_OPERAION ALL BUT SEWING AND PRESSING
   **** WHICH ARE MFG_OPR2 & MFG_OPR3, OPEN WIP, QUANTITY OF STATUS
   SELE &WORKFILE
   XTOT_PCS=0
   DO CASE                      && QUANTITY OF STATUS
     CASE STATUS='A'
       XTOT_PCS=PCS_ACT
     OTHERWISE
       XTOT_PCS=PCS_BUD - PCS_CAN
   ENDCASE

   XOPN_WIP = PCS_OPN
   lnActCst = 0
   FOR lnI=1 TO 5
     IF INLIST(laCost[lnI,2],'F','T')
       lcZ = STR(lnI,1)
       lnActCst = lnActCst + nAct_Cost&lcZ
     ENDIF
   ENDFOR
   
   XUNT_CST = IIF(XTOT_PCS<>0,ROUND(lnActCst/XTOT_PCS,3),0)   && FABRIC + TRIM
   XUNT_CST=XUNT_CST                                   && FIRST MFG.OPER
   SELE &WORKFILE
   @ ROW,000 SAY ENTERED
   
   *B803940,1 AMH [Start] Adjusting the column due to adding new fields.
   *@ ROW,009 SAY CUTTKT
   *@ ROW,016 SAY CDIVISION
   *@ ROW,023 SAY STYLE
   *@ ROW,043 SAY PCS_BUD PICTURE '9999999' 
   *@ ROW,051 SAY XVENDOR
   *@ ROW,060 SAY XWIDTH
   *@ ROW,068 SAY CTKTTYPE
   *@ ROW,075 SAY PATTERN
   *@ ROW,086 SAY ACT_DATE
   *@ ROW,095 SAY CONTR1
   *@ ROW,105 SAY CONTR2
   *@ ROW,115 SAY COMPLETE
   *@ ROW,124 SAY STATUS
   *@ ROW,127 SAY XTOT_PCS PICTURE '9999999'
   *@ ROW,135 SAY PCS_REC PICTURE '9999999'
   *@ ROW,143 SAY PCS_DAM PICTURE '9999999'
   @ ROW,009 SAY IIF(EMPTY(ACCOUNT),'CUT TO STOCK',ACCOUNT)
   @ ROW,022 SAY CUTTKT
   @ ROW,029 SAY CUSTPO
   @ ROW,040 SAY DEL_DATE
   @ ROW,049 SAY CDIVISION
   @ ROW,056 SAY STYLE
   @ ROW,076 SAY PCS_BUD PICTURE '9999999' 
   @ ROW,084 SAY XVENDOR
   @ ROW,093 SAY XWIDTH
   @ ROW,101 SAY CTKTTYPE
   @ ROW,108 SAY PATTERN
   @ ROW,119 SAY ACT_DATE
   @ ROW,128 SAY CONTR1
   @ ROW,138 SAY CONTR2
   @ ROW,148 SAY COMPLETE
   @ ROW,157 SAY STATUS
   @ ROW,160 SAY XTOT_PCS PICTURE '9999999'
   @ ROW,168 SAY PCS_REC PICTURE '9999999'
   @ ROW,176 SAY PCS_DAM PICTURE '9999999'
   IF STATUS='A'
     *@ ROW,151 SAY (PCS_CAN - PCS_CANOLD) PICTURE '9999999'
     @ ROW,184 SAY (PCS_CAN - PCS_CANOLD) PICTURE '9999999'
   ELSE
     *@ ROW,151 SAY PCS_CAN PICTURE '9999999'
     @ ROW,184 SAY PCS_CAN PICTURE '9999999'
   ENDIF
   *@ ROW,160 SAY XOPN_WIP PICTURE '9999999'
   @ ROW,193 SAY XOPN_WIP PICTURE '9999999'

   IF qCostPrv          && COSTING ACCESS.
     *@ ROW,168 SAY XUNT_CST PICTURE '9999.999'
     *@ ROW,176 SAY (XOPN_WIP * XUNT_CST) PICTURE '99999999.99'
     @ ROW,201 SAY XUNT_CST PICTURE '9999.999'
     @ ROW,209 SAY (XOPN_WIP * XUNT_CST) PICTURE '99999999.99'
   ENDIF
   *B803940,1 AMH [End] 


   *** ADD THE SUBTOTALS AND GRAND TOTALS ***
   XTOT1=XTOT1+XTOT_PCS
   XTOTA1=XTOTA1+XTOT_PCS
   XGTOT1=XGTOT1+XTOT_PCS
   XTOT2=XTOT2+PCS_REC
   XTOTA2=XTOTA2+PCS_REC
   XGTOT2=XGTOT2+PCS_REC
   XTOT3=XTOT3+PCS_DAM
   XTOTA3=XTOTA3+PCS_DAM
   XGTOT3=XGTOT3+PCS_DAM
   XTOT4=XTOT4+XOPN_WIP
   XTOTA4=XTOTA4+XOPN_WIP
   XGTOT4=XGTOT4+XOPN_WIP
   XTOT5=XTOT5+(XOPN_WIP*XUNT_CST)
   XTOTA5=XTOTA5+(XOPN_WIP*XUNT_CST)
   XGTOT5=XGTOT5+(XOPN_WIP*XUNT_CST)
   XTOT6=XTOT6+PCS_CAN
   XTOTA6=XTOTA6+PCS_CAN
   XGTOT6=XGTOT6+PCS_CAN
   ROW=ROW+1

   SELECT &WORKFILE
   SKIP
ENDDO
SET DECIMALS TO 2
*********************************************************************
* PROG: MFG950B.PRG
* DESC: OPEN WORK IN PROCESS REEPORT FORMAT B
* DATE: 08/10/92
* MODI: 01/30/94 YMA
* NOTE: THIS REPORT IS XTRA WIDE.  IT SHOULD BE PRINTED ON WIDE PAPER
*       THIS PROSEDURE IS A MODI. COPY OF THE ORIGENAL FORMAT .
*       - MODIFICATION WAS ADDED BY Y.S.IBRAHIM
*         COST AND TOTAL LABOR COST
*       - DUPLICATE FROM REPORT PRINTING LOOP NAMED MFG950B & USED AS
*         PROSEDURES
*       - CALCULATION OF LABOR COST AND TOTAL LABOR COST
*       - NEW MODI.:REMOVING (VENDORS,WIDTH,TYPE,CONTR1,QTY.DAM,CAN.QTY)
*         FIELDS ADDED:STYLE DESC., LABOR COST AND EXTENTION
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Modifications :
*!    MFM 07/12/94
*!        1) Incresed vendor code to 8 characters and fixed the
*!           says of data in it's correct places.
*!********************************************************************
*!*************************************************************
*! Name      : MFG950A
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/04/98
*! Purpose   : OPEN WORK IN PROCESS REEPORT FORMAT A
*!*************************************************************
*! Called from : MFOPENWP.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : DO MFG950A
*!*************************************************************

PROCEDURE MFG950B
SET DECIMALS TO 3

DO WHILE INKEY() <>32
  IF ROW >=51
    PAGENO = PAGENO+1
    IF BREAK <> ' '
      XREPORT='MFG950B'+'  '+'TOTAL :'+TOTALD
    ELSE
      XREPORT='MFG950B'
    ENDIF
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH

    *B803940,1 AMH [Start] Changing the layout by adding the account, custpo
    *B803940,1 AMH         and delivery date.
    *@ 005,000 SAY 'ISS DATE CTKT # DIV.   STYLE              DESC.        EST Q. PATTERN #  ACT DATE CON.2    COMPDATE S.    QTY QTY REC QTY DAM CAN QTY OPN WIP  MA.COST  TOT.MA.COST LABOR COST TOT.LAB.COST'
    @ 005,000 SAY 'ISS DATE CUSTOMER CTKT # CUST. PO#  DEL DATE DIV.   STYLE              DESC.        EST Q. PATTERN #  ACT DATE CON.2    COMPDATE S.    QTY QTY REC QTY DAM CAN QTY OPN WIP  MA.COST  TOT.MA.COST LABOR COST TOT.LAB.COST'
    
    *@ 006,000 SAY REPLICATE('=',187)
    @ 006,000 SAY REPLICATE('=',225)
    *B803940,1 AMH [End] 
    
    ROW = 7
  ENDIF

  DOCUST=IIF(lnRpSrtCd=1 .AND. (ACCOUNT+DTOC(DEL_DATE)<>XCTLCUST),.T.,.F.)

   ******************************************************************
   ***  PRINT SUBTOTAL FOR DEL_dATE
   ******************************************************************
   
   IF DOCUST
   
     @ ROW,030 SAY '*** TOTALS FOR '
     @ ROW,046 SAY SUBSTR(XCTLCUST,6,LEN(XCTLCUST))

     SELE &WORKFILE
     @ ROW,131 SAY XTOTA1  PICTURE '9999999'
     @ ROW,139 SAY XTOTA2  PICTURE '9999999'
     @ ROW,147 SAY XTOTA3  PICTURE '9999999'
     @ ROW,155 SAY XTOTA31 PICTURE '9999999'
     @ ROW,163 SAY XTOTA4  PICTURE '9999999'

     *- ARH 03/27/94 Changed QPW(14) with qCostPrv
     IF qCostPrv           && COSTING ACCESS.
       @ ROW,180 SAY XTOTA5 PICTURE '999999999.99'
       @ ROW,204 SAY XTOTA6 PICTURE '999999999.99' && SUBTOTAL ON TOTAL LABOR COST,ADDED 08/10/92
     ENDIF

     ROW=ROW+2
     XCTLCUST=ACCOUNT+DTOC(DEL_DATE)
     STORE 0 TO XTOTA1,XTOTA2,XTOTA3,XTOTA31,XTOTA4,XTOTA5,XTOTA6
   ENDIF

   ******************************************************************
   ***  PRINT SUBTOTAL AT ACCOUNT
   *****************************************************************
   *B803940,1 AMH [Start]
   *IF lnRpSrtCd=3 .AND. PATTERN # XCTLPATT
   IF lnRpSrtCd=4 .AND. PATTERN # XCTLPATT
   *B803940,1 AMH [End]
     @ ROW,030 SAY '*** TOTALS FOR '
     @ ROW,046 SAY SUBSTR(XCTLPATT,11,5)
     SELE CUSTOMER
     XKEY='M'+SUBSTR(XCTLPATT,11,5)
     SEEK XKEY
     IF FOUND()
       @ ROW,053 SAY BTNAME+'   =======>'
     ELSE
       IF EMPTY(SUBSTR(XCTLPATT,11,5))
         @ ROW,053 SAY 'CUT TO STOCK   =======>'
       ENDIF
     ENDIF
     SELE &WORKFILE
     *B803940,1 AMH [Start] Change the Posation of subtotals.
     *@ ROW,102 SAY XTOTA1 PICTURE '9999999'
     *@ ROW,110 SAY XTOTA2 PICTURE '9999999'
     *@ ROW,118 SAY XTOTA3 PICTURE '9999999'
     *@ ROW,126 SAY XTOTA31 PICTURE '9999999'
     *@ ROW,134 SAY XTOTA4  PICTURE '9999999'
     @ ROW,131 SAY XTOTA1 PICTURE '9999999'
     @ ROW,139 SAY XTOTA2 PICTURE '9999999'
     @ ROW,147 SAY XTOTA3 PICTURE '9999999'
     @ ROW,155 SAY XTOTA31 PICTURE '9999999'
     @ ROW,163 SAY XTOTA4  PICTURE '9999999'
     IF qCostPrv
       *@ ROW,150 SAY XTOTA5 PICTURE '999999999.99'
       *@ ROW,174 SAY XTOTA6 PICTURE '999999999.99'
       @ ROW,180 SAY XTOTA5 PICTURE '999999999.99'
       @ ROW,204 SAY XTOTA6 PICTURE '999999999.99'
     ENDIF
     *B803940,1 AMH [End]
     ROW=ROW+2
     XCTLPATT=PATTERN
     STORE 0 TO XTOTA1,XTOTA2,XTOTA3,XTOTA31,XTOTA4,XTOTA5,XTOTA6
   ENDIF

   IF !EMPTY(BREAK) .AND. EVAL(BREAK) # HBREAK
     *B803940,1 AMH [Start] Increase the replication.
     *@ ROW,00 SAY REPLICATE('=',187)
     @ ROW,00 SAY REPLICATE('=',225)
     *B803940,1 AMH [End]
     ROW = ROW+ 1
     DO CASE
     *B803940,1 AMH [Start]
       CASE lnRpSrtCd = 1
         SELE CUSTOMER
         XKEY='M'+SUBSTR(HBREAK,1,5)
         SEEK XKEY
         IF FOUND()
           SELE &WORKFILE
           @ ROW,001 SAY '**** SUB TOTAL **** '+SUBSTR(HBREAK,1,5)
           @ ROW,027 SAY CUSTOMER->BTNAME
         ELSE
           SELE &WORKFILE
           IF EMPTY(SUBSTR(HBREAK,1,5))
             @ ROW,001 SAY '**** SUB TOTAL ****  CUT TO STOCK   '
           ELSE
             @ ROW,001 SAY '**** SUB TOTAL ****  '+HBREAK
           ENDIF
         ENDIF
       CASE lnRpSrtCd = 2
         @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,5)+'  '+SUBSTR(HBREAK,6,LEN(HBREAK))
       CASE INLIST(lnRpSrtCd,1,3,5,6)
         @ ROW,001 SAY '**** SUB TOTAL ****  '+HBREAK
       *CASE lnRpSrtCd = 3
       CASE lnRpSrtCd = 4
         @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,10)
       *CASE lnRpSrtCd = 6
       CASE lnRpSrtCd = 7
         DO CASE
          CASE SUBSTR(HBREAK,6,1)='A'
            @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,5)+'  '+'ACTUAL'
          CASE SUBSTR(HBREAK,6,1)='O'
            @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,5)+'  '+'OPEN'
          CASE SUBSTR(HBREAK,6,1)='H'
            @ ROW,001 SAY '**** SUB TOTAL ****  '+SUBSTR(HBREAK,1,5)+'  '+'HOLD'
        ENDCASE
     *B803940,1 AMH [End]
     ENDCASE
     **** PRINT SUBTOTALS *****
     *B803940,1 AMH [Start] Change the Posation of subtotals.
     *@ ROW,102 SAY XTOTA1 PICTURE '9999999'
     *@ ROW,110 SAY XTOTA2 PICTURE '9999999'
     *@ ROW,118 SAY XTOTA3 PICTURE '9999999'
     *@ ROW,126 SAY XTOTA31 PICTURE '9999999'
     *@ ROW,134 SAY XTOTA4  PICTURE '9999999'
     @ ROW,131 SAY XTOT1 PICTURE '9999999'
     @ ROW,139 SAY XTOT2 PICTURE '9999999'
     @ ROW,147 SAY XTOT3 PICTURE '9999999'
     @ ROW,155 SAY XTOT31 PICTURE '9999999'
     @ ROW,163 SAY XTOT4  PICTURE '9999999'
     IF qCostPrv
       *@ ROW,150 SAY XTOTA5 PICTURE '999999999.99'
       *@ ROW,174 SAY XTOTA6 PICTURE '999999999.99'
       @ ROW,180 SAY XTOT5 PICTURE '999999999.99'
       @ ROW,204 SAY XTOT6 PICTURE '999999999.99'
     ENDIF
     *B803940,1 AMH [End]

     ROW = ROW+1
     *B803940,1 AMH [Start] Increase the replication.
     *@ ROW,00 SAY REPLICATE('=',187)
     @ ROW,00 SAY REPLICATE('=',225)
     *B803940,1 AMH [End]
     ROW = ROW+1

     ******************** INITIALIZE SUBTOTALS ************
     STORE 0 TO XTOT1,XTOT2,XTOT3,XTOT31,XTOT4,XTOT5,XTOT6
     STORE 0 TO XTOTA1,XTOTA2,XTOTA3,XTOTA31,XTOTA4,XTOTA5,XTOTA6
     HBREAK = &BREAK
     *B803940,1 AMH [Start]
     *IF lnRpSrtCd = 4  .AND. llrpPBrk
     IF lnRpSrtCd = 5  .AND. llrpPBrk
     *B803940,1 AMH [End]
       ROW=99
     ENDIF
   ENDIF
   *--------------------- END SUBTOTALS ----------------------------*

   IF EOF()
      EXIT
   ENDIF

   IF ROW >=50
      ROW = 99
      LOOP
   ENDIF

   SELE &WORKFILE
   XTOT_PCS=0
   DO CASE                      && QUANTITY OF STATUS
     CASE STATUS='A'
       XTOT_PCS=PCS_ACT
     OTHERWISE
       XTOT_PCS=PCS_BUD - PCS_CAN
   ENDCASE

   XOPN_WIP = PCS_OPN
   lnActCst = 0
   FOR lnI=1 TO 5
     IF INLIST(laCost[lnI,2],'F','T')
       lcZ = STR(lnI,1)
       lnActCst = lnActCst + nAct_Cost&lcZ
     ENDIF
   ENDFOR

   XUNT_CST = IIF(XTOT_PCS<>0,ROUND(lnActCst/XTOT_PCS,3),0)   && FABRIC + TRIM
   XUNT_CST=XUNT_CST                                  && FIRST MFG.OPER
   STORE 0 TO XLAB_CST,XTOT_LAB_CST
   SELE CTKTBOM

   SEEK 'M'+&WORKFILE->CUTTKT
   LOCATE REST WHILE cIMTyp+Cuttkt='M'+&WORKFILE->CUTTKT FOR MFGCode=xMFG
   IF FOUND()
     XLAB_CST    = UNTCOST  * UNTQTY
     XTOT_LAB_CST= XLAB_CST * PIECES
   ELSE
     SELE BOM
     SEEK &WORKFILE->STYLE
     LOCATE REST WHILE STYLE=&WORKFILE->STYLE FOR MfgCode=XMFG
     IF FOUND()
       XLAB_CST    = UNTCOST  * nBomTOTQTY
       XTOT_LAB_COS= XLAB_CST * XTOT_PCS
     ENDIF
   ENDIF

   SELE &WORKFILE
   @ ROW,000 SAY ENTERED

   *B803940,1 AMH [Start] Adjusting the layout according to adding new fields.
   *@ ROW,009 SAY CUTTKT
   *@ ROW,016 SAY STYLE.CDIVISION
   *@ ROW,023 SAY STYLE  
   *@ ROW,041 SAY SUBSTR(DESC,1,12)
   *@ ROW,054 SAY PCS_BUD PICTURE '9999999'
   *@ ROW,062 SAY PATTERN
   *@ ROW,073 SAY ACT_DATE
   *@ ROW,082 SAY CONTR2
   *@ ROW,091 SAY COMPLETE
   *@ ROW,100 SAY STATUS
   *@ ROW,102 SAY XTOT_PCS PICTURE '9999999'
   *@ ROW,110 SAY PCS_REC  PICTURE '9999999'
   *@ ROW,118 SAY PCS_DAM  PICTURE '9999999'
   *@ ROW,126 SAY PCS_CAN  PICTURE '9999999'
   *@ ROW,134 SAY XOPN_WIP PICTURE '9999999'
   @ ROW,009 SAY IIF(EMPTY(ACCOUNT),'STOCK',ACCOUNT)
   @ ROW,018 SAY CUTTKT
   @ ROW,025 SAY CUSTPO
   @ ROW,036 SAY DEL_DATE
   @ ROW,045 SAY STYLE.CDIVISION
   @ ROW,052 SAY STYLE  
   @ ROW,070 SAY SUBSTR(DESC,1,12)
   @ ROW,083 SAY PCS_BUD PICTURE '9999999'
   @ ROW,091 SAY PATTERN
   @ ROW,102 SAY ACT_DATE
   @ ROW,111 SAY CONTR2
   @ ROW,120 SAY COMPLETE
   @ ROW,129 SAY STATUS
   @ ROW,131 SAY XTOT_PCS PICTURE '9999999'
   @ ROW,139 SAY PCS_REC  PICTURE '9999999'
   @ ROW,147 SAY PCS_DAM  PICTURE '9999999'
   @ ROW,155 SAY PCS_CAN  PICTURE '9999999'
   @ ROW,163 SAY XOPN_WIP PICTURE '9999999'

   IF qCostPrv               && COSTING ACCESS.
     *@ ROW,142 SAY XUNT_CST              PICTURE '9999.99'   &&MATERIAL COST
     *@ ROW,150 SAY (XOPN_WIP * XUNT_CST) PICTURE '99999999.99'
     *@ ROW,163 SAY XLAB_CST              PICTURE '99999999.99'        && LABOR COST/PEC
     *@ ROW,175 SAY XTOT_LAB_CST          PICTURE '999999999.99'        && TOTAL LABOR COST
     @ ROW,173 SAY XUNT_CST              PICTURE '9999.99'   &&MATERIAL COST
     @ ROW,181 SAY (XOPN_WIP * XUNT_CST) PICTURE '99999999.99'
     @ ROW,192 SAY XLAB_CST              PICTURE '99999999.99'        && LABOR COST/PEC
     @ ROW,204 SAY XTOT_LAB_CST          PICTURE '999999999.99'        && TOTAL LABOR COST
   ENDIF
   *B803940,1 AMH [End] 


   XTOT1=XTOT1+XTOT_PCS
   XTOTA1=XTOTA1+XTOT_PCS
   XGTOT1=XGTOT1+XTOT_PCS
   XTOT2=XTOT2+PCS_REC
   XTOTA2=XTOTA2+PCS_REC
   XGTOT2=XGTOT2+PCS_REC
   XTOT3=XTOT3+PCS_DAM
   XTOTA3=XTOTA3+PCS_DAM
   XGTOT3=XGTOT3+PCS_DAM
   XTOT31=XTOT31+PCS_CAN
   XTOTA31=XTOTA31+PCS_CAN
   XGTOT31=XGTOT31+PCS_CAN
   XTOT4=XTOT4+XOPN_WIP
   XTOTA4=XTOTA4+XOPN_WIP
   XGTOT4=XGTOT4+XOPN_WIP
   XTOT5=XTOT5+(XOPN_WIP*XUNT_CST)
   XTOTA5=XTOTA5+(XOPN_WIP*XUNT_CST)
   XGTOT5=XGTOT5+(XOPN_WIP*XUNT_CST)
   XTOT6 =XTOT6 +XTOT_LAB_CST                    && TOTAL LABOR COST
   XTOTA6=XTOTA6+XTOT_LAB_CST
   XGTOT6=XGTOT6+XTOT_LAB_CST
   ROW=ROW+1

   SELECT &WORKFILE
   SKIP
ENDDO
SET DECIMALS TO 2

*!*************************************************************
*! Name      : lfvFormAB
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/04/98
*! Purpose   : to enable or disable the MFG operation due to the form 
*!             (A or B)
*!*************************************************************
*! Called from : MFOPENWP.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : =lfvFormAB()
*!*************************************************************
FUNCTION lfvFormAB
*-- Get the position of the MFG operation in the array to enable or 
*-- disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'XMFG'),1)
laOGObjCnt[lnPos] = lcRpNO = 'B'
XMFG = IIF(lcRpNO = 'A','',XMFG)
= lfOGShowGet('XMFG')

*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/04/98
*! Purpose   : Valid function of the status button
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : =lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ''
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'Closed'  ,'S',;
                            IIF(laRpTarget[lnI] = 'Actual'  ,'A',''))))

ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 02/04/99
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwOGWhen
DECLARE laRpSource[4],laRpTarget[1]

lnStatus = lcRpStatus
STORE 'Open'     TO laRpSource[1]
STORE 'Hold'     TO laRpSource[2]
STORE 'Closed'   TO laRpSource[3]
STORE 'Actual'   TO laRpSource[4]

*B603160,1 AMM Initialize
*lcRpStatus = ''
STORE SPACE(0) TO lcRpStatus, laRpTarget
*B603160,1 AMM end

SET ORDER TO TAG CSTYLE IN STYLE
SET ORDER TO SEGVAL IN ICSEGVAL
*-- Get the position of the MFG operation in the array to enable or 
*-- disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'XMFG'),1)
laOGObjCnt[lnPos] = lcRpNO = 'B'
= lfOGShowGet('XMFG')

lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrpPBrk'),1)
*B803940,1 AMH [Start]
*laOGObjCnt[lnPos] = lnRpSrtCd = 4
laOGObjCnt[lnPos] = lnRpSrtCd = 5
*B803940,1 AMH [End]
= lfOGShowGet('llrpPBrk')


*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 02/04/99
*! Purpose   : Get title of the cuttkt to be displayed 
*!*************************************************************
*! Called from : The option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! Example     : = lfGetTit()
*!*************************************************************
FUNCTION lfGetTit

PRIVATE lcTit
lcTit   = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))  
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
RETURN lcTit

*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 02/04/99
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
FUNCTION lfAdjSeg

STORE 0 TO lnFPos, lnDPos, lnZPos, lnGPos, lnCPos, lnOPos, lnTPos, ;
           lnQPos, lnSPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.

DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
FOR lnC = 1 TO lnMajSeg
  *-- If the style major consists of one segment, don't display it, 
  *-- display the style major instead (style major will browse from the 
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1 
    EXIT
  ENDIF
  DO CASE
    CASE laMajSeg[lnC,1] = 'F'
      *-- If there are more than one "FREE" segment , get first one only
      lnFPos = IIF(lnFPos = 0, lnC , lnFPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Style'
      ENDIF
    CASE laMajSeg[lnC,1] = 'C'
      lnCPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Color'
      ENDIF
    CASE laMajSeg[lnC,1] = 'O'
      *-- If there are more than one "OTHER" segment , get first one only
      lnOPos = IIF(lnOPos = 0, lnC , lnOPos)
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Other'
      ENDIF
    CASE laMajSeg[lnC,1] = 'T'
      lnTPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Make'
      ENDIF
    CASE laMajSeg[lnC,1] = 'Q'
      lnQPos = lnC
      IF EMPTY(laMajSeg[lnC,5])
        laMajSeg[lnC,5] = 'Quality'
      ENDIF
  ENDCASE

ENDFOR

*!*************************************************************
*! Name      : lfvSrtBy
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 02/04/98
*! Purpose   : If sorting by 'Second cont.+Completion date' Enable 
*!             "Page break by cont." setting
*!*************************************************************
*! Called from : MFOPENWP.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : =lfvSrtBy()
*!*************************************************************

FUNCTION lfvSrtBy

lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrpPBrk'),1)
*B803940,1 AMH [Start]
*laOGObjCnt[lnPos] = lnRpSrtCd = 4
laOGObjCnt[lnPos] = lnRpSrtCd = 5
*B803940,1 AMH [End]
= lfOGShowGet('llrpPBrk')


*!*************************************************************
*! Name      : lfvVend
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 02/04/99
*! Purpose   : Vaildate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************

FUNCTION lfvVend

SELECT APVENDOR
SET ORDER TO TAG VenCode 
IF !EMPTY(lcRPVend) .AND. ;
   ('?' $ lcRPVend .OR. !SEEK(lcRPVend , 'APVENDOR'))
  =gfApVnBrow(@lcRPVend)
ENDIF


*!*************************************************************
*! Name      : lfSRVFab
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 08/16/98
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
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
    llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
  OTHERWISE      && Valid code
    lcAlias = ALIAS()
    SELECT STYLE
    LOCATE FOR STYLE.Fabric = Fabric.Fabric
    llHaveSty = FOUND()
    *-- If no styles found for this fabric
    IF !llHaveSty
      *-- the following message is
      *-- No styles in fabric group XXX .
      *--           <Ok>
      = gfModalGen("TRM32055B36000","Dialog",Fabric.Fabric)
    ENDIF
    SELECT (lcAlias)
    RETURN llHaveSty    && Record selected only if fabric found in style file.
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 05/27/98
*! Purpose   : sum a specific field for the current fabric in fabric file
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

******************************************************************************
* PROG: RPT_HDR
* DESC: PRINTS THE HEADER ON THE REPORTS (XTRA WIDE PAPER)
* DATE: 01/7/2001
* Developer : Ahmed Maher (AMH)
* PARA: 1 PROGRAM
*       2 TITLE    ADDITIONAL TITLE (XRPTNAME)
*       3 TYPE     'N' -> NARROW   'W'-> WIDE    'XW'-> XTRA WIDE
* NOTE: THE ACTUAL REPORT CAN START ON LINE 5
*       R_TITLE => THE REPORT TITLE (STANDARD)
*       PAGENO=PAGE NUMBER
******************************************************************************
*B803940
PROCEDURE RPT_HDR
PARAMETER XPROG,XRPTNAME,XTYPE

PRIVATE ALL LIKE X*
XRPTNAME = TRIM(XRPTNAME)
R_TITLE  = TRIM(R_TITLE)
X1 = ((203 - (LEN(TRIM(QCOMPANY))))/2)

X2 = ((203 - (LEN( R_TITLE  )))/2)
X3 = ((203 - (LEN( XRPTNAME )))/2)

@ 01,000 SAY XPROG
@ 01,X1  SAY QCOMPANY
@ 01,190 SAY DATE()
@ 01,199 SAY '~'
@ 02,000 SAY TIME()
@ 02,X2  SAY R_TITLE 
@ 02,190 SAY 'PAGE#'
@ 02,196 SAY STR(PAGENO,4)
@ 03,X3  SAY XRPTNAME
@ 04,00 SAY REPLICATE('*',225)

RETURN

*!*************************************************************
*! Name      : lfAdCustDt
*! Developer : Ahmed Maher (AMH)
*! Date      : 01/07/2001
*! Purpose   : Add Customer , Cust. PO# and Del date to WORKFILE
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : =lfAdCustDt()
*!*************************************************************
*B803940
FUNCTION lfAdCustDt

PRIVATE lnAlias, llMultiAct
lnAlias = SELECT(0)
StORE .F. TO llMultiAct

IF SEEK('1'+CUTTKT,'CUTPICK')
  SELECT CUTPICK
  SCAN REST WHILE trancd+ctktno+style = '1'+CUTTKTH.CUTTKT
    IF SEEK('O'+ORDER,'ORDHDR')
      IF !(m.Account==ORDHDR.ACCOUNT)
        IF EMPTY(m.Account)
          m.Account = ORDHDR.ACCOUNT
          m.CustPo = ORDHDR.CUSTPO
          m.Del_Date = ORDHDR.COMPLETE
        ELSE
          llMultiAct = .T.
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ENDIF
IF llMultiAct
  *B605227,1 KHM 12/23/2001 (Begin) Printing the word MULTI instead of space.
  *m.Account = SPACE(0)
  *m.CustPo = SPACE(0)
  m.Account = 'MULTI'
  m.CustPo = 'MULTI'
  *B605227,1 KHM 12/23/2001 (End)
  
  m.Del_Date = {  /  /  }
  *B605227,1 KHM 12/23/2001 (Begin) Commented out
  *m.Cuttkt = 'MULTI'
  *B605227,1 KHM 12/23/2001 (End)
ENDIF
SELECT (lnAlias)
*-- end of lfAdCustDt.