**********************************************************************
*: Program file  : SOXES700.PRG (C# 102163)
*: Program desc. : OPEN ORDER SUMMARY REPORTS FROM HEADER FILE (FOR XES)
*: System        : Aria Apparel System (A27)
*:Developer      : MOHAMED SHOKRY _ (MHM)
*:********************************************************************
*: Calls : FUNCTIONS  : None.
*:         PROCEDURE  : ENDREPORT,RPT_HDR.
*:********************************************************************
*: Passed Parameters  : None
*:********************************************************************
*: MODIFICATION :
*: C102163,4 MHM 26/02/2001 delete currancy becuse customer use single currancy
*:********************************************************************
*--- This Custom report is copy from our standard Order Header report
*--- With new sort by cancel reason in case of status X and report format
*--- short.

*C102163,4 [start]
IF llRpMulti
  lcRpExp =lcRpExp + ".AND. Multi = 'Y'"
ENDIF

IF llRpBulk
  lcRpExp =lcRpExp + ".AND. Bulk = 'Y'"
ENDIF

IF !EMPTY(lcRpStatus)
  lcRpExp =lcRpExp + ".AND. STATUS $ lcRpStatus"
ENDIF
*C102163,4 [end]

IF llRpReord
  lcRpExp =lcRpExp + ".AND. cReOrder = 'Y'"
ENDIF
R_WIDTH  = 'W'    && STANDARD REPORT IS 'WIDE'
R_TITLE  = 'ORDER HEADER REPORT'
XREPORT  = 'XES700'
WORKFILE = gfTempName()
CUSTFILE = gfTempName()
CHOICE   = lcRpSortBy
SORTFIELD = ' '
BREAK     = ' '
DO CASE
	* SUMMARY BY ORDER#
  CASE CHOICE='O'
    SORTFIELD = 'ORDER'
    XSORT     = 'O'
  * SUMMARY BY CUSTOMER ACCOUNT
  CASE CHOICE='A'
    SORTFIELD = 'ACCOUNT+ORDER'
    BREAK = 'ACCOUNT'
    XSORT     = 'A'
  * SUMMARY BY CUSTOMER PO#
  CASE CHOICE='C'
    STORE 'ACCOUNT+CUSTPO+ORDER' TO SORTFIELD
    BREAK = 'ACCOUNT+CUSTPO'
    XSORT = 'C'
  * SUMMARY BY PRIOIRTY
  CASE CHOICE='P'
    SORTFIELD = 'DTOS(COMPLETE)+PRIORITY+ACCOUNT+ORDER'
    BREAK = 'DTOS(COMPLETE)+PRIORITY'
    XSORT = 'P'
  * SUMMARY BY REP
  CASE CHOICE='S'
    STORE 'REP1+ORDER' TO SORTFIELD
    BREAK = 'REP1'
    XSORT = 'S'
  * SUMMARY BY CANCELATON REASON
  CASE CHOICE='N'
    STORE 'CcancReson+ORDER' TO SORTFIELD
    BREAK = 'CcancReson'
    XSORT = 'S'
  ENDCASE

*---------------------------------------------------------
* [3] SELECT REPORT FILE & INITIALIZE MEMORY
*---------------------------------------------------------
SELE CUSTOMER
COPY STRU TO &gcWorkDir.&CUSTFILE FIELDS ACCOUNT,SALESREP
SELECT 0
=gfOpenFile(gcWorkDir+CUSTFILE,'','EX')
IF XSORT='S'        && WHEN SORTING BY SALESREP
  CUSTFL1=gfTempName()
  INDEX ON SALESREP+ACCOUNT TAG &CUSTFILE
  INDEX ON ACCOUNT TAG &CUSTFL1
  SET ORDER TO TAG &CUSTFILE
ELSE
  INDEX ON ACCOUNT TAG &CUSTFILE
ENDIF
******************************************************
SELECT ORDHDR
SET FILTER TO
WAIT WINDOW 'Selecting records for report ...' NOWAIT
LOCATE ALL FOR &lcRpExp
IF !FOUND()
  *--No records to display.
  = gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF
COPY REST TO &gcWorkDir.&WORKFILE FOR &lcRpExp
SELECT ORDHDR
SET RELATION TO
=gfOpenFile(gcWorkDir+WORKFILE,'','EX')
*******END TAK****************
* SORT TO WORKFILE INDEX
IF SORTFIELD<>' '
   Z = LTRIM(STR(RECCOUNT(),7))
   WAIT WINDOW 'SORTING &Z RECORDS FOR ORDER SUMMARY REPORT ...' NOWAIT
   INDEX ON &SORTFIELD TAG &WORKFILE
   SET ORDER TO TAG &WORKFILE
ENDIF
SET RELATION TO IIF(EMPTY(Store),'M','S') + Account + Store INTO CUSTOMER
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*XES700                                  ORDER SUMMARY - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*
*ORDER  ACCOUNT# SHIP-TO NAME.................  BUYER                          TERMS............   ENTERED   REP1      BOOKED
*STATUS STORE  # SHIP-TO ADDRESS............... PHONE.......................   SHIPVIA..........   START     COMM1     SHIPPED
*SEASON P/O    # 123456789012345678901234567890 COMMENTS....................   SPECIAL INSTRUC..   COMPLETE  REP2      CANCELLED
*DIVIS. DEPART.  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX (123)123-123                   REGION...........   MM/DD/YY  COMM2     OPEN
*PRIOR.  2345    XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 123456789012345678901234567890 XX 123456789012345  MM/DD/YY  123    1234567 12345678.99
*       12345678 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 123456789012345678901234567890 XX XXXXXXXXXXXXXXX            12.99  1234567 12345678.99
*       APPROVE: 12345 1234567 1234567890
*
*                    ......BOOKED.......      ......SHIPPED......      .....CANCELED......      .......OPEN........
* *SUB TOTALS*       1234567 12345678.99      1234567 12345678.99      1234567 12345678.99      1234567 12345678.99
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

DIMENSION XTOTAL(2,8)
XTOTAL = 0.00
PAGENO = 0
ROW    = 99
XTIME  = TIME()
XGRAND_CNT=0
XREP_CNT  =0
SELECT &WORKFILE
LOCATE
IF LEN(TRIM(BREAK)) <>0
   HBREAK = &BREAK
ENDIF
WAIT WINDOW 'Report printing - <SPACE BAR> to abort' NOWAIT
SET DEVICE TO PRINT
SELECT &WORKFILE
SET RELATION TO 'O' + Order INTO ORDHDR
*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------
*-- Changed the terminating key in case of batch mode
*-- to be CTRL+X instead of SPACE BAR.
DO WHILE INKEY() <> 32
   IF ROW >=53
      IF BREAK <> ' '
        XTITLE1 = ALLTRIM(lcRpOpTitl)+' TOTAL: ' + BREAK
      ELSE
        XTITLE1 = lcRpOpTitl
      ENDIF
      PAGENO = PAGENO+1
      DO RPT_HDR WITH XREPORT,XTITLE1,R_WIDTH
      IF lcRpFormat ='L'
        @ 05,000 SAY 'ORDER  ACCOUNT# SHIP-TO NAME.................  BUYER                          TERMS............   REP1   ENTERED     BOOKED'
        @ 06,000 SAY 'STATUS STORE  # SHIP-TO ADDRESS............... PHONE.......................   SHIPVIA..........   COMM1  START       SHIPPED'
        @ 07,000 SAY 'SEASON P/O                                     COMMENTS....................   SPECIAL INSTRUC..   REP2   COMPLETE    CANCELLED'
        @ 08,000 SAY 'DIVIS. DEPART.                                                                REGION...........   COMM2              OPEN'
        @ 09,000 SAY 'PRIOR.                                         CANCELLATION REASON'
        ROW = 10
       ELSE
        @ 05,00 SAY 'ORDER    S SE ACCT# NAME........ CUST.P/O.. COMPLETE ......BOOKED....... ......SHIPPED...... .....CANCELED...... .......OPEN........'
        ROW = 6
      ENDIF
   ENDIF
   DO WHILE LEN(TRIM(BREAK)) <>0
      IF &BREAK = HBREAK
         EXIT
      ENDIF
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW+1
      IF lcRpFormat ='L'
        IF XSORT='S'
          @ ROW,000 SAY XREP_CNT PICTURE '99999'
          @ ROW,006 SAY IIF(XREP_CNT<>1,'Accounts','Account')
          XREP_CNT=0
        ENDIF
        @ ROW,020 SAY '......BOOKED.......      ......SHIPPED......      .....CANCELED......      .......OPEN........'
        ROW = ROW+1
        IF XSORT='P'
          @ ROW,000 SAY '* SUB TOTAL * '   && BECAUSE NOT ENOUGH SPACE
        ELSE
          @ ROW,000 SAY '* SUB TOTAL * '+HBREAK
        ENDIF
        @ ROW,020 SAY XTOTAL(1,1)  PICTURE '9999999'
        @ ROW,028 SAY XTOTAL(1,2)  PICTURE '99999999.99'
        @ ROW,045 SAY XTOTAL(1,3)  PICTURE '9999999'
        @ ROW,053 SAY XTOTAL(1,4)  PICTURE '99999999.99'
        @ ROW,070 SAY XTOTAL(1,5)  PICTURE '9999999'
        @ ROW,078 SAY XTOTAL(1,6)  PICTURE '99999999.99'
        @ ROW,095 SAY XTOTAL(1,7)  PICTURE '9999999'
        @ ROW,103 SAY XTOTAL(1,8)  PICTURE '99999999.99'
      ELSE
        @ ROW,000 SAY '* SUB TOTAL *'+HBREAK
        IF XSORT='S'
          @ ROW,025 SAY XREP_CNT PICTURE '99999'
          @ ROW,031 SAY IIF(XREP_CNT<>1,'Accounts','Account')
          XREP_CNT=0
        ENDIF
        @ ROW,053 SAY XTOTAL(1,1)  PICTURE '9999999'
        @ ROW,061 SAY XTOTAL(1,2)  PICTURE '99999999.99'
        @ ROW,073 SAY XTOTAL(1,3)  PICTURE '9999999'
        @ ROW,081 SAY XTOTAL(1,4)  PICTURE '99999999.99'
        @ ROW,093 SAY XTOTAL(1,5)  PICTURE '9999999'
        @ ROW,101 SAY XTOTAL(1,6)  PICTURE '99999999.99'
        @ ROW,113 SAY XTOTAL(1,7)  PICTURE '9999999'
        @ ROW,121 SAY XTOTAL(1,8)  PICTURE '99999999.99'
      ENDIF
      ROW = ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW+1
      X = 1
      DO WHILE X<=8
         XTOTAL(2,X) = XTOTAL(2,X) + XTOTAL(1,X)
         XTOTAL(1,X) = 0.00
         X = X+1
      ENDDO
      HBREAK = &BREAK
      EXIT
   ENDDO
   *--------------------- END SUBTOTALS ----------------------------
   IF EOF()
      EXIT
   ENDIF
   IF ROW >=53
      ROW = 99
      LOOP
   ENDIF
   SELECT &WORKFILE
   XORDER    = ORDER
   XTERMS    = CTERMCODE
   XSHIPVIA  = SHIPVIA
   XSPCINST  = SPCINST
   XSEASON   = SEASON
   XDIVISION = CDIVISION
   XREGION   = CUSTOMER->REGION
   XSTADDR22  = CUSTOMER->CADDRESS22
   XSTADDR32  = TRIM(CUSTOMER->CADDRESS32) + ' ' + LEFT(CUSTOMER->CADDRESS42,3);
              + ' ' +LEFT(TRIM(CUSTOMER->CADDRESS52),10)
   IF LEN(TRIM(XSTADDR22)) = 0
      XSTADDR22 = XSTADDR32
      XSTADDR32 = ' '
   ENDIF
   Z = ' '
   SELECT CODES
   PTERMS    = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
   PSHIPVIA  = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
   PSPCINST  = gfCodDes(ORDHDR.SPCINST , 'SPCINST'  )
   PSEASON   = gfCodDes(ORDHDR.SEASON , 'SEASON'  )
   PDIVISION = gfCodDes(ORDHDR.CDIVISION , 'CDIVISION'  )
   PREGION   = gfCodDes(CUSTOMER.REGION , 'REGION'  )
   lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
   SELECT &WORKFILE
   IF lcRpFormat ='L'
     @ ROW,000 SAY REPLICATE('-',132)
     ROW = ROW+1
     @ ROW,000 SAY ORDER
     @ ROW,007 SAY ACCOUNT
     @ ROW,016 SAY CUSTOMER->STNAME
     @ ROW,047 SAY BUYER
     @ ROW,078 SAY CTERMCODE + LEFT(PTERMS,13)
     @ ROW,098 SAY REP1
     @ ROW,105 SAY ENTERED
     @ ROW,114 SAY BOOK         PICTURE '9999999'
     @ ROW,121 SAY BOOKAMT      PICTURE '99999999.99'
     ROW = ROW+1
     * LINE #2
     DO CASE
       CASE STATUS = 'O'
         @ ROW,00 SAY 'OPEN'
       CASE STATUS = 'C'
         @ ROW,00 SAY 'COMPLETE'
       CASE STATUS = 'H'
         @ ROW,00 SAY 'HOLD'
       CASE STATUS = 'X'
         @ ROW,00 SAY 'CANCEL'
       CASE STATUS = 'B'
         @ ROW,00 SAY 'BID'
     ENDCASE
     @ ROW,007 SAY IIF(llRpMulti,'*MULTI*',STORE)
     @ ROW,016 SAY LEFT(CUSTOMER->CADDRESS12,30)
     @ ROW,047 SAY PHONE  PICTURE lcPhonPict
     @ ROW,078 SAY SHIPVIA + LEFT(PSHIPVIA,13)
     @ ROW,098 SAY COMM1 PICTURE '99.99'
     @ ROW,105 SAY START
     @ ROW,114 SAY SHIP         PICTURE '9999999'
     @ ROW,121 SAY SHIPAMT      PICTURE '99999999.99'
     ROW = ROW+1
     *LINE #3
     @ ROW,000 SAY SUBSTR(PSEASON,1,6)
     @ ROW,007 SAY IIF(MULTIPO,'*M-PO#*',SUBSTR(CUSTPO,1,8))  
     @ ROW,016 SAY LEFT(XSTADDR22,30)
     @ ROW,047 SAY NOTE1
     @ ROW,078 SAY SPCINST + LEFT(PSPCINST,13)
     @ ROW,098 SAY REP2
     @ ROW,105 SAY COMPLETE
     @ ROW,114 SAY CANCEL       PICTURE '9999999'
     @ ROW,121 SAY CANCELAMT    PICTURE '99999999.99'
     ROW = ROW+1
     * LINE #4
     @ ROW,000 SAY SUBSTR(PDIVISION,1,6)
     @ ROW,007 SAY DEPT
     @ ROW,016 SAY LEFT(XSTADDR32,30)
     @ ROW,047 SAY NOTE2
     @ ROW,078 SAY LEFT(XREGION,2) + LEFT(PREGION,13)
     @ ROW,098 SAY COMM2        PICTURE '99.99'
     @ ROW,114 SAY OPEN         PICTURE '9999999'
     @ ROW,121 SAY OPENAMT      PICTURE '99999999.99'
     ROW = ROW+1
     * LINE 5
     @ ROW,000 SAY PRIORITY
     IF llRpBulk
       @ ROW,007 SAY 'BULK'
     ENDIF
     IF LEN(TRIM(cfaccode)) <>0
        @ ROW,007 SAY 'APPROVE:'
        @ ROW,016 SAY cfaccode
        @ ROW,022 SAY APPRAMT   PICTURE '9999999'
        @ ROW,030 SAY APPROVAL
     ENDIF
     IF !EMPTY(cCancReson)
       @ Row, 47 SAY cCancReson
       PCANREASON= gfCodDes(ORDHDR.cCancReson,'cCancReson')
       *---------------------------------------
       @ ROW, 52 SAY PCANREASON
     ENDIF
   ELSE       &&SHORT FORM
     @ ROW,000 SAY ORDER
     IF llRpBulk
       @ ROW,007 SAY 'B'
     ENDIF
     @ ROW,009 SAY STATUS
     @ ROW,011 SAY SEASON
     @ ROW,014 SAY ACCOUNT
     @ ROW,020 SAY SUBSTR(CUSTOMER->STNAME,1,12)
     @ ROW,033 SAY IIF(MULTIPO,'*MULTI-PO*',CUSTPO)
     @ ROW,044 SAY COMPLETE
     @ ROW,053 SAY BOOK         PICTURE '9999999'
     @ ROW,061 SAY BOOKAMT      PICTURE '99999999.99'
     @ ROW,073 SAY SHIP         PICTURE '9999999'
     @ ROW,081 SAY SHIPAMT      PICTURE '99999999.99'
     @ ROW,093 SAY CANCEL       PICTURE '9999999'
     @ ROW,101 SAY CANCELAMT    PICTURE '99999999.99'
     @ ROW,114 SAY OPEN         PICTURE '9999999'
     @ ROW,121 SAY OPENAMT      PICTURE '99999999.99'
   ENDIF
   ROW = ROW+1
  *** DO NOT INCLUDE BULK ORDERS THAT HAVE CANCELLED STATUS
  *** IN CALCULATING TOTALS.
  IF !(llRpBulk .AND. STATUS='X')
     XTOTAL(1,1) = XTOTAL(1,1) + BOOK
     XTOTAL(1,2) = XTOTAL(1,2) + BOOKAMT
     XTOTAL(1,3) = XTOTAL(1,3) + SHIP
     XTOTAL(1,4) = XTOTAL(1,4) + SHIPAMT
     XTOTAL(1,5) = XTOTAL(1,5) + CANCEL
     XTOTAL(1,6) = XTOTAL(1,6) + CANCELAMT
     XTOTAL(1,7) = XTOTAL(1,7) + OPEN
     XTOTAL(1,8) = XTOTAL(1,8) + OPENAMT
   ENDIF
   SELE &CUSTFILE
   IF XSORT='S'              && SALESREP
     SEEK &WORKFILE->REP1+&WORKFILE->ACCOUNT
     IF .NOT. FOUND()
       XREP_CNT  =XREP_CNT + 1
       APPE BLANK
       REPL ACCOUNT WITH &WORKFILE->ACCOUNT
       REPL SALESREP WITH &WORKFILE->REP1
     ENDIF
   ELSE
     SEEK &WORKFILE->ACCOUNT
     IF .NOT. FOUND()
       XGRAND_CNT=XGRAND_CNT + 1
       APPE BLANK
       REPL ACCOUNT WITH &WORKFILE->ACCOUNT
       REPL SALESREP WITH &WORKFILE->REP1
     ENDIF
   ENDIF
   ***************************************************
   IF llRpOrdNot

     DO PRT_NOTE WITH 'B',XORDER
   ENDIF
   SELECT &WORKFILE
   SKIP
ENDDO
*------------------ END MAIN REPORT LOOP --------------------
X = 2
IF LEN(TRIM(BREAK)) =0
   X =1
ENDIF
IF XSORT='S'    && IF SORTED BY SALESREP TO GET ACCOUNT COUNT
  SELE &CUSTFILE
  SET ORDER TO 2
  COUNT TO XGRAND_CNT
  SELECT &WORKFILE
ENDIF
@ ROW,00 SAY REPLICATE('*',132)
ROW = ROW+1
IF lcRpFormat ='L'
  @ ROW,000 SAY XGRAND_CNT PICTURE '99999'
  @ ROW,006 SAY IIF(XGRAND_CNT<>1,'Accounts','Account')
  @ ROW,020 SAY '......BOOKED.......      ......SHIPPED......      .....CANCELED......      .......OPEN........'
  ROW = ROW+1
  @ ROW,000 SAY '** GRAND TOTAL ** '
  @ ROW,020 SAY XTOTAL(X,1)  PICTURE '9999999'
  @ ROW,028 SAY XTOTAL(X,2)  PICTURE '99999999.99'
  @ ROW,045 SAY XTOTAL(X,3)  PICTURE '9999999'
  @ ROW,053 SAY XTOTAL(X,4)  PICTURE '99999999.99'
  @ ROW,070 SAY XTOTAL(X,5)  PICTURE '9999999'
  @ ROW,078 SAY XTOTAL(X,6)  PICTURE '99999999.99'
  @ ROW,095 SAY XTOTAL(X,7)  PICTURE '9999999'
  @ ROW,103 SAY XTOTAL(X,8)  PICTURE '99999999.99'
ELSE      && SHORT FORM
  @ ROW,000 SAY '** GRAND TOTAL ** '
  @ ROW,025 SAY XGRAND_CNT PICTURE '99999'
  @ ROW,031 SAY IIF(XGRAND_CNT<>1,'Accounts','Account')
  @ ROW,053 SAY XTOTAL(X,1)  PICTURE '9999999'
  @ ROW,061 SAY XTOTAL(X,2)  PICTURE '99999999.99'
  @ ROW,073 SAY XTOTAL(X,3)  PICTURE '9999999'
  @ ROW,081 SAY XTOTAL(X,4)  PICTURE '99999999.99'
  @ ROW,093 SAY XTOTAL(X,5)  PICTURE '9999999'
  @ ROW,101 SAY XTOTAL(X,6)  PICTURE '99999999.99'
  @ ROW,113 SAY XTOTAL(X,7)  PICTURE '9999999'
  @ ROW,121 SAY XTOTAL(X,8)  PICTURE '99999999.99'
ENDIF
ROW = ROW+1
@ ROW,00 SAY REPLICATE('*',132)

DO ENDREPORT
*!*************************************************************
*! Name      : lfsrOrder
*! Developer : Mohamed Shokry (MMH)
*! Date      : 01/15/2001
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! C1alled from : Option Grid
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
SELECT ORDHDR
SET RELATION TO IIF(EMPTY(STORE),"M","S") + ACCOUNT + STORE INTO CUSTOMER
IF lcParm = "S"
  IF !EMPTY(lcRpStatus)
	  SET FILTER TO STATUS$lcRpStatus
  ENDIF
  llClearOrd = .F.
  LOCATE
ELSE
  IF !EMPTY(lcRpStatus)
    SET FILTER TO
  ENDIF
ENDIF  
*-- end of lfsrOrder.

*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Mohamed Shokry (MMH)
*! Date      : 01/15/2001
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
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfsrRep
*! Developer : Mohamed Shokry (MMH)
*! Date      : 01/15/2001
*! Purpose   : Rise change sales rep. flag, in range browse screen.
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
*! Example   : =lfsrRep()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsrRep
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    GO TOP IN SALESREP
  CASE lcParm = 'R'
    llClearRep = .F.
ENDCASE
*-- end of lfsrRep.
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Mohamed Shokry (MMH)
*! Date      : 01/15/2001
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
lcRpStatus = ''
IF (ALEN(laRpTarget,1) < 5) AND !(ALEN(laRpTarget,1) = 1 AND EMPTY(laRpTarget))
  DIMENSION laStatus[5]
  laStatus[1] = 'B' 
  laStatus[2] = 'O' 
  laStatus[3] = 'H' 
  laStatus[4] = 'X' 
  laStatus[5] = 'C' 

  PRIVATE lnI
  *-- Loop to make Status expression.
  FOR lnI = 1 TO ALEN(laRpSource,1)
    
    IF ATC('\',laRpSource[lnI]) > 0
      lcRpStatus = lcRpStatus + laStatus[lnI]
    ENDIF  
  ENDFOR  && end Loop to make Status expression.
ENDIF  

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llOGFltCh  = .T.
  llClearOrd = .T.
ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llOGFltCh  = .T.
      llClearOrd = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]

*-- Activate suppress expression [begin]
*--C102163,4 MHM 26/02/2001 no need to clear read [start]
*llCancNow = ('X' $ lcRpStatus) OR EMPTY(lcRpStatus)
*llCancPrv = ('X' $ lcOldStat)  OR EMPTY(lcOldStat)
*IF (llCancNow AND !llCancPrv) OR (!llCancNow AND llCancPrv)
*  CLEAR READ
*ENDIF
*--C102163,4 MHM 26/02/2001 no need to clear read [end]
*-- Activate suppress expression [end]
*-- end of lfvOStatus.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Shokry (MMH)
*! Date      : 01/15/2001
*! Purpose   : When function of OG
*!*************************************************************
*! Called from : OG read cycle
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
*
FUNCTION lfwRepWhen

IF EMPTY(laRpSource)   
   DECLARE laRpSource[5],laRpTarget[5]

  STORE 'Bid'       TO laRpSource[1],laRpTarget[1]
  STORE 'Open'      TO laRpSource[2],laRpTarget[2]
  STORE 'Hold'      TO laRpSource[3],laRpTarget[3]
  STORE 'Canceled'  TO laRpSource[4],laRpTarget[4]
  STORE 'Completed' TO laRpSource[5],laRpTarget[5]
ENDIF
DECLARE  laShipTo[5,1],laCodDesc[6,3]
STORE ''          TO lcRpStatus,laShipTo,laCodDesc
laCodDesc[1,2] = "SEASON"
laCodDesc[2,2] = "CDIVISION"
laCodDesc[3,2] = "CTERMCODE"
laCodDesc[4,2] = "SHIPVIA"
laCodDesc[5,2] = "SPCINST"
laCodDesc[6,2] = "REGION"

*!*************************************************************
*! Name      : lfAsignVar
*! Developer : Mohamed Shokry (MMH)
*! Date      : 01/15/2001
*! Purpose   : Assign all Report Variables.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfAsignVar()
*!*************************************************************
FUNCTION lfAsignVar
PARAMETERS llFromPrg

IF !llFromPrg
  *-- Make Sort by Arrays and fill it. [Begin]
  DIMENSION laSortDesc[6,1],laSortVal[6,1]
  laSortDesc[1,1] = "Order"
  laSortDesc[2,1] = "Account"
  laSortDesc[3,1] = "Cust PO"
  laSortDesc[4,1] = "Priority"
  laSortDesc[5,1] = "Primary Sales Representative"
  laSortDesc[6,1] = "Cancelation Reason"

  laSortVal[1,1]  = "O"
  laSortVal[2,1]  = "A"
  laSortVal[3,1]  = "C"
  laSortVal[4,1]  = "P"
  laSortVal[5,1]  = "S"
  laSortVal[6,1]  = "N"
  *-- Make Sort by Arrays and fill it. [Begin]
ENDIF
  
*!*************************************************************
*! Name      : PRT_NOTE
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/26/2001
*! Purpose   : Print Order line notes.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Non
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO PRT_NOTE
*!*************************************************************
PROCEDURE PRT_NOTE

PARAMETER XTYPE,XKEY
PRIVATE lnAlias, lnMemoWdth, lnMemoLnNo, lnMemolins
WAIT CLEAR
lnAlias = SELECT()
SELECT NOTEPAD

IF SEEK (XTYPE+XKEY)
  lnMemoWdth = SET('MEMOWIDTH')
  SET MEMOWIDTH TO 75
  lnMemoLnNo = 0      
  lnMemolins = 0
  IF ROW > 55
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
    ROW = 5
  ENDIF
  
  lnMemolins = MEMLINES(mNotes)
  IF lnMemolins > 0
    @ ROW,00 SAY REPLICATE('|',80 )
    ROW = ROW + 1

    DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
      IF ROW > 55
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
        ROW = 5
      ENDIF
      lnMemoLnNo = lnMemoLnNo + 1
      IF SUBSTR(MLINE(mNotes,lnMemoLnNo),1,1) = '*'
        LOOP
      ENDIF
      @ ROW,00 SAY '|'+MLINE(mNotes,lnMemoLnNo)
      ROW = ROW + 1
    ENDDO
    @ ROW,00 SAY REPLICATE('|',80 )
    ROW = ROW + 1

  ENDIF
  SET MEMOWIDTH TO (lnMemoWdth)
ENDIF

SELECT (lnAlias)
RETURN
