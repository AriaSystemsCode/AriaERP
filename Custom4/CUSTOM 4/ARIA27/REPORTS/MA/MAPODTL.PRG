*:********************************************************************************************
*: Program file  : MAPODTL
*: Program desc. : Material PURCHASE ORDER DETAIL
*! Date          : 03/09/1999
*: System        : Aria Advantage Series.
*: Module        : MATERIALS (M)
*: Developer     : BASSEM RAFAAT (BWA)
*:********************************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:********************************************************************************************
*: Passed Parameters  : None
*:********************************************************************************************
*: Notes   : Enh. # 301166
*:********************************************************************************************
*: Example : DO MAPODTL
*:********************************************************************************************
*: Modifications:
*: B603128,1 HDM 08/25/1999 Damaged should be OTHERS as it confuses the user
*: B603252,1 HDM 11/09/1999 Fixing bug of
*:                          - Wrong open qty Calculation
*:                            if there is over receiving
*:                          - Format B prints only the first line of the PO
*: B602908,1 ADEL 08/23/2000 When browsing material POs, Browse only POs not Return POS...etc.
*: B602908,1                 This modification is done in OG.
*: B603170,1 ADEL 08/24/2000 1- Change "Fabric #" label in OG to "Fabric".
*: B603170,1                 2- If selecting "Summary", Disable "Report Format" option.
*: B603170,1                 3- Add "Currency" and "Currency Display" in OG and make them work.
*: B803016,1 ADEL 09/03/2000 Fix the bug of caculate 'Amount' field in the 'Reciving lines' 
*: B803016,1                 based on the default PO's cost not the received cost and also show 
*: B803016,1                 the recieved Price not the default one.
*: B804481,1 ABD  10/25/2001 The detail report form 'B' Show a wrong UOM.
*: B605091,1 ADEL 12/06/2001 Fix the bug calculating Grand Total For Open incorrectly when changing sorts.
*: B605281,1 WAB  12/26/2001 FIX the BUG incorrect accounting in PO detail report format a summary
*: B605409,1 RAE  02/18/2002 Material PO detail screen (option grid) blows up
*:********************************************************************************************

*-- llPrint variable that is used to prevent printing if there is not
*--           any record matches the report criteria
PRIVATE llPrint
llPrint    = .F.

*** LOOP FOR THE REPORT CRITERIA ***

*-- R_WIDTH  Variable that hold the report width
*-- R_TITLE  Variable that hold the report title
*-- XREPORT  variable that hold the report name

R_WIDTH    = 'W'       && STANDARD REPORT IS 'WIDE'
R_TITLE    = 'MATERIAL PURCHASE ORDER DETAIL REPORT'
XREPORT    = 'MaPoDtl'

*-- QCountry  Variable that hold the name of country
llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)
llIsEngland= IIF(UPPER(ALLTRIM(gcContCode)) = 'ENGLAND', .T., .F.)

*--  variable that is used to get the contents of the pofln.trancd
*--           and get its value to variable xrep
lnDatePos = ASCAN(laOGFxFlt,"POFLN.TRANCD")
IF lnDatePos > 0
  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
  XREP  = laOGFxFlt[lnDatePos,6]
ELSE
  XREP = SPACE(0)
ENDIF


*-- XFILTER  variable of the option grid
*B603170,1 (Begin) Print POs only.
*XFILTER    = lcRpExp
XFILTER    = lcRpExp + "AND cmattype = 'P'"
*B603170,1 (End)
XPRINT     = lcRPPrint
XFORMAT    = lcRpFrmat
lnChoice   = lcRpSort
XTITLE     = lcRPOpti
XDYELOT_S  = llDyelot


*-- lcWorkFile  Variable that hold the temporary file name

lcWorkFile = gfTempName()

*---- SORT TO  lcWorkFile FILE	 
SORTFB    = ' '
SORTFIELD = ' '
BREAK     = ' '
BREAKD    = ' '

DO CASE
*---SUMMARY BY ORDER#
  CASE lnChoice = 'P'
    SORTFIELD   = 'POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
    *SORTFIELD   = 'POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7)'
    *B603252,1 [Start] adjust the index in the temp file to make
    *                  the TranCd the latest field
    SORTFB      = 'POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7)'
    *SORTFB      = 'POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
    *B603252,1 [End] 
    
    BREAK       = 'POMAT'
    BREAKD      = 'POMAT'

*---SUMMARY BY VENDOR ACCOUNT
  CASE lnChoice = 'V'
    *B603252,1 [Start] index should be on the temp file and not including any
    *                  fields from other files
    *SORTFIELD   = 'POFHDR->VENDOR+POMAT+TRANCD+FABRIC+COLOR+STR(RECNO(),7)'
    *SORTFB      = 'POFHDR->VENDOR+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7)'
    *BREAK       = 'POFHDR->VENDOR'
    *BREAKD      = 'VENDOR'
    SORTFIELD   = 'VENDOR+TRANCD+POMAT+FABRIC+COLOR+STR(RECNO(),7)'
    SORTFB      = 'VENDOR+POMAT+FABRIC+COLOR+TRANCD+STR(RECNO(),7)'
    BREAK       = 'VENDOR'
    BREAKD      = 'VENDOR'
    *B603252,1 [End]

*---SUMMARY BY ITEM
  CASE lnChoice = 'I'
    SORTFIELD   = 'FABRIC+TRANCD+COLOR+POMAT+STR(RECNO(),7)'
    BREAK       = 'FABRIC'
    BREAKD      = 'FABRIC'
    
*---SUMMARY BY COLOR
  CASE lnChoice = 'C'
    SORTFIELD   = 'FABRIC+COLOR+TRANCD+POMAT+STR(RECNO(),7)'
    BREAK       = 'FABRIC+COLOR'
    BREAKD      = 'COLOR'
    
ENDCASE
*B603170,1 (Begin) Make Subtotals work if the currency changes.
BREAK = IIF(llMultCurr AND lcRpCurr = "F",'cCurrCode+"/"+'+BREAK,BREAK)
*B603170,1 (End)
*-- THE MAIN PROGRAM 
DO lpCollData

*-- PRINT THE REPORT FROM THE WORKFILE
IF llPrint
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF


IF USED(lcWorkFile)
  SELECT (lcWorkFile)
  SET RELATION TO
  USE 
ENDIF

*-- ERASE THE lcWorkFile
ERASE (gcWorkDir+lcWorkFile+'.DBF')  
ERASE (gcWorkDir+lcWorkFile+'.CDX')  
*!*************************************************************
*! Name      : lpCollData
*! Developer : BASSEM RAFAAT 
*! Date      : 03/09/1999
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollData
*!*************************************************************

PROCEDURE lpCollData

SELECT POFLN
SET FILTER TO
SET RELATION TO
SET RELATION TO Cmattype+POMAT INTO POFHDR
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
LOCATE ALL FOR &XFILTER
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG')
  *B605409,1 RAE [START]
  SET DEVICE TO SCREEN
  *B605409,1 RAE [END]
  RETURN
ELSE
  WAIT 'Selecting records for report ...' WINDOW NOWAIT
  llPrint    = .T. 
  *B603170,1 (Begin) Add the currency filed in case of Foriegn Currency.
  *COPY REST TO &lcWorkFile FOR &XFILTER
  IF llMultCurr AND lcRpCurr = "F"
    =AFIELDS(laFileStru)
    lnFileStru = ALEN(laFileStru,1)
    DIMENSION laFileStru[lnFileStru+1,4]
    lnFileStru = lnFileStru+1
    laFileStru[lnFileStru,1] = 'cCurrCode'
    laFileStru[lnFileStru,2] = 'C'
    laFileStru[lnFileStru,3] = 3
    laFileStru[lnFileStru,4] = 0
    CREATE TABLE (gcWorkDir+lcWorkFile) FROM ARRAY laFileStru
    SELECT POFLN
    SCAN REST FOR &XFILTER
      SCATTER MEMVAR MEMO
      m.cCurrCode = IIF(SEEK('P'+POMAT,'POFHDR'),POFHDR.cPriceCur,SPACE(3))
      INSERT INTO (lcWorkFile) FROM MEMVAR
    ENDSCAN
    SELECT (lcWorkFile)
    USE
  ELSE
    COPY REST TO (gcWorkDir+lcWorkFile) FOR &XFILTER  
  ENDIF  
  *B603170,1 (End)
  SELECT POFLN
  SET RELATION TO
  *B603170,1 (Begin) Open the temp file from the work directory.
  *=gfOpenFile('&lcWorkFile',' ','EX')
  =gfOpenFile((gcWorkDir+lcWorkFile),' ','EX')
  *B603170,1 (End)
        
  *---SORT TO lcWorkFile INDEX
       
  SELECT (lcWorkFile)
  IF !EMPTY(SORTFIELD)
    WAIT 'Sorting  records for material purchase order detail report ...' WINDOW NOWAIT
    
    IF XFORMAT='A'
      SORTFIELD = SORTFIELD
    ELSE
      SORTFIELD = SORTFB
    ENDIF
    *B603170,1 (Begin) Add the currency filed to the index in case of Foriegn Currency.
    SORTFIELD = IIF(llMultCurr AND lcRpCurr = "F","cCurrCode+"+SORTFIELD,SORTFIELD)
    *B603170,1 (End)
    
    INDEX ON &SORTFIELD TAG &lcWorkFile
    SET RELATION TO Cmattype+POMAT INTO POFHDR
    SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITIVE
    WAIT CLEAR
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT 
*! Date      : 03/09/1999
*! Purpose   : Print the report
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
*------------------------------------------------------------------
* SELECT REPORT FILE & INITIALIZE MEMORY   (F O R M A T   'A' )
*------------------------------------------------------------------
PROCEDURE LpPrint

DO CASE 
  CASE XFORMAT='A'
    *B603170,1 (Begin) Initialize a new array for the Currency Subtotal.
    IF llMultCurr AND lcRpCurr = "F"
      DIMENSION laCurrSub(5,3)
      STORE 0 TO laCurrSub
      lcCrntCurr = cCurrCode
    ENDIF  
    *B603170,1 (End)

    DO WHILE XFORMAT='A'
  
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
*
* *SUB TOTALS*    XXXXXXXXXXXX
*  ORDERED................................                                            99999999.999     99999999          99999999.99
*  RECEIVED...............................                                            99999999.999     99999999          99999999.99
*  DAMAGED................................                                            99999999.999     99999999          99999999.99
*  CANCELD................................                                            99999999.999     99999999          99999999.99      
*  OPEN...................................                                            99999999.999     99999999          99999999.99
*
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..

     DIMENSION XTOTAL(5,3),XTOTAL1(5,3)
     XTOTAL = 0.00
     XTOTAL1= 0.00
     PAGENO = 0
     ROW    = 99
     XTIME  = TIME()
     SELECT &lcWorkFile
 
     IF LEN(TRIM(BREAK)) <>0
       HBREAK = &BREAK
     ENDIF

     CLEAR TYPEAHEAD
     SELECT &lcWorkFile
     PTRANCD=SPACE(1)
*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------

     DO WHILE INKEY() <>32
       IF ROW >=53
         PAGENO = PAGENO+1
         DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
         @ 05,00 SAY '                COMPLETE'
         IF !XDYELOT_S
           IF llIsEngland
             @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     œPRICE      AMOUNT'
           ELSE
             IF llIsCanada
               @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE      PRICE      AMOUNT'
             ELSE
               *B603170,1 (Begin) Remove dollar sign in case of multi currency.
               *@ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     $PRICE      AMOUNT'
               IF llMultCurr
                 @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE      PRICE      AMOUNT'                 
               ELSE
                 @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     REFERENCE  PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     $PRICE      AMOUNT'                                
               ENDIF
               *B603170,1 (End)
             ENDIF
           ENDIF
         ELSE
           IF llIsEngland 
             @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     œPRICE      AMOUNT'
           ELSE
             IF llIsCanada
               @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     PRICE      AMOUNT'
             ELSE
              *B603170,1 (Begin) Remove dollar sign in case of multi currency.
              *@ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     $PRICE      AMOUNT'
              IF llMultCurr
                 @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE      PRICE      AMOUNT'                
              ELSE
                 @ 06,00 SAY 'PO#    VENDOR   RECEIVED S ITEM    COLOR  DESC.     DYELOT #   PATTERN   WIDTH       TOTQTY BUY  TOTQTY USE     $PRICE      AMOUNT'
              ENDIF
               *B603170,1 (End)               
             ENDIF
           ENDIF
         ENDIF
         @ 07,00 SAY REPLICATE('=',130)
         ROW = 08
      ENDIF
      DO WHILE LEN(TRIM(BREAK)) <>0
        IF &BREAK = HBREAK
          EXIT
        ENDIF
         ROW=ROW+1
         @ ROW,00 SAY REPLICATE('=',130)
         ROW = ROW+1
         @ ROW,000 SAY '* SUB TOTAL * '
         @ ROW,015 SAY HBREAK
         
         *--Call the description of the color by global function gfCodDes
         @ ROW,35 SAY gfCodDes(SUBSTR(HBREAK,8), 'COLOR')
         ROW=ROW+1
         
         IF  '1' $ XREP .OR.  EMPTY(XREP)
           @ ROW,000 SAY '  ORDERED................................'
           @ ROW,080 SAY XTOTAL(1,1)  PICTURE '99999999.999'
           @ ROW,96 SAY XTOTAL(1,2)  PICTURE '99999999'
           @ ROW,118 SAY XTOTAL(1,3) PICTURE '99999999.999'           
                      
           ROW = ROW+1
         ENDIF
         
         IF  '2' $ XREP .OR. EMPTY(XREP)
           @ ROW,000 SAY '  RECEIVED...............................'
           @ ROW,080 SAY XTOTAL(2,1)  PICTURE '99999999.999'
           @ ROW,96 SAY XTOTAL(2,2)  PICTURE '99999999'
           @ ROW,118 SAY XTOTAL(2,3) PICTURE '99999999.999'
                     
           ROW = ROW+1
         ENDIF
         
         IF '3' $ XREP .OR. EMPTY(XREP)
           *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
           *@ ROW,000 SAY '  DAMAGED................................'
            @ ROW,000 SAY '  OTHERS.................................'
           *-- HDM B603128,1 [End]
           @ ROW,080 SAY XTOTAL(3,1)  PICTURE '99999999.999'
           @ ROW,096 SAY XTOTAL(3,2)  PICTURE '99999999'
           @ ROW,118 SAY XTOTAL(3,3) PICTURE '99999999.999'
           
           ROW = ROW+1
         ENDIF
         
         IF '4'  $ XREP .OR. EMPTY(XREP)
           @ ROW,000 SAY '  CANCELD................................'
           @ ROW,080 SAY XTOTAL(4,1)  PICTURE '99999999.999'
           @ ROW,096 SAY XTOTAL(4,2)  PICTURE '99999999'
           @ ROW,118 SAY XTOTAL(4,3)  PICTURE '99999999.999'
           
           ROW = ROW+1
         ENDIF
        
         IF '1|2' $ XREP .OR. EMPTY(XREP) 
           @ ROW,000 SAY '  OPEN...................................'
           @ ROW,080 SAY XTOTAL(5,1)  PICTURE '99999999.999'
           @ ROW,096 SAY XTOTAL(5,2)  PICTURE '99999999'
           @ ROW,118 SAY XTOTAL(5,3) PICTURE '99999999.999'
           
           ROW = ROW+1
         ENDIF
         @ ROW,00 SAY REPLICATE('=',130)
         ROW = ROW+1
         X = 1
         DO WHILE X<=3
            XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
            XTOTAL1(2,X) = XTOTAL1(2,X) + XTOTAL(2,X)
            XTOTAL1(3,X) = XTOTAL1(3,X) + XTOTAL(3,X)
            XTOTAL1(4,X) = XTOTAL1(4,X) + XTOTAL(4,X)
            XTOTAL1(5,X) = XTOTAL1(5,X) + XTOTAL(5,X)
            XTOTAL(1,X) = 0.00
            XTOTAL(2,X) = 0.00
            XTOTAL(3,X) = 0.00
            XTOTAL(4,X) = 0
            XTOTAL(5,X) = 0
            X = X+1
         ENDDO
         HBREAK = &BREAK
         PTRANCD = SPACE(1)
         EXIT
     ENDDO
     *B603170,1 (Begin) Print currency subtotal
     IF (llMultCurr AND lcRpCurr = "F") AND  cCurrCode <> lcCrntCurr
       ROW=ROW+1
       @ ROW,00 SAY REPLICATE('=',130)
       ROW = ROW+1
       @ ROW,000 SAY '* SUB TOTAL * '
       IF SEEK(lcCrntCurr,'SYCCURR')
         lcCrntCurr = lcCrntCurr + ' - ' + ALLTRIM(SYCCURR.CCURRDESC)
       ENDIF
       @ ROW,015 SAY lcCrntCurr
       ROW = ROW+1 
       IF  '1' $ XREP .OR.  EMPTY(XREP)
         @ ROW,000 SAY '  ORDERED................................'
         @ ROW,080 SAY laCurrSub(1,1)  PICTURE '99999999.999'
         @ ROW,96  SAY laCurrSub(1,2)  PICTURE '99999999'
         @ ROW,118 SAY laCurrSub(1,3)  PICTURE '99999999.999'           
         ROW = ROW+1
       ENDIF
       IF  '2' $ XREP .OR. EMPTY(XREP)
         @ ROW,000 SAY '  RECEIVED...............................'
         @ ROW,080 SAY laCurrSub(2,1)  PICTURE '99999999.999'
         @ ROW,96  SAY laCurrSub(2,2)  PICTURE '99999999'
         @ ROW,118 SAY laCurrSub(2,3)  PICTURE '99999999.999'
         ROW = ROW+1
       ENDIF
       IF '3' $ XREP .OR. EMPTY(XREP)
         @ ROW,000 SAY '  OTHERS.................................'
         @ ROW,080 SAY laCurrSub(3,1)  PICTURE '99999999.999'
         @ ROW,096 SAY laCurrSub(3,2)  PICTURE '99999999'
         @ ROW,118 SAY laCurrSub(3,3)  PICTURE '99999999.999'
         ROW = ROW+1
       ENDIF
       IF '4'  $ XREP .OR. EMPTY(XREP)
         @ ROW,000 SAY '  CANCELD................................'
         @ ROW,080 SAY laCurrSub(4,1)  PICTURE '99999999.999'
         @ ROW,096 SAY laCurrSub(4,2)  PICTURE '99999999'
         @ ROW,118 SAY laCurrSub(4,3)  PICTURE '99999999.999'
         ROW = ROW+1
       ENDIF
       IF '1|2' $ XREP .OR. EMPTY(XREP) 
         @ ROW,000 SAY '  OPEN...................................'
         @ ROW,080 SAY laCurrSub(5,1)  PICTURE '99999999.999'
         @ ROW,096 SAY laCurrSub(5,2)  PICTURE '99999999'
         @ ROW,118 SAY laCurrSub(5,3) PICTURE '99999999.999'
         ROW = ROW+1
       ENDIF
       @ ROW,00 SAY REPLICATE('=',130)
       STORE 0 TO laCurrSub
       lcCrntCurr = cCurrCode
     ENDIF
     *B603170,1 (End)

  *--------------------- END SUBTOTALS ----------------------------
     IF EOF()
       EXIT
     ENDIF
     IF ROW >= 53
       ROW = 99
       LOOP
     ENDIF
      
     SELECT &lcWorkFile
     DO WHILE XPRINT='D'
       ROW=ROW+1
       IF (TRANCD='1') .AND. (TRANCD<>PTRANCD)
         PTRANCD=TRANCD
         @ ROW,000 SAY '--------------------------------------------------------- ORDERED LINES ----------------------------------------------------------'
         ROW=ROW+1
       ENDIF
       IF (TRANCD='2') .AND. (TRANCD<>PTRANCD)
         PTRANCD=TRANCD
         @ ROW,000 SAY '--------------------------------------------------------- RECEIVED LINES ---------------------------------------------------------'
         ROW=ROW+1
       ENDIF
       IF (TRANCD='3') .AND. (TRANCD<>PTRANCD)
         PTRANCD=TRANCD
         *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
         *@ ROW,000 SAY '--------------------------------------------------------- DAMAGED LINES ----------------------------------------------------------'
          @ ROW,000 SAY '--------------------------------------------------------- OTHERS LINES -----------------------------------------------------------'
         *-- HDM B603128,1 [End]
         
         ROW=ROW+1
       ENDIF
       IF (TRANCD = '4' ) .AND. ( TRANCD<>PTRANCD )
         PTRANCD = TRANCD
         @ ROW,000 SAY '--------------------------------------------------------- CANCELED LINES ---------------------------------------------------------'
         ROW=ROW+1
       ENDIF

       @ ROW,000 SAY POMAT
       @ ROW,007 SAY POFHDR->VENDOR
       IF INLIST(TRANCD,'2','3','4')
         @ ROW,016 SAY DATE
       ENDIF
       IF TRANCD='1'
         @ ROW,016 SAY POFHDR->COMPLETE
       ENDIF
       @ ROW,025 SAY POFHDR->STATUS
       @ ROW,027 SAY FABRIC
       @ ROW,035 SAY COLOR
       @ ROW,042 SAY LEFT(FABRIC->DESC,09)
       IF XDYELOT_S             && IF DYELOT SYSTEM
         @ ROW,052 SAY DYELOT
       ELSE
         @ ROW,052 SAY LEFT(REFERENCE,11)
       ENDIF
       @ ROW,063 SAY LEFT(PATTERN,9)
       @ ROW,073 SAY WIDTH
       @ ROW,080 SAY NFABTOTQTY PICTURE '9999999.999'
       @ ROW,092 SAY FABRIC->UOMBUY
       @ ROW,096 SAY (NFABTOTQTY*FABRIC->CONV)  PICTURE '9999999'
       @ ROW,104 SAY FABRIC->UOMUSE
       *B603170,1 (Begin) Get the real price according to the Currency Display selected.
       *@ ROW,107 SAY ncost1 PICTURE '9999999.999'
       *@ ROW,118 SAY (NFABTOTQTY*ncost1) PICTURE '99999999.999'        
       lnRealPrice= lfCalAmnts()
       @ ROW,107 SAY lnRealPrice PICTURE '9999999.999'
       @ ROW,118 SAY (NFABTOTQTY*lnRealPrice) PICTURE '99999999.999'        
       *B603170,1 (End)
       EXIT
     ENDDO

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
     XTOTAL(XTRNCD,1)=XTOTAL(XTRNCD,1)+NFABTOTQTY
     XTOTAL(XTRNCD,2)=XTOTAL(XTRNCD,2)+(NFABTOTQTY*FABRIC->CONV)
     *B603170,1 (Begin) Get the real price according to the Currency Display selected.
     *XTOTAL(XTRNCD,3)=XTOTAL(XTRNCD,3)+(NFABTOTQTY*(ncost1/POFHDR->NpriceRat))
     lnRealPrice= lfCalAmnts()
     XTOTAL(XTRNCD,3)=XTOTAL(XTRNCD,3)+(NFABTOTQTY*lnRealPrice)
     IF llMultCurr AND lcRpCurr = "F"
       *--Update currency subtotal array.
       laCurrSub(XTRNCD,1)=laCurrSub(XTRNCD,1)+NFABTOTQTY
       laCurrSub(XTRNCD,2)=laCurrSub(XTRNCD,2)+(NFABTOTQTY*FABRIC->CONV)
       laCurrSub(XTRNCD,3)=laCurrSub(XTRNCD,3)+(NFABTOTQTY*lnRealPrice)
     ENDIF  
     *B603170,1 (End)
     *B603252,1 [Start] Wrong open QTY when over receiving 1 line in
     *B603252,1         PO Containes more than 1 line
     PRIVATE lnDumRec , lnRealQty
     STORE 0 TO lnRealQty
     lnDumRec = IIF(EOF(),0,RECNO())
     *B605281,1 WAB (Start) 
     IF xtrncd <> 1
       lnRecvQty = nFabTotQty
     ELSE
       lnRecvQty = 0
     ENDIF
     *B605281,1 WAB (End) 
     
     *B603170,1 (Begin) Add the currency ciode to the fiollowing seeks.
     *DO CASE
     *  CASE lnChoice = 'P'
     *    =SEEK(POMAT+'1'+FABRIC+COLOR)
     *  CASE lnChoice = 'V'
     *    =SEEK(VENDOR+'1'+POMAT+FABRIC+COLOR)
     *  CASE lnChoice = 'I'
     *    =SEEK(FABRIC+'1'+COLOR+POMAT)
     *  CASE lnChoice = 'C'
     *    =SEEK(FABRIC+COLOR+'1'+POMAT)
     *ENDCASE
     lnOpenPrice = 0
     IF llMultCurr AND lcRpCurr = "F"
       lcCurrCode = cCurrCode
     ELSE
       lcCurrCode = ''
     ENDIF  
     
     DO CASE
       CASE lnChoice = 'P'
         =SEEK(lcCurrCode+POMAT+'1'+FABRIC+COLOR)
       CASE lnChoice = 'V'
       
        =SEEK(lcCurrCode+VENDOR+'1'+POMAT+FABRIC+COLOR)
       CASE lnChoice = 'I'
         =SEEK(lcCurrCode+FABRIC+'1'+COLOR+POMAT)
       CASE lnChoice = 'C'
         =SEEK(lcCurrCode+FABRIC+COLOR+'1'+POMAT)
     ENDCASE
     *B603170,1 (End)
     IF FOUND()
       *B605281,1 WAB (Start)  substract the recievd qty from the Budjet qty so i can handle the case 
       *B605281,1 WAB          of over rceiceve to accoumlate correctly the Open amount in case of sort by vendor
       IF xTrncd <> 1
          lnRealQty = MIN(nFabTotQty,lnRecvQty)
          REPLACE nFabTotQty WITH MAX(nFabTotQty - lnRecvQty,0)
       ELSE
         lnRealQty = nFabTotQty
       ENDIF
       *B605281,1 WAB (End) 
       
       *B803016,1 (Begin) Get the original M.PO price.
       lnOpenPrice= lfCalAmnts()
       *B803016,1 (End)
     ENDIF

     IF lnDumRec > 0
       GO lnDumRec
     ENDIF

     *Add the maximum QTY (0,nFabTotQty) TO the xTotal to avoid adding negative values
     *xTotal( 5, 1 ) = xTotal( 5, 1 ) + ( IIF( xTrnCd = 1 , 1, -1) * nFabTotQty )
     *xTotal( 5, 2 ) = xTotal( 5, 2 ) + ( IIF( xTrnCd = 1 , 1, -1) * ( nFabTotQty * FABRIC->CONV) )
     *xTotal( 5, 3 ) = xTotal( 5, 3 ) + ( IIF( xTrnCd = 1 , 1, -1) * ( nFabTotQty * (ncost1/POFHDR->NpriceRat)) )
     *B605281,1 WAB (Start)  
     *xTotal( 5, 1 ) = MAX(0,xTotal( 5, 1 ) + ( IIF( xTrnCd = 1 , 1, -1) * MIN(lnRealQty,nFabTotQty) ))
     *xTotal( 5, 2 ) = MAX(0,xTotal( 5, 2 ) + ( IIF( xTrnCd = 1 , 1, -1) * ( MIN(lnRealQty,nFabTotQty) * FABRIC->CONV)))
     
     xTotal( 5, 1 ) = MAX(0,xTotal( 5, 1 ) + ( IIF( xTrnCd = 1 , 1, -1) * lnRealQty ))
     xTotal( 5, 2 ) = MAX(0,xTotal( 5, 2 ) + ( IIF( xTrnCd = 1 , 1, -1) * ( lnRealQty * FABRIC->CONV)))
     *B605281,1 WAB (End) 
     
     *B603170,1 (Begin) Get the real price according to the Currency Display selected.
     *xTotal( 5, 3 ) = MAX(0,xTotal( 5, 3 ) + ( IIF( xTrnCd = 1 , 1, -1) * ( MIN(lnRealQty,nFabTotQty) * (ncost1/POFHDR->NpriceRat))))
     *B803016,1 (Begin)
     *--Let the Open Amnt = Remain qty * Original price.
     *B605091,1 (Begin) Increase/Decrease theis line Amount only.
     *xTotal( 5, 3 ) = (lnOpenPrice*xTotal( 5, 1 ))
     xTotal( 5, 3 )  = MAX(0,xTotal( 5, 3 )  + IIF( xTrnCd = 1 , 1, -1) * (lnOpenPrice*MIN(lnRealQty,nFabTotQty)))
     *B605091,1 (End)

     *B803016,1 (End)
     *--Update currency subtotal array.
     IF llMultCurr AND lcRpCurr = "F"
       laCurrSub(5,1)=MAX(0,laCurrSub( 5, 1 ) + ( IIF( xTrnCd = 1 , 1, -1) * MIN(lnRealQty,nFabTotQty) ))
       laCurrSub(5,2)=MAX(0,laCurrSub( 5, 2 ) + ( IIF( xTrnCd = 1 , 1, -1) * ( MIN(lnRealQty,nFabTotQty) * FABRIC->CONV)))
       *B803016,1 (Begin) *--Let the Open Amnt = Remain qty * Original price. 
       *B605091,1 (Begin) Increase/Decrease theis line Amount only.
       *laCurrSub(5,3)=(lnOpenPrice*laCurrSub(5,1))
       laCurrSub(5,3)  = MAX(0,laCurrSub(5,3)  + IIF( xTrnCd = 1 , 1, -1) * (lnOpenPrice*MIN(lnRealQty,nFabTotQty)))
       *B605091,1 (End)
       *B803016,1 (End)
     ENDIF  
     *B603170,1 (End)
     *B603252,1[End]
     SELECT &lcWorkFile 
     SKIP
   ENDDO 
   *------------------ END MAIN REPORT LOOP --------------------
   *B603170,1 (Begin) Print Grand Total in case of Single currenc or Muti and not in Foriegn Curr. 
   IF !llMultCurr OR lcRpCurr <> "F"
   *B603170,1 (End)
   
     X = 2
     IF LEN(TRIM(BREAK)) =0
        X =1
     ENDIF
     ROW=ROW+2
     @ ROW,00 SAY REPLICATE('*',130)
     ROW = ROW+1
     @ ROW,000 SAY '* GRAND TOTAL *'
     ROW=ROW+1
   
     IF '1' $ XREP .OR. EMPTY(XREP)
       @ ROW,000 SAY '  ORDERED................................'
       @ ROW,079 SAY XTOTAL1(1,1)  PICTURE '999999999.999'
       @ ROW,094 SAY XTOTAL1(1,2)  PICTURE '999999999'
       @ ROW,116 SAY XTOTAL1(1,3)  PICTURE '9999999999.999'
       ROW = ROW+1
     ENDIF
   
     IF '2' $ XREP .OR. EMPTY(XREP)
      @ ROW,000 SAY '  RECEIVED...............................'
      @ ROW,079 SAY XTOTAL1(2,1)  PICTURE '999999999.999'
      @ ROW,094 SAY XTOTAL1(2,2)  PICTURE '999999999'
      @ ROW,116 SAY XTOTAL1(2,3)  PICTURE '9999999999.999'
    
      ROW = ROW+1
    ENDIF
   
    IF '3' $ XREP .OR. EMPTY(XREP)
      *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
      *@ ROW,000 SAY '  DAMAGED................................'
       @ ROW,000 SAY '  OTHERS.................................'
      *-- HDM B603128,1 [End]
    
      @ ROW,079 SAY XTOTAL1(3,1)  PICTURE '999999999.999'
      @ ROW,094 SAY XTOTAL1(3,2)  PICTURE '999999999'
      @ ROW,116 SAY XTOTAL1(3,3)  PICTURE '9999999999.999'
    
      ROW = ROW+1
     ENDIF

     IF '4' $ XREP .OR. EMPTY(XREP)
       @ ROW,000 SAY '  CANCELD................................'
       @ ROW,079 SAY XTOTAL1(4,1) PICTURE '999999999.999'
       @ ROW,094 SAY XTOTAL1(4,2)  PICTURE '999999999'
       @ ROW,116 SAY XTOTAL1(4,3)  PICTURE '9999999999.999'
     
       ROW = ROW+1
     ENDIF
   
      IF '1|2' $ XREP .OR. EMPTY(XREP)
       @ ROW,000 SAY '  OPEN...................................'
       @ ROW,079 SAY XTOTAL1(5,1)  PICTURE '999999999.999'
       @ ROW,094 SAY XTOTAL1(5,2)  PICTURE '999999999'
       @ ROW,116 SAY XTOTAL1(5,3)  PICTURE '9999999999.999'
     
       ROW = ROW+1
     ENDIF
     @ ROW,00 SAY REPLICATE('*',130)
   *B603170,1 (Begin) Print Grand Total in case of Single currenc or Muti and not in Foriegn Curr. 
   ENDIF
   *B603170,1 (End)
   EXIT
  ENDDO
****************************************************************************
* PROG: MAT671B.PRG
* DESC: FORMAT B OF OPEN PURCHASE ORDER DETAIL REPORT
* DATE: 07/01/93
* AUTH: TIMOUR ABDALLA
*!ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*!Modifications :
*!     MFM 07/12/94
*!         1) Incresed vendor code to 8 characters and fixed the 
*!            says of data in it's correct places.
*!     MFM 01/01/95
*!         1) Printing the canceld lines.
*!***************************************************************************


*------------------------------------------------------------------
* [3] SELECT REPORT FILE & INITIALIZE MEMORY   (F O R M A T   'B' )
*------------------------------------------------------------------
CASE XFORMAT='B'
  XCancel = 0
  DO WHILE XFORMAT='B'
  *B603170,1 (Begin) Initialize Currency subtotal variables.
  IF llMultCurr AND lcRpCurr = "F"
    STORE 0 TO lnCurrCan, lnCurrOrd,lnCurrAmt,lnCurrRec,lnCurrDam,lnCurrCanc,lnCurrOpen
    lcCrntCurr = cCurrCode
  ENDIF
  *B603170,1 (End)
  
  
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
    SELECT &lcWorkFile

    XCONT=.F.       && flag is .T. if continued in a new page
    XPO=SPACE(6)
    STORE 0 TO XTOTORD,XTOTAMT,XTOTREC,XTOTDAM,XTOTOPN,XTOTCAN
    STORE 0 TO XGTOTORD,XGTOTAMT,XGTOTREC,XGTOTDAM,XGTOTOPN,XGTOTCAN 

*---------------------------------------------------------
* [REPORT] LOOP
*---------------------------------------------------------

    DO WHILE .T.
      IF ROW >=55
        PAGENO = PAGENO+1
        DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
        @ 05,00 SAY 'P.O.#   VENDOR   COMPLETE       S                                                RECEIVED'
        IF !XDYELOT_S
          IF llIsEngland 
            *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
            *@ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     œPRICE       AMOUNT DATE     REFERENCE  RECEIVD DAMAGED CANCELD  OPEN'
             @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     œPRICE       AMOUNT DATE     REFERENCE  RECEIVD OTHERS  CANCELD  OPEN'
            *-- HDM B603128,1 [END]
          ELSE
            IF llIsCanada
              *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
              *@ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM      PRICE       AMOUNT DATE     REFERENCE  RECEIVD DAMAGED CANCELD  OPEN'
               @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM      PRICE       AMOUNT DATE     REFERENCE  RECEIVD OTHERS  CANCELD  OPEN'
              *-- HDM B603128,1 [END]
            ELSE
              *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
              *@ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     REFERENCE  RECEIVD DAMAGED CANCELD  OPEN'
              *B603170,1 (Begin) Remove dollar sign in case of multi currency.
              * @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     REFERENCE  RECEIVD OTHERS  CANCELD  OPEN'
              IF llMultCurr
                @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM      PRICE       AMOUNT DATE     REFERENCE  RECEIVD OTHERS  CANCELD  OPEN'
              ELSE
                @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     REFERENCE  RECEIVD OTHERS  CANCELD  OPEN'              
              ENDIF
              *B603170,1 (End)
              *-- HDM B603128,1 [END]
            ENDIF
          ENDIF
        ELSE
          IF llIsEngland 
            *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
            *@ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     œPRICE       AMOUNT DATE     DYELOT #   RECEIVD DAMAGED CANCELD  OPEN'
             @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     œPRICE       AMOUNT DATE     DYELOT #   RECEIVD OTHERS  CANCELD  OPEN'
            *-- HDM B603128,1 [END]

          ELSE
            IF llIsCanada
              *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
              *@ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM      PRICE       AMOUNT DATE     DYELOT #   RECEIVD DAMAGED CANCELD  OPEN'
               @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM      PRICE       AMOUNT DATE     DYELOT #   RECEIVD OTHERS  CANCELD  OPEN'
              *-- HDM B603128,1 [END]
            ELSE
              *-- HDM B603128,1 [Start] Damaged should be OTHERS as it confuses the user
              *@ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     DYELOT #   RECEIVD DAMAGED CANCELD  OPEN'
              *B603170,1 (Begin) Remove dollar sign in case of multi currency.
              * @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     DYELOT #   RECEIVD OTHERS  CANCELD  OPEN'
              IF llMultCurr
                @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM      PRICE       AMOUNT DATE     DYELOT #   RECEIVD OTHERS  CANCELD  OPEN'
              ELSE
                @ 06,00 SAY 'ITEM    COLOR  DESCRIPTION  PATTERN   WIDTH  ORDERED UOM     $PRICE       AMOUNT DATE     DYELOT #   RECEIVD OTHERS  CANCELD  OPEN'              
              ENDIF
              *B603170,1 (End)
              *-- HDM B603128,1 [End]
            ENDIF
          ENDIF
        ENDIF
        @ 07,00 SAY REPLICATE('=',130)
        ROW = 08
      ENDIF

      ******** SUB TOTAL *********
      DO WHILE LEN(TRIM(BREAK)) <>0
        IF &BREAK = HBREAK
          EXIT
        ENDIF
        @ ROW,00 SAY REPLICATE('=',130)
        ROW = ROW+1
        @ ROW,000 SAY '* SUB TOTAL * '
        @ ROW,015 SAY HBREAK
        IF XTOTORD<>0
          @ ROW,044 SAY XTOTORD  PICTURE '9999999.999'
        ENDIF
        IF XTOTAMT<>0
          @ ROW,067 SAY XTOTAMT  PICTURE '999999999.999'
        ENDIF
        IF XTOTREC<>0
          @ ROW,101 SAY XTOTREC  PICTURE '9999999'
        ENDIF
        IF XTOTDAM<>0
          @ ROW,109 SAY XTOTDAM  PICTURE '9999999'
        ENDIF
        @ ROW,117 SAY IIF(XTOTCAN=0,XCANCEL,XTOTCAN) PICTURE '9999999'
        @ ROW,125 SAY XTOTOPN PICTURE '99999'
        ROW = ROW+1
        @ ROW,00 SAY REPLICATE('=',130)
        IF LEFT(BREAKD,1)='V'
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
      *B603170,1 (Begin) Print Currency subtotal
      IF llMultCurr AND lcRpCurr = "F" AND  cCurrCode <> lcCrntCurr
        ROW = ROW+1
        @ ROW,00 SAY REPLICATE('=',130)
        ROW = ROW+1
        @ ROW,000 SAY '* SUB TOTAL * '
        IF SEEK(lcCrntCurr,'SYCCURR')
          lcCrntCurr = lcCrntCurr + ' - ' + ALLTRIM(SYCCURR.CCURRDESC)
        ENDIF
        @ ROW,015 SAY lcCrntCurr
        IF lnCurrOrd<>0
          @ ROW,044 SAY lnCurrOrd PICTURE '9999999.999'
        ENDIF
        IF lnCurrAmt<>0
          @ ROW,067 SAY lnCurrAmt  PICTURE '999999999.999'
        ENDIF
        IF lnCurrRec <>0
          @ ROW,101 SAY lnCurrRec  PICTURE '9999999'
        ENDIF
        IF lnCurrDam <>0
          @ ROW,109 SAY lnCurrDam  PICTURE '9999999'
        ENDIF
        @ ROW,117 SAY IIF(lnCurrCanc=0,lnCurrCan,lnCurrCanc) PICTURE '9999999'
        @ ROW,125 SAY lnCurrOpen PICTURE '99999'
        ROW = ROW+1
        @ ROW,00 SAY REPLICATE('=',130)
        STORE 0 TO lnCurrCan, lnCurrOrd,lnCurrAmt,lnCurrRec,lnCurrDam,lnCurrCanc,lnCurrOpen        
        lcCrntCurr = cCurrCode
      ENDIF
      *B603170,1 (End)
      *--------------------- END SUBTOTALS ----------------------------
      IF EOF()
        EXIT
      ENDIF
      IF ROW >=55
        ROW = 99
        LOOP
      ENDIF

      IF POMAT<>XPO  .OR. XCONT
        IF XPO<>SPACE(6) .AND. !XCONT
          ROW=ROW+1
        ENDIF  
        XPO=POMAT
        @ ROW,000 SAY POMAT
        @ ROW,008 SAY POFHDR->VENDOR
        @ ROW,017 SAY POFHDR->COMPLETE
        @ ROW,032 SAY POFHDR->STATUS
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
      *B603252,1 [Start] Get the vendor Name to be included in the while section
      *                  while scanning the temp WorkFile
      XVEN = VENDOR
      *B603252,1 [End]
      XOPENFLAG=.T.     && Flag to check if it more than one trancd

      *B603252,1 [Start] Include fabric,color in the while section
      *                  to prevent the scan from positioning the
      *                  record pointer at the end of the file so
      *                  main DO loop will stop printing the rest
      *                  of the report
      *SCAN WHILE XPO=POMAT FOR XFABRIC=FABRIC .AND. XCOLOR=COLOR
      
      *Adjust the scan expresion according to the used index
      IF lnChoice = 'V'
        lcScanExp = "XVEN + XPO + XFABRIC + XCOLOR = VENDOR + POMAT + FABRIC + COLOR"
      ELSE
        lcScanExp = "XPO + XFABRIC + XCOLOR = POMAT + FABRIC + COLOR"
      ENDIF

      SCAN WHILE &lcScanExp
        IF ROW >=57
          ROW = 99
          XOPENFLAG=.F.
          XCONT=.T.
          EXIT
        ENDIF
        XCAN_OP = IIF(POFHDR->STATUS='X','XCNCLD','XOPEN')
        IF TRANCD='1'
          @ ROW,000 SAY FABRIC
          @ ROW,008 SAY COLOR
          @ ROW,015 SAY LEFT(FABRIC->DESC,12)
          @ ROW,028 SAY LEFT(PATTERN,9)
          @ ROW,038 SAY WIDTH
          @ ROW,044 SAY nFabTOTQTY PICTURE '9999999'
          
          *B804481,1 ABD - The detail report form 'B' Show a wrong UOM. [Begin]
          *@ ROW,052 SAY FABRIC->UOMUSE    &&UOMBYE
          @ ROW,052 SAY FABRIC->UOMBUY    &&UOMBYE
          *B804481,1 ABD - [End]
          
          *B603170,1 (Begin) Get the real price according to the Currency Display selected.
          *@ ROW,057 SAY ncost1  PICTURE '9999999.99'  
          *@ ROW,068 SAY (NFABTOTQTY*ncost1) PICTURE '99999999.999' 
          lnRealPrice= lfCalAmnts()
          @ ROW,057 SAY lnRealPrice PICTURE '9999999.99'  
          @ ROW,068 SAY (NFABTOTQTY*lnRealPrice) PICTURE '99999999.999' 
          *B603170,1 (End)
          &XCAN_OP = &XCAN_OP + NFABTOTQTY 
        ELSE   && (TRANCD='2') .OR. (TRANCD='3')

          XOPENFLAG=.F.
          @ ROW,081 SAY DATE
          IF XDYELOT_S             && IF DYELOT SYSTEM
            @ ROW,090 SAY DYELOT
          ELSE
            @ ROW,090 SAY LEFT(REFERENCE,11)
          ENDIF
          IF TRANCD='2'
            @ ROW,101 SAY NFABTOTQTY PICTURE '9999999'
          ENDIF
          IF TRANCD='3'
            @ ROW,109 SAY NFABTOTQTY PICTURE '9999999'
          ENDIF
          
          IF TranCd = '4'
            @ ROW,117 SAY nFabTotQty PICTURE '9999999'
          ENDIF
            &XCAN_OP = MAX((&XCAN_OP - NFABTOTQTY),0)
          
          IF XCNCLD <> 0
            @ ROW,117 SAY XCNCLD PICTURE '9999999'
          ENDIF
          @ ROW,125 SAY XOPEN  PICTURE '99999'
          ROW=ROW+1
        ENDIF
        
        DO CASE
          CASE TRANCD='1'
            XTOTORD=XTOTORD+NFABTOTQTY
            *B603170,1 (Begin) Get the real price according to the Currency Display selected.
            *XTOTAMT=XTOTAMT+(NFABTOTQTY*ncost1) 
            XTOTAMT=XTOTAMT+(NFABTOTQTY*lnRealPrice) 
            IF llMultCurr AND lcRpCurr = "F" AND  cCurrCode <> lcCrntCurr
              lnCurrAmt = lnCurrAmt + (NFABTOTQTY*lnRealPrice)
              lnCurrOrd = lnCurrOrd + NFABTOTQTY
            ENDIF  
            *B603170,1 (End)
          CASE TRANCD='2'
            XTOTREC=XTOTREC+NFABTOTQTY
            *B603170,1 (Begin) Calculate received qty.
            IF llMultCurr AND lcRpCurr = "F"
              lnCurrRec = lnCurrRec + NFABTOTQTY
            ENDIF
            *B603170,1 (End)
            
          CASE TRANCD='3'
            XTOTDAM=XTOTDAM+NFABTOTQTY
            *B603170,1 (Begin) Calculate damaged qty
            IF llMultCurr AND lcRpCurr = "F"
              lnCurrDam = lnCurrDam + NFABTOTQTY
            ENDIF  
            *B603170,1 (End)
          CASE TRANCD='4'
            xCancel = xCancel + NFABTotQty
            *B603170,1 (Begin) Calculate cqanceled qty
            IF llMultCurr AND lcRpCurr = "F"
              lnCurrCan = lnCurrCan + NFABTotQty
            ENDIF  
            *B603170,1 (End)
        ENDCASE
        SELECT POFHDR                  
        XCANCEL = POFHDR->NFABCANCEL   
        *B603170,1 (Begin) Calculate canceled qty
        IF llMultCurr AND lcRpCurr = "F"
          lnCurrCan = POFHDR->NFABCANCEL
        ENDIF  
        *B603170,1 (End)
        
        XCAN_OPTOT  = IIF(POFHDR->STATUS='X','XTOTCAN','XTOTOPN')
      
        *B603252,1 [Start] Collect the data from the current temp file not from POFLN
        *SELECT POFLN
        SELECT (lcWorkFile)
        &XCAN_OPTOT = IIF(TRANCD='1',(&XCAN_OPTOT+NFABTOTQTY),MAX((&XCAN_OPTOT-NFABTOTQTY),0))
        *B603252,1 [End]
        *B603170,1 (Begin) Calculate acnceld and open qtys.
        IF llMultCurr AND lcRpCurr = "F"
          lcOpenCan  = IIF(POFHDR.STATUS='X','lnCurrCanc','lnCurrOpen')
          &lcOpenCan = IIF(TRANCD='1',(&lcOpenCan+NFABTOTQTY),MAX((&lcOpenCan-NFABTOTQTY),0))
        ENDIF  
        *B603170,1 (End)
      ENDSCAN
      
      IF XOPENFLAG
        IF XCAN_OP = 'XCNCLD'
          @ ROW,117 SAY XCNCLD PICTURE '9999999'
        ELSE
          @ ROW,125 SAY XOPEN PICTURE '99999'
        ENDIF
        ROW=ROW+1
      ENDIF
   ENDDO
   *------------------ END MAIN REPORT LOOP --------------------
   *B603170,1 (Begin) Print Grand Total in case of Single currenc or Muti and not in Foriegn Curr. 
   IF !llMultCurr OR lcRpCurr <> "F"
   *B603170,1 (End)

     ROW=ROW+1
     @ ROW,00 SAY REPLICATE('*',130)
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
     @ ROW,125 SAY XGTOTOPN PICTURE '99999'
     ROW=ROW+1
     @ ROW,00 SAY REPLICATE('*',130)
   *B603170,1 (Begin) Print Grand Total in case of Single currenc or Muti and not in Foriegn Curr. 
   ENDIF
   *B603170,1 (End)
    EXIT
  ENDDO
ENDCASE 
*!*************************************************************
*! Name      : lfSortDumy
*! Developer : BASSEM RAFAAT 
*! Date      : 03/09/1999
*! Purpose   : Validate sortby
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************

FUNCTION lfSortDumy
PARAMETERS lcPar
IF lcRpFrmat = 'A'
  DIMENSION laSortItem[4,1], laSortVal[4,1]
  
  laSortItem[1]= 'Po Number'  
  laSortItem[2]= 'Vendor' 
  laSortItem[3]= 'Item'
  laSortItem[4]= 'Color'
  
  laSortVal[1] = 'P'
  laSortVal[2] = 'V'
  laSortVal[3] = 'I'
  laSortVal[4] = 'C'
  
ELSE
  DIMENSION laSortItem[2,1] , laSortVal[2,1]
  
  laSortItem[1]='Po Number'  
  laSortItem[2]='Vendor'
    
  laSortVal[1] ='P'
  laSortVal[2] ='V'
  lcRPSort     = IIF( lcRPSort $ 'PV' ,lcRPSort,'P')
ENDIF
IF lcPar = 'V'
  =lfOGShowGet('lcRPSort')
ENDIF


*!*************************************************************
*! Name      : lfvVen
*! Developer : BASSEM RAFAAT 
*! Date      : 03/09/1999
*! Purpose   : Validate vendor
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVen()
*!*************************************************************

FUNCTION lfvVen
PRIVATE lcVenFld,lcVendor,lcSelFile,lcApvenTag

lcVenFld  = VARREAD()
lcVendor  = EVAL(lcVenFld) 

lnSelFile = SELECT(0)

SELECT APVENDOR
lcApvenTag  = ORDER('APVENDOR')

SET ORDER TO TAG VENCODE IN APVENDOR

IF !EMPTY(lcVendor) .AND.('?' $ lcVendor .OR. !SEEK(lcVendor , 'APVENDOR'))
  =gfApVnBrow(@lcVendor)
ENDIF
  
&lcVenFld = lcVendor

SET ORDER TO  lcApvenTag
 
SELECT (lnSelFile)

*!*************************************************************
*! Name      : lfvPo
*! Developer : BASSEM RAFAAT 
*! Date      : 03/09/1999
*! Purpose   : VALIDATE THE PURCHASE ORDER MATERIAL
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPo()
*!*************************************************************

FUNCTION lfvPo
PRIVATE lcPoFld,lcPorder,lcSelFile,lcPOTag

lcPoFld   = VARREAD()
lcPomat  = EVAL(lcPoFld)

lnSelFile = SELECT(0)

SELECT POFHDR
lcPOTag   = ORDER('POFHDR')

SET ORDER TO TAG POFHDR IN POFHDR

IF !EMPTY(lcPomat) .AND. ('?' $ lcPomat .OR. !SEEK('P'+lcPomat , 'POFHDR'))
  =POFBROW('P','',@lcPomat)
ENDIF
 
&lcPoFld  = lcPomat

SET ORDER TO lcPOTag

SELECT (lnSelFile)
*!*************************************************************
*! Name      : lfvFabric
*! Developer : BASSEM RAFAAT 
*! Date      : 03/09/1999
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
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT 
*! Date      : 03/09/1999
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

llDyelot = (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')

*!*************************************************************
*! Name      : lfSRVFab
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 03/09/1999
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
    *llChFabric = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
ENDCASE
*-- end of lfSRVFab.

*!*************************************************************
*! Name      : lfFabSum
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 03/09/1999
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

*!*************************************************************
*! Name      : lfSRVPo
*! Developer : BASSEM RAFAAT (BWA) 
*! Date      : 03/09/1999
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
*! Example   : =lfSRVPo()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVPo
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
*-- end of lfSRVPo



*!*************************************************************
*! Name      : lfDsablFrmt
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/23/2000
*! Purpose   : Disable the Report Foramt popup when Summray.
*!*************************************************************
FUNCTION lfDsablFrmt

lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lcRpFrmat'),1)
laOGObjCnt[lnPos] = (lcRpPrint= 'D')
IF lcRpPrint= 'S'
  lcRpFrmat = 'A'
ENDIF   
= lfOGShowGet('lcRpFrmat')  

*!*************************************************************
*! Name      : lfFillVars
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/23/2000
*! Purpose   : Fill most of report memory variables.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfFillVars()
*!*************************************************************
*B603170,1
*!*************************************************************
FUNCTION lfFillVars

DIMENSION laSetUps[7,2]
IF !USED('SYCCOMP')
  USE &gcSysHome.SYCCOMP ORDER TAG cComp_ID IN 0
  llOpenComp = .T.
ENDIF  

laSetUps[1,1]   = 'M_CITYPE1'
laSetUps[2,1]   = 'M_CITYPE2'
laSetUps[3,1]   = 'M_CITYPE3'
laSetUps[4,1]   = 'M_CITYPE4'
laSetUps[5,1]   = 'M_CITYPE5'

laSetUps[6,1]  = 'llMulCurr'
laSetUps[7,1]  = 'M_DYELOT'
= gfGetMemVar(@laSetups)
llMultCurr = laSetUps[6,2]
lcUseDye = laSetUps[7,2]
IF llMultCurr
  *-- Open international file.
  IF !USED("SYCINT")
    USE (gcSysHome+"SYCINT.DBF") IN 0 
    llOpenInt = .T.
  ENDIF
  *-- Open exchange rates file.
  IF !USED("SYCEXCH")
    USE (gcSysHome+"SYCEXCH.DBF") IN 0 ORDER TAG Currency
    llOpenExch = .T.
  ENDIF  
  *-- Fill Currency arrays [Begin]
  DIMENSION laCurrVal[1,1]
  *-- Open Currency file.
  IF !USED('SYCCURR')
    llOpenCurr = gfOpenFile(gcSysHome+'SYCCURR',gcSysHome+'Ccurrcode','SH')
  ELSE
    SELECT SYCCURR
    SET ORDER TO CCURRCODE  && To VALIDATE currency code.
  ENDIF
  SELECT DISTINCT CCURRCODE FROM SYCCURR ORDER BY CCURRCODE INTO ARRAY laCurrVal
  DIMENSION laCurrDesc[ALEN(laCurrVal,1),1]
  FOR lnI = 1 TO ALEN(laCurrVal,1)
    = SEEK(ALLTRIM(laCurrVal[lnI,1]))
    laCurrVal[lnI,1]  = PADR(laCurrVal[lnI,1],3)
    laCurrDesc[lnI,1] = CCURRCODE + ' - ' + ALLTRIM(CCURRDESC)
  ENDFOR
  *-- Fill Currency arrays [End  ]
ENDIF
*-- end of lfFillVars.

*!*************************************************************
*! Name      : lfvCurr
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/23/2000
*! Purpose   : Validate currency browse in OG.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfcurrbrow()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfvCurr()
*!*************************************************************
*B603170,1
*!*************************************************************
FUNCTION lfvCurr
PRIVATE lcVar , lcObj , laTemp,lcCurTag

lcCurFld = VARREAD()

lcCurVal = &lcCurFld

lcCurTag = ORDER('SYCCURR')
SET ORDER TO Ccurrcode IN SYCCURR

IF !EMPTY(lcCurVal) AND ('?' $ lcCurVal OR !SEEK(lcCurVal , 'SYCCURR'))
  = gfcurrbrow(@lcCurVal)
ENDIF  
SET ORDER TO lcCurTag IN SYCCURR
&lcCurFld = lcCurVal

*!*************************************************************
*! Name      : lfvCurDisp
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/23/2000
*! Purpose   : Activate currency display screen to get user 
*!           : selection for currencies.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfRepCur()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfvCurDisp()
*!*************************************************************
*B603170,1
*!*************************************************************
FUNCTION lfvCurDisp
llRpProced = gfRepCur(.T., @lcRpCurr,@ldRpExDate,lcRpTmpNam)
*-- end of lfvCurDisp.

*!*************************************************************
*! Name      : lfCalAmnts
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/23/2000
*! Purpose   : Calculate amount in selected currencies. 
*!           : selection for currencies.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfAmntDisp()
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None.
*!*************************************************************
*! Example   : =lfCalAmnts()
*!*************************************************************
*B603170,1
*!*************************************************************
FUNCTION lfCalAmnts
PRIVATE lnRetrnVal
lnRetrnVal = 0
*-- If Print in forign currency.
IF lcRpCurr = "F"
  *B803016,1 (Begin) Get the proper cost (Default of Received.)
  *lnRetrnVal = nCost1
  lnRetrnVal = IIF(TRANCD = '1',nCost1,nLan_Cost1)
  *B803016,1 (End)
ELSE
  *-- If Print in original base currency.
  IF lcRpCurr = "O"
    *B803016,1 (Begin) Get the proper cost (Default of Received.)
    *lnRetrnVal = nECost1
    lnRetrnVal = IIF(TRANCD = '1',nECost1,nELanCost1)
    *B803016,1 (End)
  ELSE  && print By Date or calculated now.
    IF (POfHDR.CPRICECUR= gcBaseCurr) OR (nCost1 = 0)
      *B803016,1 (Begin) Get the proper cost (Default of Received.)
      *lnRetrnVal = nCost1
      lnRetrnVal = IIF(TRANCD = '1',nCost1,nLan_Cost1)
      *B803016,1 (End)
    ELSE
      *B803016,1 (Begin) Get the proper cost (Default of Received.)
      *lnRetrnVal = gfAmntDisp(nCost1,lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"POFHDR")
      lnRetrnVal = gfAmntDisp(IIF(TRANCD = '1',nCost1,nLan_Cost1),lcRpCurr,ldRpExDate,lcRpTmpNam,.F.,"POFHDR")
      *B803016,1 (End)
    ENDIF  
  ENDIF
ENDIF
*-- End If Print in forign currency.
RETURN lnRetrnVal
*--end of lfCalAmnts.

