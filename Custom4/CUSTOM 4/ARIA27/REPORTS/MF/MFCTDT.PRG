*:************************************************************************
*: Program file  : MFCTDT.Prg
*: Program desc. : CUTTING TICKET DETAIL REPORT
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 07/28/98
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetTit(), lfsrvSty(), lfwOGWhen(), lfSRVFab(), 
*:                 lfFabSum(), lfTranCD()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO MFCTDT
*:************************************************************************
*: Modi    : 
*B802027,1 AMM 03/10/1999 Adjust in range function of the style major
*B602738,1 AMM 04/08/1999 Print the first 3 contractor and make a filter on them.
*B802277,1 AMM 05/19/1999 Add an option to sort by style major.
*B602947,1 AMM 06/07/1999 Enable user to select many status in the option grid
*:************************************************************************

QTOPIC='CUTTING TICKET DETAIL REPORT'
R_WIDTH  = 'W'    && STANDARD REPORT IS 'WIDE'
XREPORT  = 'MFCTDT'
R_TITLE  = 'CUTTING TICKET DETAIL REPORT'
STORE SPACE(0) TO lcStat , XRep
lcStyTitle = gfItemMask('HI')             && Style Title
*B802277,1 AMM Get the style major length
*-- Get the length of the style major
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSz    = LEN(lcMaj)                   && Length of the major
*B802277,1 AMM end
DO CASE     
  CASE lcSort='T'
    SORTFIELD = 'CUTTKT+TRANCD+STYLE+COLOR+STR(RECNO(),7)'
    BREAK     = 'CUTTKT'
    BREAKD    = 'CUTTKT'

     * SUMMARY BY COMPLETION DATE
  CASE lcSort='D'
    SORTFIELD = 'DTOC(CUTTKTH->COMPLETE)+CUTTKT+TRANCD+STYLE+COLOR+STR(RECNO(),7)'
    BREAK     = 'CUTTKTH->COMPLETE'
    BREAKD    = 'COMPLETE'

   * SUMMARY BY STYLE
   *B802277,1 AMM Sort by color
   *CASE lcSort='S'
   CASE lcSort='C'
   *B802277,1 AMM end
     SORTFIELD = 'STYLE+TRANCD+CUTTKT+STR(RECNO(),7)'
     BREAK     = 'STYLE'
     BREAKD    = 'STYLE'
   *B802277,1 AMM Sort and group by style major
   * SUMMARY BY STYLE
   CASE lcSort='S'
     SORTFIELD = 'LEFT(STYLE,lnMajSz)+TRANCD+SUBSTR(STYLE,lnMajSz+1)+CUTTKT+STR(RECNO(),7)'
     
     BREAK     = 'LEFT(STYLE,lnMajSz)'
     BREAKD    = 'STYLE'
   *B802277,1 AMM end
ENDCASE
IF !EMPTY(lcTranCD)
  lcRpExp = '('+lcRpExp+')'+ " .AND. (CUTTKTL.TRANCD $ lcTranCD )"
ENDIF
FOR lnCount=1 TO LEN(lcTranCD)
  XRep = XREP + SUBSTR('BRDC' , VAL(SUBSTR(lcTranCD,lnCount,1)) ,1  )
ENDFOR
*B602947,1 AMM status may be more than one, so adjust
*IF lcStatus <> "L"
  *lcRpExp = '(' +lcRpExp+ ") .AND. (CUTTKTH.Status = '"+lcStatus+"')"
*ENDIF
IF !EMPTY(lcRpStatus)
  lcRpExp = '(' +lcRpExp+ ") .AND. (CUTTKTH.Status $ '"+lcRpStatus+"')"
ENDIF
*B602947,1 AMM end
*B602738,1 AMM Create a temporary file based on CUTTKTH and add 3 fields 
*B602738,1 AMM for the 3 contractors
lcTmpTKTH = gfTempName()
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

Create Table (gcWorkDir+lcTmpTKTH) FROM ARRAY laFileStru
INDEX ON CUTTKT TAG &lcTmpTKTH
lcTmpOPH = GFTEMPNAME()
CREATE CURSOR (lcTmpOPH) (CTKTNO C(6), cOperSeq C(2),cOprCode C(6),cContCode C(8) )
*B802277,1 AMM Adjust not to cause a bug
*INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH
ZAP
lcSetPath = SET('FULLPATH')
SET FULLPATH ON
LcCDXName = STRTRAN(DBF() , '.TMP' , '.CDX')
INDEX ON CTKTNO+cOperSeq+cOprCode TAG &lcTmpOPH OF (LcCDXName)
SET FULLPATH &lcSetPath
*B802277,1 AMM END
*B602738,1 AMM end


*---------------------------------------------------------
* [3] SELECT REPORT FILE & INITIALIZE MEMORY
*---------------------------------------------------------
DO WHILE .T.
  SELECT CUTTKTL
  SET FILTER TO
  SET RELATION TO
  SET RELATION TO CUTTKT INTO CUTTKTH
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  
  LOCATE ALL FOR &lcRpExp
  IF EOF()
    *Message 'There are no records to display...!'
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF

  WORKFILE = gfTempName()
  *B602738,1 AMM Get the cut ticket contractors and apply the filter on them.
  *COPY REST TO (gcWorkDir+WorkFile) FOR &lcRpExp
  COPY STRUCTURE TO (gcWorkDir+WorkFile)
  =gfOpenFile(gcWorkDir+WorkFile,'','EX')
  SELECT CUTTKTL
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
      SCATTER MEMVAR MEMO 
      INSERT INTO (lcTmpTKTH) FROM MEMVAR
      SELECT CUTTKTL
      SCATTER MEMVAR MEMO 
      INSERT INTO (WorkFile) FROM MEMVAR
    ENDIF
  ENDSCAN
  *B602738,1 AMM end


  SELECT CUTTKTL
  SET RELATION TO
  
  *B602738,1 AMM The file allready opened.
  *=gfOpenFile(gcWorkDir+WorkFile,'','EX')
  SELECT (WorkFile)
  GO TOP
  IF EOF()
    *Message 'There are no records to display...!'
    =gfModalGen('TRM00052B00000','DIALOG')
    RETURN
  ENDIF
  *B602738,1 AMM end
  
  SET RELATION TO CUTTKT INTO CUTTKTH
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  *B602738,1 AMM Set relation to get contractors data.
  SET RELATION TO CUTTKT INTO (lcTmpTKTH) ADDITIVE
  *B602738,1 AMM end

   ***** NOW SET FILTER ON WORKFILE ONLY IF SOMETHING IN XFILTER1 *****
   ***** THE REASON THERE ARE TWO FILTERS IS THAT LENGTH OF ONE FILTER ******
   ***** BECOMES TWO LONG *****


   * SORT TO WORKFILE INDEX
  IF SORTFIELD<>' '
     Z = LTRIM(STR(RECCOUNT(),7))
     INDEX ON &SORTFIELD TAG &WORKFILE
     SET ORDER TO TAG &WORKFILE
  ENDIF
  DIMENSION XTOTAL(4,10),XTOTAL1(4,10)
  XTOTAL = 0.00
  XTOTAL1= 0.00
  PAGENO = 0
  ROW    = 99
  XTIME  = TIME()

  SELECT &WORKFILE
  GOTO TOP

  IF LEN(TRIM(BREAK)) <>0
    HBREAK = &BREAK
  ENDIF

  CLEAR TYPEAHEAD
  SET DEVICE TO PRINT

  SELE &WORKFILE
  * If the cut ticket status is canceled add a new 
  * record with the canceled quantity.
  SCAN FOR Trancd = '1' AND CUTTKTH.STATUS = 'X'
    lnRecNo=RECNO()
    SCATTER MEMVAR MEMO
    m.Trancd='4'
    *m.DATE = CUTTKTH.MOD_DATE
    m.DATE = CUTTKTH.DADD_DATE
    INSERT INTO (WorkFile) FROM MEMVAR
    GOTO lnRecNo
  ENDSCAN
  GO TOP
  PTRANCD=SPACE(1)
  *---------------------------------------------------------
  * [REPORT] LOOP
  *---------------------------------------------------------

  *- Define Alocal Character Variable & save in it
  *- A value of style#.

  lcSTYLE = STYLE

  DO WHILE INKEY() <>32
    IF ROW >=53
      PAGENO = PAGENO+1
      DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
        *B602738,1 AMM Add the 3 contractors title
        *@ 05,00 SAY  '                                  Complete'
        *@ 06,00 SAY  'CUTTKT Status   Season Div.       Received    '+PADR(lcStyTitle,25)+'QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7   QTY8  TOTQTY'
        @ 05,00 SAY  '                                        Complete'
        @ 06,00 SAY  'CUTTKT S Cont1    Cont2   Cont3   Se DV Received '+PADR(lcStyTitle,20)+'  QTY1   QTY2   QTY3   QTY4   QTY5   QTY6   QTY7   QTY8  TOTQTY'
        *B602738,1 AMM end
        ROW = 07
    ENDIF
    DO WHILE LEN(TRIM(BREAK)) <>0
      IF &BREAK = HBREAK
        EXIT
      ENDIF
      @ ROW,00 SAY REPLICATE('=',132)
      ROW = ROW+1
      @ ROW,000 SAY '* SUB TOTAL * '
      @ ROW,015 SAY HBREAK

      *- TMI 03/04/94 Print STYLE# & set variable with
      *- the new style value if the report is sorted by CUKTTKT#.
      IF  BREAKD = 'CUTTKT'
         @ ROW,046 SAY lcStyle
         lcSTYLE = STYLE
      ENDIF
      ROW=ROW+1

      IF (LEN(XREP)=0) .OR. ('B'$XREP)
        @ ROW,000 SAY '  BUDGET........................................................ '
        @ ROW,069 SAY XTOTAL(1,1)  PICTURE '999999'
        @ ROW,076 SAY XTOTAL(1,2)  PICTURE '999999'
        @ ROW,083 SAY XTOTAL(1,3)  PICTURE '999999'
        @ ROW,090 SAY XTOTAL(1,4)  PICTURE '999999'
        @ ROW,097 SAY XTOTAL(1,5)  PICTURE '999999'
        @ ROW,104 SAY XTOTAL(1,6)  PICTURE '999999'
        @ ROW,111 SAY XTOTAL(1,7)  PICTURE '999999'
        @ ROW,118 SAY XTOTAL(1,8)  PICTURE '999999'
        @ ROW,125 SAY XTOTAL(1,9)  PICTURE '9999999'

        ROW = ROW+1
      ENDIF

      IF (LEN(XREP)=0) .OR. ('R'$XREP)
        @ ROW,000 SAY '  RECEIVED...................................................... '
        @ ROW,069 SAY XTOTAL(2,1)  PICTURE '999999'
        @ ROW,076 SAY XTOTAL(2,2)  PICTURE '999999'
        @ ROW,083 SAY XTOTAL(2,3)  PICTURE '999999'
        @ ROW,090 SAY XTOTAL(2,4)  PICTURE '999999'
        @ ROW,097 SAY XTOTAL(2,5)  PICTURE '999999'
        @ ROW,104 SAY XTOTAL(2,6)  PICTURE '999999'
        @ ROW,111 SAY XTOTAL(2,7)  PICTURE '999999'
        @ ROW,118 SAY XTOTAL(2,8)  PICTURE '999999'
        @ ROW,125 SAY XTOTAL(2,9)  PICTURE '9999999'

        ROW = ROW+1
      ENDIF

      IF (LEN(XREP)=0) .OR. ('D'$XREP)
        @ ROW,000 SAY '  DAMAGED....................................................... '
        @ ROW,069 SAY XTOTAL(3,1)  PICTURE '999999'
        @ ROW,076 SAY XTOTAL(3,2)  PICTURE '999999'
        @ ROW,083 SAY XTOTAL(3,3)  PICTURE '999999'
        @ ROW,090 SAY XTOTAL(3,4)  PICTURE '999999'
        @ ROW,097 SAY XTOTAL(3,5)  PICTURE '999999'
        @ ROW,104 SAY XTOTAL(3,6)  PICTURE '999999'
        @ ROW,111 SAY XTOTAL(3,7)  PICTURE '999999'
        @ ROW,118 SAY XTOTAL(3,8)  PICTURE '999999'
        @ ROW,125 SAY XTOTAL(3,9)  PICTURE '9999999'
        ROW = ROW+1
      ENDIF

      IF (LEN(XREP)=0) .OR. ('C'$XREP)
        @ ROW,000 SAY '  CANCELLED..................................................... '
        @ ROW,069 SAY XTOTAL(4,1)  PICTURE '999999'
        @ ROW,076 SAY XTOTAL(4,2)  PICTURE '999999'
        @ ROW,083 SAY XTOTAL(4,3)  PICTURE '999999'
        @ ROW,090 SAY XTOTAL(4,4)  PICTURE '999999'
        @ ROW,097 SAY XTOTAL(4,5)  PICTURE '999999'
        @ ROW,104 SAY XTOTAL(4,6)  PICTURE '999999'
        @ ROW,111 SAY XTOTAL(4,7)  PICTURE '999999'
        @ ROW,118 SAY XTOTAL(4,8)  PICTURE '999999'
        @ ROW,125 SAY XTOTAL(4,9)  PICTURE '9999999'

        ROW = ROW+1
      ENDIF

      IF (LEN(XREP)=0)
        @ ROW,000 SAY '  OPEN.......................................................... '
        @ ROW,069 SAY MAX(XTOTAL(1,1)-(XTOTAL(2,1)+XTOTAL(3,1)+XTOTAL(4,1)),0)  PICTURE '999999'
        @ ROW,076 SAY MAX(XTOTAL(1,2)-(XTOTAL(2,2)+XTOTAL(3,2)+XTOTAL(4,2)),0)  PICTURE '999999'
        @ ROW,083 SAY MAX(XTOTAL(1,3)-(XTOTAL(2,3)+XTOTAL(3,3)+XTOTAL(4,3)),0)  PICTURE '999999'
        @ ROW,090 SAY MAX(XTOTAL(1,4)-(XTOTAL(2,4)+XTOTAL(3,4)+XTOTAL(4,4)),0)  PICTURE '999999'
        @ ROW,097 SAY MAX(XTOTAL(1,5)-(XTOTAL(2,5)+XTOTAL(3,5)+XTOTAL(4,5)),0)  PICTURE '999999'
        @ ROW,104 SAY MAX(XTOTAL(1,6)-(XTOTAL(2,6)+XTOTAL(3,6)+XTOTAL(4,6)),0)  PICTURE '999999'
        @ ROW,111 SAY MAX(XTOTAL(1,7)-(XTOTAL(2,7)+XTOTAL(3,7)+XTOTAL(4,7)),0)  PICTURE '999999'
        @ ROW,118 SAY MAX(XTOTAL(1,8)-(XTOTAL(2,8)+XTOTAL(3,8)+XTOTAL(4,8)),0)  PICTURE '999999'
        @ ROW,125 SAY MAX(XTOTAL(1,9)-(XTOTAL(2,9)+XTOTAL(3,9)+XTOTAL(4,9)),0)  PICTURE '9999999'

        ROW = ROW+1
      ENDIF

      X = 1
      DO WHILE X<=10
        XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
        XTOTAL1(2,X) = XTOTAL1(2,X) + XTOTAL(2,X)
        XTOTAL1(3,X) = XTOTAL1(3,X) + XTOTAL(3,X)
        XTOTAL1(4,X) = XTOTAL1(4,X) + XTOTAL(4,X)
        XTOTAL(1,X) = 0.00
        XTOTAL(2,X) = 0.00
        XTOTAL(3,X) = 0.00
        XTOTAL(4,X) = 0.00
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

    IF ROW >=53
      ROW = 99
      LOOP
    ENDIF

    SELECT &WORKFILE

    DO WHILE XPRINT='D'
      IF (TRANCD='1') .AND. (TRANCD<>PTRANCD)
        PTRANCD=TRANCD
        @ ROW,000 SAY '--------------------------------------------------------- BUDGET  LINES ------------------------------------------------------------'
        ROW=ROW+1
      ENDIF
      IF (TRANCD='2') .AND. (TRANCD<>PTRANCD)
        PTRANCD=TRANCD
        @ ROW,000 SAY '--------------------------------------------------------- RECEIVED LINES -----------------------------------------------------------'
        ROW=ROW+1
      ENDIF
      IF (TRANCD='3') .AND. (TRANCD<>PTRANCD)
        PTRANCD=TRANCD
        @ ROW,000 SAY '---------------------------------------------------------- DAMAGED  LINES ----------------------------------------------------------'
        ROW=ROW+1
      ENDIF
      IF (TRANCD='4') .AND. (TRANCD<>PTRANCD)
        PTRANCD=TRANCD
        @ ROW,000 SAY '---------------------------------------------------------- CANCELLED LINES ---------------------------------------------------------'
        ROW=ROW+1
      ENDIF
      @ ROW,000 SAY CUTTKT
      *B602738,1 AMM Add contractors and adjust 
      *DO CASE
        *CASE CUTTKTH.STATUS = 'O'
          *lcStat = 'Open'
        *CASE CUTTKTH.STATUS = 'H'
          *lcStat = 'Hold'
        *CASE CUTTKTH.STATUS = 'X'
          *lcStat = 'Canceled'
        *CASE CUTTKTH.STATUS = 'S'
          *lcStat = 'Closed'
        *CASE CUTTKTH.STATUS = 'C'
          *lcStat = 'Complete'
        *CASE CUTTKTH.STATUS = 'A'
          *lcStat = 'Actual'
      *ENDCASE
      *@ ROW,007 SAY lcStat
      *@ ROW,016 SAY CUTTKTH->SEASON
      *@ ROW,023 SAY CUTTKTH.CDIVISION
      *IF (TRANCD='2') .OR. (TRANCD='3') .OR. (TRANCD='4')
        *@ ROW,034 SAY DATE
      *ENDIF
      *IF TRANCD='1'
        *@ ROW,034 SAY CUTTKTH->COMPLETE
      *ENDIF
      *@ ROW,046 SAY STYLE
      *@ ROW,069 SAY QTY1   PICTURE '999999'
      *@ ROW,076 SAY QTY2   PICTURE '999999'
      *@ ROW,083 SAY QTY3   PICTURE '999999'
      *@ ROW,090 SAY QTY4   PICTURE '999999'
      *@ ROW,097 SAY QTY5   PICTURE '999999'
      *@ ROW,104 SAY QTY6   PICTURE '999999'
      *@ ROW,111 SAY QTY7   PICTURE '999999'
      *@ ROW,118 SAY QTY8   PICTURE '999999'
      *@ ROW,125 SAY TOTQTY PICTURE '9999999'

      @ ROW,007 SAY CUTTKTH.STATUS
      @ ROW,009  SAY &lcTmpTKTH..CONTR1
      @ ROW,018  SAY LEFT(&lcTmpTKTH..CONTR2,7)
      @ ROW,026  SAY LEFT(&lcTmpTKTH..CONTR3,7)

      @ ROW,034  SAY LEFT(CUTTKTH.SEASON,2)
      @ ROW,037  SAY LEFT(CUTTKTH.CDIVISION,2)
      IF (TRANCD='2') .OR. (TRANCD='3') .OR. (TRANCD='4')
        @ ROW,040 SAY DATE
      ENDIF
      IF TRANCD='1'
        @ ROW,040 SAY CUTTKTH->COMPLETE
      ENDIF
      @ ROW,049 SAY STYLE
      @ ROW,069 SAY QTY1   PICTURE '999999'
      @ ROW,076 SAY QTY2   PICTURE '999999'
      @ ROW,083 SAY QTY3   PICTURE '999999'
      @ ROW,090 SAY QTY4   PICTURE '999999'
      @ ROW,097 SAY QTY5   PICTURE '999999'
      @ ROW,104 SAY QTY6   PICTURE '999999'
      @ ROW,111 SAY QTY7   PICTURE '999999'
      @ ROW,118 SAY QTY8   PICTURE '999999'
      @ ROW,125 SAY TOTQTY PICTURE '9999999'
      *B602738,1 AMM end
      ROW=ROW+1
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

    XTOTAL(XTRNCD,1)=XTOTAL(XTRNCD,1)+QTY1
    XTOTAL(XTRNCD,2)=XTOTAL(XTRNCD,2)+QTY2
    XTOTAL(XTRNCD,3)=XTOTAL(XTRNCD,3)+QTY3
    XTOTAL(XTRNCD,4)=XTOTAL(XTRNCD,4)+QTY4
    XTOTAL(XTRNCD,5)=XTOTAL(XTRNCD,5)+QTY5
    XTOTAL(XTRNCD,6)=XTOTAL(XTRNCD,6)+QTY6
    XTOTAL(XTRNCD,7)=XTOTAL(XTRNCD,7)+QTY7
    XTOTAL(XTRNCD,8)=XTOTAL(XTRNCD,8)+QTY8
    XTOTAL(XTRNCD,9)=XTOTAL(XTRNCD,9)+TOTQTY
    SELECT &WORKFILE
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
 IF (LEN(XREP)=0) .OR. ('B'$XREP)
   @ ROW,000 SAY '  BUDGET........................................................ '
   @ ROW,069 SAY XTOTAL1(1,1)  PICTURE '999999'
   @ ROW,076 SAY XTOTAL1(1,2)  PICTURE '999999'
   @ ROW,083 SAY XTOTAL1(1,3)  PICTURE '999999'
   @ ROW,090 SAY XTOTAL1(1,4)  PICTURE '999999'
   @ ROW,097 SAY XTOTAL1(1,5)  PICTURE '999999'
   @ ROW,104 SAY XTOTAL1(1,6)  PICTURE '999999'
   @ ROW,111 SAY XTOTAL1(1,7)  PICTURE '999999'
   @ ROW,118 SAY XTOTAL1(1,8)  PICTURE '999999'
   @ ROW,125 SAY XTOTAL1(1,9)  PICTURE '9999999'
   ROW = ROW+1
 ENDIF

 IF (LEN(XREP)=0) .OR. ('R'$XREP)
   @ ROW,000 SAY '  RECEIVED...................................................... '
   @ ROW,069 SAY XTOTAL1(2,1)  PICTURE '999999'
   @ ROW,076 SAY XTOTAL1(2,2)  PICTURE '999999'
   @ ROW,083 SAY XTOTAL1(2,3)  PICTURE '999999'
   @ ROW,090 SAY XTOTAL1(2,4)  PICTURE '999999'
   @ ROW,097 SAY XTOTAL1(2,5)  PICTURE '999999'
   @ ROW,104 SAY XTOTAL1(2,6)  PICTURE '999999'
   @ ROW,111 SAY XTOTAL1(2,7)  PICTURE '999999'
   @ ROW,118 SAY XTOTAL1(2,8)  PICTURE '999999'
   @ ROW,125 SAY XTOTAL1(2,9)  PICTURE '9999999'
   ROW = ROW+1
 ENDIF

 IF (LEN(XREP)=0) .OR. ('D'$XREP)
   @ ROW,000 SAY '  DAMAGED....................................................... '
   @ ROW,069 SAY XTOTAL1(3,1)  PICTURE '999999'
   @ ROW,076 SAY XTOTAL1(3,2)  PICTURE '999999'
   @ ROW,083 SAY XTOTAL1(3,3)  PICTURE '999999'
   @ ROW,090 SAY XTOTAL1(3,4)  PICTURE '999999'
   @ ROW,097 SAY XTOTAL1(3,5)  PICTURE '999999'
   @ ROW,104 SAY XTOTAL1(3,6)  PICTURE '999999'
   @ ROW,111 SAY XTOTAL1(3,7)  PICTURE '999999'
   @ ROW,118 SAY XTOTAL1(3,8)  PICTURE '999999'
   @ ROW,125 SAY XTOTAL1(3,9)  PICTURE '9999999'
   ROW = ROW+1
 ENDIF

 IF (LEN(XREP)=0) .OR. ('C'$XREP)
   @ ROW,000 SAY '  CANCELLED..................................................... '
   @ ROW,069 SAY XTOTAL1(4,1)  PICTURE '999999'
   @ ROW,076 SAY XTOTAL1(4,2)  PICTURE '999999'
   @ ROW,083 SAY XTOTAL1(4,3)  PICTURE '999999'
   @ ROW,090 SAY XTOTAL1(4,4)  PICTURE '999999'
   @ ROW,097 SAY XTOTAL1(4,5)  PICTURE '999999'
   @ ROW,104 SAY XTOTAL1(4,6)  PICTURE '999999'
   @ ROW,111 SAY XTOTAL1(4,7)  PICTURE '999999'
   @ ROW,118 SAY XTOTAL1(4,8)  PICTURE '999999'
   @ ROW,125 SAY XTOTAL1(4,9)  PICTURE '9999999'
   ROW = ROW+1
 ENDIF

 IF (LEN(XREP)=0)
   @ ROW,000 SAY '  OPEN.......................................................... '
   @ ROW,069 SAY MAX(XTOTAL1(1,1)-(XTOTAL1(2,1)+XTOTAL1(3,1)+XTOTAL1(4,1)),0)  PICTURE '999999'
   @ ROW,076 SAY MAX(XTOTAL1(1,2)-(XTOTAL1(2,2)+XTOTAL1(3,2)+XTOTAL1(4,2)),0)  PICTURE '999999'
   @ ROW,083 SAY MAX(XTOTAL1(1,3)-(XTOTAL1(2,3)+XTOTAL1(3,3)+XTOTAL1(4,3)),0)  PICTURE '999999'
   @ ROW,090 SAY MAX(XTOTAL1(1,4)-(XTOTAL1(2,4)+XTOTAL1(3,4)+XTOTAL1(4,4)),0)  PICTURE '999999'
   @ ROW,097 SAY MAX(XTOTAL1(1,5)-(XTOTAL1(2,5)+XTOTAL1(3,5)+XTOTAL1(4,5)),0)  PICTURE '999999'
   @ ROW,104 SAY MAX(XTOTAL1(1,6)-(XTOTAL1(2,6)+XTOTAL1(3,6)+XTOTAL1(4,6)),0)  PICTURE '999999'
   @ ROW,111 SAY MAX(XTOTAL1(1,7)-(XTOTAL1(2,7)+XTOTAL1(3,7)+XTOTAL1(4,7)),0)  PICTURE '999999'
   @ ROW,118 SAY MAX(XTOTAL1(1,8)-(XTOTAL1(2,8)+XTOTAL1(3,8)+XTOTAL1(4,8)),0)  PICTURE '999999'
   @ ROW,125 SAY MAX(XTOTAL1(1,9)-(XTOTAL1(2,9)+XTOTAL1(3,9)+XTOTAL1(4,9)),0)  PICTURE '9999999'
   ROW = ROW+1
 ENDIF

 @ ROW,00 SAY REPLICATE('*',132)
 EXIT
ENDDO

DO ENDREPORT         && END THE REPORT OR DISPLAY ON SCREEN
SET DEVICE TO SCREEN
*B602738,1 AMM Erase temporary files
IF USED(WORKFILE)
  USE IN (WORKFILE)
ENDIF
ERASE (gcWorkDir+WORKFILE+'.DBF')
ERASE (gcWorkDir+WORKFILE+'.CDX')

IF USED(lcTmpTKTH)
  USE IN (lcTmpTKTH)
ENDIF
ERASE (gcWorkDir+lcTmpTKTH+'.DBF')
ERASE (gcWorkDir+lcTmpTKTH+'.CDX')
*B602738,1 AMM end

*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : Get title to be displayed (PO or cuttkt or MFG order)
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
*B802027,1 AMM Get the Cuttkt title and return with the Major title
*lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
*RETURN lcTit
lcTitle = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
RETURN gfItemMask('HM')      && Major Title
*B802027,1 AMM end

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : MFOPTR.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : When function of the option grid
*!*************************************************************
*! Called from : MFOPTR.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen
*B602947,1 AMM define status arrays
DECLARE laRpSource[6],laRpTarget[1]
STORE 'Open'       TO laRpSource[1]
STORE 'Hold'       TO laRpSource[2]
STORE 'Canceled'   TO laRpSource[3]
STORE 'Closed'     TO laRpSource[4]
STORE 'Complete'   TO laRpSource[5]
STORE 'Actual'     TO laRpSource[6]



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

*!*************************************************************
*! Name      : lfTranCD
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 05/27/98
*! Purpose   : Display a mover to get transaction types from user
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover()
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
FUNCTION  lfTranCD

PRIVATE laSrce, laDist
DIMENSION laSrce[4], laDist[1]
STORE SPACE(0) TO laDist
laSrce[1] = 'Budget'
laSrce[2] = 'Receipts'
laSrce[3] = 'Damaged'
laSrce[4] = 'Cancelled'
lnCount = 1
IF '1' $ lcTranCD
  laDist[lnCount] = 'Budget'
  lnCount = lnCount + 1
ENDIF
IF '2' $ lcTranCD
  DIMENSION laDist[lnCount]
  laDist[lnCount] = 'Receipts'
  lnCount = lnCount + 1
ENDIF
IF '3' $ lcTranCD
  DIMENSION laDist[lnCount]
  laDist[lnCount] = 'Damaged'
  lnCount = lnCount + 1
ENDIF
IF '4' $ lcTranCD
  DIMENSION laDist[lnCount]
  laDist[lnCount] = 'Cancelled'
ENDIF

=gfMover(@laSrce,@laDist,"Select report transaction types")

lcTranCD = ''
IF ASCAN(laDist,'Budget') # 0
  lcTranCD = lcTranCD+'1'
ENDIF
IF ASCAN(laDist,'Receipts') # 0
  lcTranCD = lcTranCD+'2'
ENDIF
IF ASCAN(laDist,'Damaged') # 0
  lcTranCD = lcTranCD+'3'
ENDIF
IF ASCAN(laDist,'Cancelled') # 0
  lcTranCD = lcTranCD+'4'
ENDIF

*!*************************************************************
*! Name      : lfvVend
*! Developer : Ahmed Mohamed Ibrahim (AMM)
*! Date      : 04/05/99
*! Purpose   : Vaildate vendor
*! Reference : *B602739,1 AMM
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
  lcOldVen = lcRPVend
  =gfApVnBrow(@lcRPVend)
  IF EMPTY(lcRPVend)
    lcRPVend = lcOldVen
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Ahmed Mohamed Ibrahim
*! Date      : 06/07/99
*! Purpose   : Valid function of the status button
*! Ref       : *B602947,1
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
  IF !EMPTY(laRpTarget[lnI])
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'CLOSED'  ,'S',;
                            IIF(laRpTarget[lnI] = 'Canceled'  ,'X',;
                            IIF(laRpTarget[lnI] = 'Complete'  ,'C',;
                            IIF(laRpTarget[lnI] = 'Actual'  ,'A',''))))))
  ENDIF
ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))
