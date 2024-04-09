*:***************************************************************************
*: Program file  : ICGMA150
*: Program desc. : Open To Sale Report
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : AHMED MOHAMED MAHER (AMH)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICGMA150
*:***************************************************************************
*: Modifications : 
*:***************************************************************************
*
SELECT STYLE
SET ORDER TO TAG 'Style'
SET RELATION TO Style INTO ICDEPTDT
SET FILTER TO &lcRpExp
GO TOP

PAGENO     = 00
LINENO     = 00
MAX_LINES  = 58
LMARGIN    = 00

SUB_SAVE   = cStyMajor

*-- If ther are no records show this message and return
STORE 0 TO XTOTP, XTOTN
SCAN
  Y = 1
  *-- loop for 8 sizes
  DO WHILE Y<=8
    ZY = STR(Y,1)
    WKAMT = IIF(lcRpBase='W' ,STYLE.WIP&ZY,STYLE.PLAN&ZY) + STYLE.STK&ZY - STYLE.ORD&ZY
    IF (WKAMT >0 ) .AND. (lcRpPrnt='P' .OR. lcRpPrnt='A')
      XTOTP = XTOTP + WKAMT
      EXIT
    ENDIF
    IF (WKAMT <0 ) .AND. (lcRpPrnt='N' .OR. lcRpPrnt='A')
      XTOTN = XTOTN + WKAMT
      EXIT
    ENDIF
    Y = Y + 1
  ENDDO
  IF XTOTP<>0 .AND. XTOTN<>0
    EXIT
  ENDIF
ENDSCAN
GO TOP
IF (EOF()) .OR. (lcRpPrnt='P' .AND. XTOTP=0) .OR. (lcRpPrnt='N' .AND. XTOTN=0) .OR. (lcRpPrnt='A' .AND. XTOTP=0 .AND. XTOTN=0)
  = gfModalGen("TRM00000B42000",.F.," ",.F.,"NO RECORDS SELECTED FOR REPORT!")
  SELECT STYLE
  SET ORDER TO TAG 'Cstyle'
  SET RELATION TO
  SET FILTER TO
  GO TOP
  RETURN
ENDIF
  
SET DEVICE TO PRINTER
*-- calling the procedure to print the report sorted by user choise from option grid
DO CASE
  CASE lcRpSort = "D"
    SELECT STYLE
    SCAN
      SELECT ICDEPTDT
      IF !EOF()
        EXIT
      ENDIF
      SELECT STYLE
    ENDSCAN
    SELECT STYLE
    IF EOF()
      = gfModalGen("TRM00000B42000",.F.," ",.F.,"NO RECORDS SELECTED FOR REPORT!")
      SET DEVICE TO SCREEN
      SET ORDER TO TAG 'Cstyle'
      SET RELATION TO
      SET FILTER TO
      GO TOP
      RETURN
    ENDIF
    DO lpPrintByD
  CASE lcRpSort = "F"
    DO lpPrintByF
  CASE lcRpSort = "S"
    DO lpPrintByS
ENDCASE

DO ENDREPORT
SET DEVICE TO SCREEN
SELECT STYLE
SET ORDER TO TAG 'Cstyle'
SET RELATION TO
SET FILTER TO
GO TOP

*!*************************************************************
*! Name      : lpPrintByD
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/11/2000
*! Purpose   : To Print The Report Sorted By Dept.
*!*************************************************************
*! Called from : ICGMA150
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpPrintByD()
*!*************************************************************
PROCEDURE lpPrintByD

SELECT STYLE
SET RELATION TO
SELECT ICDEPTDT
SET ORDER TO TAG 'DeptDt'
SET RELATION TO Style INTO STYLE
GO TOP
PAGENO     = 00
ROW        = 99
R_WIDTH    = 'W'
R_TITLE    = [OPEN TO SELL BY DEPARTMENT]
CLEAR TYPEAHEAD
SUB_SAVE   = ' NULL'
STORE 0 TO TOTP, SUBTOTP, TOTN, SUBTOTN
STORE 0 TO XPRICEA,XVALPOS,XSUBVAL,XTOTVAL
DO WHILE INKEY() <> 32
  *-- If the end of page
  IF ROW > 58
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'GMA1500',lcRpTitle,R_WIDTH
    ROW = 5
    *-- if Positive or all
    IF lcRpPrnt = "P" .OR. lcRpPrnt = "A"
      @ ROW,0 SAY 'COLOR  COLOR DESCR.... STYLE DESCRIPTION.    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8     POS     NEG     NET   $ VALUE'
      ROW = ROW + 1
      @ ROW,100 SAY 'O-T-S   O-T-S   O-T-S POS O-T-S'
    ELSE
      @ ROW,0 SAY 'COLOR  COLOR DESCR.... STYLE DESCRIPTION.    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8     POS     NEG     NET'
      ROW = ROW + 1
      @ ROW,100 SAY 'O-T-S   O-T-S   O-T-S'
   ENDIF
   *-- End positive or all
    ROW = 7
  ENDIF
  *-- End if the end of page
  STORE 0 TO XTOTP, XTOTN
  Y = 1
  *-- loop for 8 sizes
  DO WHILE Y<=8
    ZY = STR(Y,1)
    WKAMT = IIF(lcRpBase='W' ,STYLE.WIP&ZY,STYLE.PLAN&ZY) + STYLE.STK&ZY - STYLE.ORD&ZY
    IF (WKAMT >0 ) .AND. (lcRpPrnt='P' .OR. lcRpPrnt='A')
      XTOTP = XTOTP + WKAMT
    ENDIF
    IF (WKAMT <0 ) .AND. (lcRpPrnt='N' .OR. lcRpPrnt='A')
      XTOTN = XTOTN + WKAMT
    ENDIF
    Y = Y + 1
  ENDDO
  *-- End loop for 8 sizes
  *-- Case Positive or negative
  DO CASE
    CASE lcRpPrnt<>'N' .AND. XTOTP < lnRpMini
      IF .NOT. EOF()
        SKIP
        LOOP
      ENDIF
    CASE lcRpPrnt='N' .AND. XTOTN > lnRpMini
      IF .NOT. EOF()
        SKIP
        LOOP
      ENDIF
  ENDCASE
  *-- End case Positive or negative
  *-- If the the positive total and negative total =0 and not in the end of file
  *-- don't print this line
  IF XTOTP = 0 .AND. XTOTN = 0 .AND. !EOF()
    SKIP
    LOOP
  ENDIF
  *-- printing the dept. desc. for the frist time
  IF SUB_SAVE = ' NULL'
    @ ROW,0 SAY REPLICATE ('=',132)
    ROW = ROW + 1
    @ ROW,0 SAY '** '
    @ ROW,4 SAY DEPT
    @ ROW,12 SAY IIF(SEEK(DEPT,'ICDEPTHD'),ICDEPTHD.cDeptDesc,SPACE(30))
    SUB_SAVE = DEPT
    ROW = ROW + 1
  ENDIF
  *-- printing the footer of dept. group
  IF ICDEPTDT.DEPT <> ICDEPTHD.DEPT OR EOF()
    @ ROW,000 SAY '** SubTotal For '+ SUB_SAVE+' **'
    @ ROW,098 SAY SUBTOTP  PICTURE '9999999'
    @ ROW,106 SAY SUBTOTN  PICTURE '9999999'
    @ ROW,114 SAY (SUBTOTP + SUBTOTN) PICTURE '9999999'
    IF lcRpPrnt = "P" .OR. lcRpPrnt  ="A"
      @ ROW,123 SAY XSUBVAL PICTURE '99999999'
    ENDIF
    SUB_SAVE  = DEPT
    TOTP      = TOTP + SUBTOTP
    TOTN      = TOTN + SUBTOTN
    XTOTVAL   = XTOTVAL + XSUBVAL
    XSUBVAL   = 0
    SUBTOTP   = 0
    SUBTOTN   = 0
    ROW       = ROW + 2
    @ ROW,0 SAY '** '
    @ ROW,04 SAY DEPT
    @ ROW,12 SAY IIF(SEEK(DEPT,'ICDeptHd'),ICDeptHd.cDeptDesc,SPACE(30))
    ROW = ROW + 1
  ENDIF
  *-- If the end of page loop to check it in the begining of the loop
  IF ROW > 58
    LOOP
  ENDIF
  *-- If end of file exit to print the report footer
  IF EOF()
   EXIT
  ENDIF
  *-- Printing the details of each group
  @ ROW,00 SAY SUBSTR(STYLE,lnClrPo,lnColorLen)
  XCOLOR = SUBSTR(STYLE,lnClrPo,lnColorLen)
  SELECT CODES
  *-- Printing the color discrep.
  IF SEEK ('D'+XCOLOR+'N')
    @ ROW,7 SAY SUBSTR(cDiscrep,1,15)                     && Color Description
  ELSE
    IF SEEK('N'+XCOLOR+'N')
      @ ROW,7 SAY SUBSTR(cDiscrep,1,15)                   && Color Description
    ENDIF
  ENDIF
  SELECT ICDEPTDT
  @ ROW,23 SAY SUBSTR(STYLE.DESC,1,18)          && Style Description
  XPRICEA = STYLE.PRICEA
  XCOL = 42
  Y = 1
  *-- loop for printing data of 8 sizes
  DO WHILE Y <= 8
    ZY = STR(Y,1)
    WKAMT = IIF(lcRpBase='W',STYLE.WIP&ZY,STYLE.PLAN&ZY) + STYLE.STK&ZY - STYLE.ORD&ZY
    DO CASE
      CASE (WKAMT >= 0 .AND. lcRpPrnt='P')
        @ ROW,XCOL SAY WKAMT PICTURE '999999'
      CASE (WKAMT < 0 .AND. lcRpPrnt='N')
        @ ROW,XCOL SAY WKAMT PICTURE '999999'
      CASE (lcRpPrnt ='A')
        @ ROW,XCOL SAY WKAMT PICTURE '999999'
    ENDCASE
    Y=Y+1
    XCOL = XCOL+7
  ENDDO
  @ ROW,098 SAY XTOTP PICTURE '9999999'
  @ ROW,106 SAY XTOTN PICTURE '9999999'
  @ ROW,114 SAY (XTOTP + XTOTN) PICTURE '9999999'
  IF lcRpPrnt = "P" .OR. lcRpPrnt = "A"
    XVALPOS = XPRICEA * XTOTP
    @ ROW,123 SAY XVALPOS PICTURE '99999999'
  ENDIF
  SUBTOTP = SUBTOTP + XTOTP
  SUBTOTN = SUBTOTN + XTOTN
  XSUBVAL = XSUBVAL + XVALPOS
  ROW     = ROW+1
  SKIP
ENDDO !EOF()

*-- Printing the Report Footer
TOTP    = IIF(TOTP=0, SUBTOTP, TOTP)
TOTN    = IIF(TOTN=0, SUBTOTN, TOTN)
XTOTVAL = IIF(XTOTVAL=0, XSUBVAL, XTOTVAL)
ROW = ROW + 2
@ ROW,000 SAY '*** Total ***'
@ ROW,098 SAY TOTP PICTURE '9999999'
@ ROW,106 SAY TOTN PICTURE '9999999'
@ ROW,114 SAY (TOTP + TOTN) PICTURE '9999999'
IF lcRpPrnt = "P" .OR. lcRpPrnt = "A"
  @ ROW,123 SAY XTOTVAL PICTURE '99999999'
ENDIF
*-- restore the relation as in sorting by style case
SET RELATION TO
SET ORDER TO TAG 'DeptDts'
SELECT STYLE
SET RELATION TO Style INTO ICDEPTDT

*!*************************************************************
*! Name      : lpPrintByF
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/11/2000
*! Purpose   : To Print The Report Sorted By Fabric
*!*************************************************************
*! Called from : ICGMA150
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpPrintByF()
*!*************************************************************
PROCEDURE lpPrintByF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+
*STYLE        COLOR  STYLE DESCR.........    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8     POS
*                                                                                                   O-T-S
*123456789*12 123456 12345678901234567890 123456 123456 123456 123456 123456 123456 123456 123456 1234567
*SUB TOTAL                                                                                        1234567
*                                                                                                 -------
*DIVISION TOTAL                                                                                   1234567
*                                                                                                 =======
*GRAND TOTAL                                                                                      1234567
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+


R_TITLE = 'QUANTITY OPEN TO SELL'
R_WIDTH = 'W'
SELECT STYLE
PRIVATE lcStyTag
lcStyTag = ORDER()
INDEX ON CDIVISION + FABRIC + STYLE TAG DivFabSrt OF (gcWorkDir+"StyleFab.CDX")
GOTO TOP
MAXLN1 = 55
MAXLN2 = 50

IF lcRpPrnt1 = 'P' 
  HL3='STYLE        COLOR  COLOR DESC..... STYLE DESCRIPTION..    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8        POS'
ELSE
  HL3='STYLE        COLOR  COLOR DESC..... STYLE DESCRIPTION..    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8        NEG'
ENDIF
HL4='O-T-S'
*---------------------------
* SAVE CONTROL BREAK DATA
*---------------------------
SELECT STYLE
ROW  =99
PAGENO=0
TDIVISION = CDIVISION
STORE ' ' TO TSTYLE
STORE 0 TO DIVOTS, FABOTS, GRANDOTS
SELECT CODES
IF SEEK ('D'+STYLE.CDIVISION+'N')
  STORE SUBSTR(CDiscrep,1,15) TO PDIV, HPDIV
ELSE
  SEEK 'N'+STYLE.CDIVISION+'N'
  STORE SUBSTR(CDiscrep,1,15) TO PDIV, HPDIV
ENDIF
SELECT STYLE
STORE CStyMajor  TO PSTYLE, HPSTYLE
STORE FABRIC TO PFABRIC,HPFABRIC,TFABRIC

STORE ' ' TO XFAB_SAVE           && TO STORE FABRIC

*** A FLAG TO AVOID PRINTTING FABRIC NAME & DESC BEFORE EVRY STYLE
*** BELONGING TO THE SAME FABRIC
XFAB_FLAG = .T.

*** OPEN TO SELL REPORT BY DIVISION, FABRIC GROUP
DO WHILE INKEY()<>32
  R_TITLE = 'OPEN TO SELL BY FABRIC'
  SELECT STYLE
    IF EOF()
      EXIT
    ENDIF
  *** COMPUTE OPEN TO SELL TOTAL STYLE
  STORE CStyMajor TO TSTYLE, PSTYLE, HPSTYLE
  XFAB_FLAG = .T.
  OTS = 0

  *** A FLAG TO INDICATE CASE OF END OF FILE BUT TOTALS HAVE NOT
  *** BEEN PRINTED YET
  XEND_FLAG = .T.
  SAVREC = RECNO()
  DO WHILE CStyMajor = TSTYLE
    X = 1
    DO WHILE X<=8
      Z = STR(X,1)
      WKAMT = IIF(lcRpBase='W',WIP&Z,PLAN&Z) + STK&Z - ORD&Z  && TAK 07/18/94
      IF (WKAMT >0 .AND. lcRpPrnt1 = 'P') .OR. (WKAMT < 0 .AND. lcRpPrnt1='N')
        OTS = OTS + WKAMT
      ENDIF
      X = X+1
    ENDDO
    SKIP
  ENDDO

  IF (lcRpPrnt1 = 'P' .AND. OTS<lnRpMini) .OR. (lcRpPrnt1 = 'N' .AND. OTS>lnRpMini)
    *** IF END OF FILE ENCOUNTERED AND THERE IS A STYLE BELONGING TO
    *** THAT FABRIC HAS A QUANTITIES LESS THAN XMINQTY OR GREATER
    *** THAN XMINQTY DO NOT EXIT BEFORE PRINT TOTALS.
    IF EOF()
      XEND_FLAG=.F.
    ELSE
      *** IF THE LAST LINE OF THIS FABRIC HAS A QTY. LESS OR GREATER THAN
      *** XMINQTY [ DEPENDS ON THE CONDISION +VE OR -VE ] DO NOT GET THE
      *** NEXT FABRIC UNTIL PRINTING THE OLD FABRIC TOTALS.
      IF FABRIC <> TFABRIC
        IF FABOTS <> 0
          ROW = ROW + 1
          @ ROW,00 SAY '** SUB TOTAL **'
          @ ROW,112 SAY FABOTS
          ROW = ROW+2
        ENDIF
        DIVOTS = DIVOTS + FABOTS
        FABOTS = 0
        SELECT STYLE
        STORE FABRIC TO TFABRIC, PFABRIC, HPFABRIC
      ENDIF

      *** IF THE LAST LINE OF THIS DIVISION HAS A QTY. LESS OR GREATER
      *** THAN XMINQTY [ DEPENDS ON THE CONDISION +VE OR -VE ] DO NOT GET
      *** THE NEXT DIVISION UNTIL PRINTING THE OLD DIVISION TOTALS.

      IF CDIVISION <> TDIVISION
        IF DIVOTS <> 0
          @ ROW,00 SAY REPLICATE('-',130)
          ROW=ROW+1
          @ ROW,00 SAY '** DIVISION TOTAL **'
          @ ROW,112 SAY DIVOTS
        ENDIF
        GRANDOTS = GRANDOTS + DIVOTS
        DIVOTS   = 0
        SELECT STYLE
        TDIVISION = CDIVISION
        ROW = 99
      ENDIF
      LOOP
    ENDIF
  ENDIF

  IF XEND_FLAG
    FABOTS = FABOTS + OTS
    IF ROW>= MAXLN1
      PAGENO=PAGENO+1
      SELECT CODES
      IF SEEK ('D'+TDIVISION+'N')
        PDIV = SUBSTR(CDiscrep,1,15)
      ELSE
        SEEK 'N'+TDIVISION+'N'
        PDIV = SUBSTR(CDiscrep,1,15)
      ENDIF
      SELECT STYLE
      DO RPT_HDR WITH 'GMA1500',lcRpTitle,R_WIDTH
      @ 05,00 SAY 'DIVISION: '+ TDIVISION +' '+ PDIV
      @ 06,00 SAY HL3
      @ 07,117 SAY HL4
      @ 08,00 SAY REPLICATE('=',132)
      ROW = 9
      PFABRIC = HPFABRIC
      PSTYLE  = HPSTYLE
    ENDIF
    GOTO SAVREC
    IF XFAB_FLAG
      @ ROW,00 SAY "**"
      @ ROW,03 SAY PFABRIC
      SELECT FABRIC
      IF SEEK(HPFABRIC)
        HPDESC = DESC
      ELSE
        STORE SPACE(20) TO HPDESC
      ENDIF
      SELECT STYLE
      @ ROW,13 SAY HPDESC
      ROW = ROW + 2
      STORE  PFABRIC TO XFAB_SAVE
    ENDIF

    *** LOOP STYLE COLORS
    DO WHILE CStyMajor = TSTYLE
      IF ROW>= MAXLN1
        PAGENO=PAGENO+1
        SELECT CODES
        IF SEEK ('D'+TDIVISION+'N')
          PDIV = SUBSTR(CDiscrep,1,15)
        ELSE
          SEEK 'N'+TDIVISION+'N'
          PDIV = SUBSTR(CDiscrep,1,15)
        ENDIF
        DO RPT_HDR WITH 'GMA1500',lcRpTitle,R_WIDTH
        @ 05,000 SAY 'DIVISION: '+ TDIVISION +' '+ PDIV
        @ 06,000 SAY HL3
        @ 07,117 SAY HL4
        @ 08,000 SAY REPLICATE('=',132)
        ROW=9
      ENDIF
        
      SELECT STYLE
      @ ROW,00 SAY ALLTRIM(HPSTYLE)
      @ ROW,13 SAY SUBSTR(STYLE,lnClrPo,lnColorLen)
      STORE SUBSTR(STYLE,lnClrPo,lnColorLen) TO XCOLOR
      SELECT CODES
      IF SEEK ('D'+XCOLOR+'N')
        @ ROW,20 SAY SUBSTR(CDiscrep,1,15)      && Color Description
      ELSE
        IF SEEK ('N'+XCOLOR+'N')
          @ ROW,20 SAY SUBSTR(CDiscrep,1,15)      && Color Description
        ENDIF
      ENDIF
      SELECT STYLE
      @ ROW,36 SAY SUBSTR(DESC,1,12)          && Style Description
      XCOL=56
      Y=1
      STORE 0 TO XTOTP
      DO WHILE Y<=8
        ZY = STR(Y,1)
        WKAMT = IIF(lcRpBase='W',WIP&ZY,PLAN&ZY)+STK&ZY-ORD&ZY  && TAK 07/18/94
        IF (WKAMT >0 .AND. lcRpPrnt1='P') .OR. (WKAMT<0 .AND. lcRpPrnt1='N')
          XTOTP = XTOTP + WKAMT
        ENDIF
        DO CASE
          CASE (WKAMT >= 0 .AND. lcRpPrnt1='P')
            @ ROW,XCOL SAY WKAMT PICTURE '999999'
          CASE (WKAMT < 0 .AND. lcRpPrnt1='N')
            @ ROW,XCOL SAY WKAMT PICTURE '999999'
          CASE (lcRpPrnt1 ='A')
            @ ROW,XCOL SAY WKAMT PICTURE '999999'
        ENDCASE
        Y=Y+1
        XCOL=XCOL+7
      ENDDO
      @ ROW,XCOL SAY XTOTP
      ROW=ROW+1
      SKIP
    ENDDO
    *** ASSIGN TRUE TO THE FLAG IF IT IS A NEW FABRIC TO PRINT
    *** NAME AND DESC.
    XFAB_FLAG=IIF(XFAB_SAVE<>FABRIC,.T.,.F.)
  ENDIF
  BREAK =0
  DO CASE
    CASE EOF()
      BREAK = 4
    CASE CDIVISION <> TDIVISION
      BREAK = 3
    CASE FABRIC <> TFABRIC
      BREAK = 2
  ENDCASE

  IF ROW>= MAXLN1 .AND. BREAK>= 3  .AND. XEND_FLAG
    PAGENO=PAGENO+1
    SELECT CODES
    IF SEEK ('D'+TDIVISION+'N')
      PDIV = SUBSTR(CDiscrep,1,15)
    ELSE
      SEEK 'N'+TDIVISION+'N'
      PDIV = SUBSTR(CDiscrep,1,15)
    ENDIF
    DO RPT_HDR WITH 'GMA1500',lcRpTitle,R_WIDTH
    @ 05,00 SAY 'DIVISION: '+ TDIVISION +' '+ PDIV
    @ 06,00 SAY HL3
    @ 07,117 SAY HL4
    ROW=8
    PFABRIC = HPFABRIC
    PSTYLE  = HPSTYLE
  ENDIF

  *** FABRIC BREAK
  IF BREAK >=2 .AND. FABOTS <>0
    ROW = ROW + 1
    @ ROW,00 SAY '** SUB TOTAL **'
    @ ROW,112 SAY FABOTS
    ROW = ROW + 2
    DIVOTS = DIVOTS + FABOTS
    FABOTS = 0
    SELECT STYLE
    IF !EOF()
      STORE FABRIC TO TFABRIC, PFABRIC, HPFABRIC
    ENDIF
  ENDIF

  *** DIVISION BREAK
  IF BREAK >=3
    @ ROW,00 SAY REPLICATE('-',130)
    ROW=ROW+1
    @ ROW,00 SAY '** DIVISION TOTAL **'
    @ ROW,112 SAY DIVOTS
    GRANDOTS = GRANDOTS + DIVOTS
    DIVOTS   = 0
    IF !EOF()
      SELECT STYLE
      TDIVISION = CDIVISION
      ROW = 99
      LOOP
    ENDIF
  ENDIF
    
  *** GRAND TOTAL BREAK
  IF BREAK >=4 .AND. GRANDOTS <> 0
    ROW = ROW +1
    IF ROW >= 60
      ROW = 1
    ENDIF
    @ ROW,00 SAY REPLICATE('=',130)
    ROW = ROW + 1
    @ ROW,00 SAY '** GRAND TOTAL **'
    @ ROW,112 SAY GRANDOTS
    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',130)
    ROW = ROW+2
    GRANDOTS = 0
    EXIT
  ENDIF
ENDDO

SELECT STYLE
SET INDEX TO
ERASE (gcWorkDir+"StyleFab.CDX")
SET ORDER TO (lcStyTag)

*!*************************************************************
*! Name      : lpPrintByS
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/11/2000
*! Purpose   : To Print The Report Sorted By Style
*!*************************************************************
*! Called from : ICGMA150
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpPrintByD()
*!*************************************************************
PROCEDURE lpPrintByS

  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+...1....+....2....+....3.
  *COLOR  COLOR DESCR.... STYLE DESCRIPTION.    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8     POS     NEG     NET   $ VALUE
  *                                                                                                    O-T-S   O-T-S   O-T-S POS O-T-S
  *123456 123456789*12345 123456789*12345678 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567 1234567  12345678
  *** 123456789*                                SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+...1....+....2....+....3.

  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+...1....+....2....+
  *COLOR  COLOR DESCR.... STYLE DESCRIPTION.    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8     POS     NEG     NET
  *                                                                                                    O-T-S   O-T-S   O-T-S
  *123456 123456789*12345 123456789*12345678 123456 123456 123456 123456 123456 123456 123456 123456 1234567 1234567 1234567
  *** 123456789*                                SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8
  *0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+...1....+....2....+

GO TOP
PAGENO     = 00
ROW        = 99
R_WIDTH    = 'W'
R_TITLE    = [STYLE QUANTITIES OPEN TO SELL SUMMARY]
SELECT STYLE

SUB_SAVE   = ''
STORE 0 TO TOTP, SUBTOTP, TOTN, SUBTOTN
STORE 0 TO XPRICEA,XVALPOS,XSUBVAL,XTOTVAL

DO WHILE INKEY() <> 32
  IF ROW > 58
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'GMA1500',lcRpTitle,R_WIDTH
    ROW = 5
    IF lcRpPrnt = "P" .OR. lcRpPrnt = "A"
      @ ROW,0 SAY 'COLOR  COLOR DESCR.... STYLE DESCRIPTION.    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8     POS     NEG     NET   $ VALUE'
      ROW = ROW + 1
      @ ROW,100 SAY 'O-T-S   O-T-S   O-T-S POS O-T-S'
    ELSE
      @ ROW,0 SAY 'COLOR  COLOR DESCR.... STYLE DESCRIPTION.    SZ1    SZ2    SZ3    SZ4    SZ5    SZ6    SZ7    SZ8     POS     NEG     NET'
      ROW = ROW + 1
      @ ROW,100 SAY 'O-T-S   O-T-S   O-T-S'
    ENDIF
      ROW = 7
  ENDIF

  STORE 0 TO XTOTP, XTOTN

  Y = 1
  DO WHILE Y<=8
    ZY = STR(Y,1)
    WKAMT = IIF(lcRpbase='W',WIP&ZY,PLAN&ZY) + STK&ZY - ORD&ZY  && TAK 07/18/94
    IF (WKAMT >0 ) .AND. (lcRpPrnt='P' .OR. lcRpPrnt='A')
      XTOTP = XTOTP + WKAMT
    ENDIF
    IF (WKAMT <0 ) .AND. (lcRpPrnt='N' .OR. lcRpPrnt='A')
      XTOTN = XTOTN + WKAMT
    ENDIF
    Y = Y + 1
  ENDDO

  DO CASE
    CASE lcRpPrnt<>'N' .AND. XTOTP<lnRpMini
      IF .NOT. EOF()
        SKIP
        LOOP
      ENDIF
    CASE lcRpPrnt='N' .AND. XTOTN>lnRpMini
      IF .NOT. EOF()
        SKIP
        LOOP
      ENDIF
  ENDCASE

  IF XTOTP = 0 .AND. XTOTN = 0 .AND. !EOF()
    SKIP
    LOOP
  ENDIF

  IF EMPTY(SUB_SAVE)
    @ ROW,0 SAY REPLICATE ('=',132)
    ROW = ROW + 1
    @ ROW,0 SAY '** '
    XSCALE = GETSCALE(SCALE,SPACE(2))
    @ ROW,4 SAY cStyMajor
    @ ROW,43 SAY IIF(EOF(),' ',XSCALE)
    SUB_SAVE = cStyMajor+SCALE
    ROW = ROW + 1
  ENDIF

  IF SUB_SAVE <> cStyMajor+ SCALE
    @ ROW,000 SAY '** SubTotal **'
    @ ROW,098 SAY SUBTOTP  PICTURE '9999999'
    @ ROW,106 SAY SUBTOTN  PICTURE '9999999'
    @ ROW,114 SAY (SUBTOTP + SUBTOTN) PICTURE '9999999'
    IF lcRpPrnt = "P" .OR. lcRpPrnt = "A"
      @ ROW,123 SAY XSUBVAL PICTURE '99999999'
    ENDIF
    SUB_SAVE  = cStyMajor + SCALE
    TOTP      = TOTP+SUBTOTP
    TOTN      = TOTN+SUBTOTN
    XTOTVAL   = XTOTVAL+XSUBVAL
    XSUBVAL   = 0
    SUBTOTP   = 0
    SUBTOTN   = 0
    ROW       = ROW + 2
    @ ROW,0 SAY '** '
    XSCALE = GETSCALE(SCALE,SPACE(2))
    @ ROW,4 SAY cStyMajor
    @ ROW,43 SAY IIF(EOF(),' ',XSCALE)
    ROW = ROW + 1
  ENDIF
  IF ROW > 58
    LOOP
  ENDIF
  IF EOF()
    EXIT
  ENDIF

  @ ROW,00 SAY SUBSTR(STYLE,lnClrPo,lnColorLen)
  XCOLOR=SUBSTR(STYLE,lnClrPo,lnColorLen)
  SELECT CODES
  IF SEEK ('D'+XCOLOR+'N')
    @ ROW,7 SAY SUBSTR(cDiscrep,1,15)                     && Color Description
  ELSE
    IF SEEK('N'+XCOLOR+'N')
      @ ROW,7 SAY SUBSTR(cDiscrep,1,15)                   && Color Description
    ENDIF
  ENDIF
  SELECT STYLE

  @ ROW,23 SAY SUBSTR(DESC,1,18)          && Style Description
  XPRICEA = PRICEA

  XCOL = 42
  Y = 1
  DO WHILE Y <= 8
    ZY = STR(Y,1)
    WKAMT = IIF(lcRpBase='W',WIP&ZY,PLAN&ZY) + STK&ZY - ORD&ZY
    DO CASE
      CASE (WKAMT >= 0 .AND. lcRpPrnt='P')
        @ ROW,XCOL SAY WKAMT PICTURE '999999'
      CASE (WKAMT < 0 .AND. lcRpPrnt='N')
        @ ROW,XCOL SAY WKAMT PICTURE '999999'
      CASE (lcRpPrnt ='A')
        @ ROW,XCOL SAY WKAMT PICTURE '999999'
    ENDCASE
    Y=Y+1
    XCOL=XCOL+7
  ENDDO

  @ ROW,098 SAY XTOTP PICTURE '9999999'
  @ ROW,106 SAY XTOTN PICTURE '9999999'
  @ ROW,114 SAY (XTOTP + XTOTN) PICTURE '9999999'

  IF lcRpPrnt = "P" .OR. lcRpPrnt = "A"
    XVALPOS = XPRICEA * XTOTP
    @ ROW,123 SAY XVALPOS PICTURE '99999999'
  ENDIF

  SUBTOTP = SUBTOTP + XTOTP
  SUBTOTN = SUBTOTN + XTOTN
  XSUBVAL = XSUBVAL + XVALPOS
  ROW     = ROW+1
  SKIP
ENDDO !EOF()

TOTP   =IIF(TOTP=0, SUBTOTP, TOTP)
TOTN   =IIF(TOTN=0, SUBTOTN, TOTN)
XTOTVAL=IIF(XTOTVAL=0, XSUBVAL, XTOTVAL)

ROW = ROW + 2
@ ROW,000 SAY '*** Total ***'
@ ROW,098 SAY TOTP PICTURE '9999999'
@ ROW,106 SAY TOTN PICTURE '9999999'
@ ROW,114 SAY (TOTP + TOTN) PICTURE '9999999'
IF lcRpPrnt = "P" .OR. lcRpPrnt = "A"
  @ ROW,123 SAY XTOTVAL PICTURE '99999999'
ENDIF

*!*************************************************************
*! Name      : lfvDept
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2000
*! Purpose   : To Valid the Dept field
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDept()
*!*************************************************************
FUNCTION lfvDept

PRIVATE lcDeptVar
lcDeptVar = VARREAD()
&lcDeptVar. = UPPER(&lcDeptVar.)

IF !SEEK(&lcDeptVar.,"ICDeptHD")
  &lcDeptVar. = lfDeptBrow()
ENDIF

*!*************************************************************
*! Name      : lfDeptBrow
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2000
*! Purpose   : To Browse the ICDEPTHD file
*!*************************************************************
*! Called from : lfvDept
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ICDEPTHD.DEPT
*!*************************************************************
*! Example     : = lfDeptBrow()
*!*************************************************************
FUNCTION lfDeptBrow
PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl,lcRetVal
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow,lcRetVal
PRIVATE laTemp
DIMENSION laTemp[1]

lnCurAlias = SELECT(0)

lcFields    = "Dept"
lcBrFields  = [Dpet:H='Deaprtment',]+;
              [cDeptDesc:H='Description',]+;
              [cStyGroup:H='Style Group"]
lcFile_Ttl  = 'Departements'
          
SELECT ICDeptHd
PRIVATE lcStyTag
lcStyTag = ORDER()
INDEX ON cDeptDesc TAG DeptUnique OF (gcWorkDir+"DeptUnqe.CDX") UNIQUE

IF gfBrows("''",lcFields,'laTemp',lcFile_Ttl,.F.,"ICDeptHd")
  lcRetVal = laTemp[1]
ENDIF  

SET INDEX TO
ERASE (gcWorkDir+"DeptUnqe.CDX")
SET ORDER TO (lcStyTag)
SELECT(lnCurAlias)

RETURN lcRetVal

*!*************************************************************
*! Name      : lfvFabric
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2000
*! Purpose   : To Valid the Fabric Field
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpPrintByD()
*!*************************************************************
FUNCTION lfvFabric

PRIVATE lcFabVar,lcFab
lcFabVar = VARREAD()
&lcFabVar=UPPER(&lcFabVar)
lcFab    = &lcFabVar

IF !SEEK(&lcFabVar.,"Fabric")
  = FaBrow(@lcFab,'*')
  &lcFabVar = lcFab
ENDIF

*!*************************************************************
*! Name      : lfvPrnt
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2000
*! Purpose   : To redraw the option Grid when valid lcRpPrnt
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrnt()
*!*************************************************************
FUNCTION lfvPrnt

DO CASE
  CASE lcRpPrnt = 'A'
    lnRpMini = 0
  CASE lcRpPrnt = 'P'
    lnRpMini = 1
  CASE lcRpPrnt = 'N'
    lnRpMini = -1
ENDCASE
CLEAR READ

*!*************************************************************
*! Name      : lfvPrnt1
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2000
*! Purpose   : To redraw the option grid when valid lcRpPrnt1
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrnt1()
*!*************************************************************
FUNCTION lfvPrnt1

DO CASE
  CASE lcRpPrnt1 = 'P'
    lnRpMini = 1
  CASE lcRpPrnt1 = 'N'
    lnRpMini = -1
ENDCASE
CLEAR READ

*!*************************************************************
*! Name      : lfvSort
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2000
*! Purpose   : To redraw the option grid when valid lcRpSort
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSort()
*!*************************************************************
FUNCTION lfvSort

DO CASE
  CASE lcRpSort = 'S' .OR. lcRpSort = 'D'
    =lfvPrnt()
  CASE lcRpSort = 'F'
    IF lcRpPrnt='A'
      lcRpPrnt='P'
    ENDIF
    =lfvPrnt1()
ENDCASE
CLEAR READ

*!*************************************************************
*! Name      : lfwRpWhen
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2000
*! Purpose   : When function of the Option Grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRpWhen()
*!*************************************************************
FUNCTION lfwRpWhen

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lnFreeLen  = LEN(lcNonMajPi)

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]
      
      lnColorLen = LEN(lcNonMajPi)

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      
      lcColorTt  = lcNonMajT
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.
SELECT STYLE
GO TOP

*!*************************************************************
*! Name      : lfNonMaj
*! Developer : AHMED MAHER (AMH)
*! Date      : 05/08/2000
*! Purpose   : To initial value of variables in option grid
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
FUNCTION lfNonMaj

PARAMETERS lcvar
*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lnFreeLen  = LEN(lcNonMajPi)

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]
      
      lnColorLen = LEN(lcNonMajPi)

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      
      lcColorTt  = lcNonMajT
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.