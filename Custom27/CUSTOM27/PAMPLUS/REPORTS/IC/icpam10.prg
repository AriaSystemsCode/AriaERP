*:***************************************************************************
*: Program file  : ICPAM100 (Convetred from 26 to 27  FOR PAM PLUS)
*: Program desc. : Special Cut & Sold
*: System        : Aria Advantage Series.
*: Date          : 08/10/1999
*: Module        : Inventory Control (IC)
*: Developer     : Adel Mohammed El Gazzar (ADEL)
*: NOTE          : CUTTKT is Not Needed
*: Refer To (C101629)
*:***************************************************************************
*: Modifications
*B603955,1 ABD 10/18/2000 Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD            Failed to convert 00 to 2000, it converts it to 1900.
*:***************************************************************************
*--- Variables Used For This Program
*-----------------------------------
*--(1) Variables created in the OG
*--llRPBySize    && Print Cut & Sold By size?
*--lcRPWIPSta    && Print WIP Summary/Detail  
*--lcRPWIPSor    && Sort WIP by              
*--lcRPSalSta    && Print Sal. Summary/Detail
*--lcRPSalSor    && Sort Sales by            
*--llRPHolOrd    && Include S. order on hold 
*--lcMajTtl      && Holds the major tilte
*--lcRPDomImp    && Domestic/Imported        
*--lcRpFact      && Only this Factory
*--llRpPrntNo    && Print Notepad
*--ldRPSLDate    && First  S/O Comp. date.
*--ldRPSHDate    && Second S/O Comp. date.
*--ldRPPLDate    && First  Prod. Order Comp. date.
*--ldRPPHDate    && Second Prod. Order Comp. date.
*--(2) Variables created in this Progam.
*--llWipDet      && Will we print the WIP in Detail? 
*--llOrdDet      && Will we print the S/O in Detail? 
*--lcStatus      && Holds the Order status.
*--lcWrkTmp      && Holds the work temp file.
*--lcPoTmp       && Holds the prod.so temp file.
*--lcOrdTmp      && Holds the Order temp file. 
*--lcConsol
lcConsol = 'N'
**--- Files used in this program
*-------------------------------
*--(1) Files opened in the OG
* Style    ORDER style    (style)
* Scale    ORDER scale    (scale)
* PosHdr   ORDER poshdr   (cstytype+po)
* PosLn    ORDER poslns   (style+cstytype+po+STR(lineno,6)+trancd)
* OrdHdr   ORDER ordhdr   (cordtype+order)
* OrdLine  ORDER ordlines (style+DTOS(complete)+cordtype+order+store+STR(lineno,6))
* Customer ORDER customer (type+account+store)
* ApVendor ORDER vencode  (cvendcode)
* MfGoprhd ORDER Mfgoprhd (cimtyp+ctktno+coprcode)
* Fabric   ORDER cfabric  (fabric)
*--(2) Files opened in this program
* lcPoTmp      temp file.
* lcOrdTmp     temp file.
* lcWrkTmp     temp file. 
**--- Relations set in this program
*-------------------------------
*--(1) Relations set in the OG
* (Posln->Poshdr)   SET RELATION TO Posln.cstytype+ Posln.po INTO Poshdr ADDITIVE
* (Ordline->Ordhdr) SET RELATION TO Ordline.cordtype+ Ordline.order INTO Ordhdr ADDITIVE
*--(2) Relations set in this program
* (Style->Scale)    SET RELATION TO 'S'+SCALE INTO SCALE
* (lcPoTmp->POSHDR) SET RELATION TO cstytype+ po INTO Poshdr ADDITIVE  
* (lcOrdTmp->ORDHDR)SET RELATION TO cordtype+ order INTO Ordhdr ADDITIVE
*--Set the Details flags
*--Will we print the WIP in Detail?
llWipDet = (lcRPWIPSta = 'D')
*--Will we print the S/O in Detail? 
llOrdDet = (lcRPSalSta = 'D')
lcStatus = IIF(!llRPHolOrd,'O','HO')
*--Set the needed filter.
SELECT Style
lcRpExp = lcRpExp +'.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'
SET FILTER TO &lcRpExp
GO TOP
IF EOF()
  *-- Message : There are no records to display...!
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*--Set rlation to sale file.
SET RELATION TO 'S'+SCALE INTO SCALE ADDITIVE
*--Collect data
=lfColData()
*-------------------
*--STart printing
*-------------------
PAGENO=0
ROW=99
XTIME=TIME()
R_WIDTH='W'
*--Note : in 26 program the (Consolidate Company? Y/N) option was obtained in lcConsol VR. from th OG 
*--       but was remarked without any documentation so the variable always carried  'N'
*--       as it had been previously intilialized. So I deleted any condition having lcConsol = 'Y'
*--       as it will never evaluates .T.
DO CASE
  CASE (llWipDet .OR. llOrdDet) .AND. lcConsol='N'
    R_TITLE = 'SPECIAL CUT & SOLD - DETAIL REPORT'
  CASE lcConsol='N'
    R_TITLE = 'SPECIAL CUT & SOLD - SUMMARY REPORT'
ENDCASE
SET DEVICE TO PRINT
IF !llRPBySize
  *-- No Breakdown In Size
  DO ICPAM101
ELSE
  *-- Breakdown In Size
  DO ICPAM102
ENDIF
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN

****************************************************************************
* PROG: ICPAM101.PRG
* DATE: 08/25/1999
* DESC: REPORT WITHOUT SIZE BREAKDOWN
* MODI: Print PO Notepad After PO If Requested (Detail Report Only)
* AUTH: Adel Mohammed El Gazzar (ADEL)
****************************************************************************
PROCEDURE ICPAM101

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL330              123456789012345678901234567890 - SPECIAL CUT & SOLD REPORT          MM/DD/YY
*PAGE: 123                                                                               HH:MM:SS
*
*------------------------------------------------------------------------------------------------
*XXXXXXX XXXXXXXXXXXXXXXXXXXX       XXX      XXX      XXX      XXX      XXX      XXX      XXX ........ PO Notepad.........     TOTAL
*PO #   VENDOR COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999                               99999999
*     P.O. Subtotals ........  99999999 99999999 99999999 99999999 99999999 99999999 99999999 xxxxxxxxxxxxxxxxxxxxxxxxxxxx 999999999
*
*                            ====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===
*                            GRAND TOTALS:  STOCK: 12345678   W.I.P: 12345678   ORDERS: 12345678   O.T.S: 12345678
*                            ====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===
****************************************************************************

*-- Declare & Initialize Variables & Array
STORE SPACE(6)  TO XPO, XORDER
STORE SPACE(8)  TO XVENDOR
STORE SPACE(5)  TO XACCOUNT
STORE SPACE(6)  TO lcColor
STORE CTOD('  /  /  ') TO XDATE
STORE 0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD, TEMP
DIMENSION lcColor(10),XCLR(10),XSTK(50),XPOS(10),XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10)
*--DIMENSION CHANGED IN PRECEDING LINE FOR XSTK...VIK
*-- Determine Which Index, Seek, and Scan To Use (WIP)
XPOSCAN='(PO=XPO .AND. VENDOR=XVENDOR .AND. POSHDR.COMPLETE=XDATE) FOR STYLE=lcStyle'
DO CASE
  *--Sort by Completion Date
  CASE lcRPWIPSor='D'
    XPOHDR='COMPLETE PO #   VENDOR'
  *--Sort by Contractor
  CASE lcRPWIPSor='F'
    XPOHDR='VENDOR   PO #   COMPLETE'
  *--Sort by PO    
  OTHERWISE
    XPOHDR='PO #   VENDOR   COMPLETE'
ENDCASE
*-- Determine Which Index, Seek, and Scan To Use (ORDERS)
XORDSCAN='(ORDER=XORDER .AND. ACCOUNT=XACCOUNT .AND. COMPLETE=XDATE) FOR STYLE=lcStyle'
DO CASE
  *--Sort by Completion Date
  CASE lcRPSalSor='D'
    XORDHDR='COMPLETE ORDER   ACCT#'
  *--Sort by Account
  CASE lcRPSalSor='A'
    XORDHDR='ACCT# ORDER   COMPLETE'
  *--Sort by Account  
  OTHERWISE
    XORDHDR='ORDER  ACCT#  COMPLETE'
ENDCASE
SELE STYLE
GO TOP
DO WHILE INKEY() <> 32
  WAIT WINDOW 'PRINTING REPORT - <Space Bar> TO ABORT' NOWAIT
  lcStyle = SUBSTR(STYLE,1,lnMajorLen)
  XDESC   = SUBSTR(DESC,1,19)
  STORE SPACE(5) TO XCLR
  STORE SPACE(6) TO lcColor
  STORE 0 TO XSTK, XPOS, XSUBPOS, XTOTPOS, XORD, XSUBORD, XTOTORD, XOTS
  *--Get Number of Color For Each Style
  COUNT ALL FOR (STYLE=lcStyle) TO XCLRCNT
  *-- Load Array With Color and Stock
  =SEEK(lcStyle)                               && To Place Pointer Back to 1st Match
  XCNT=IIF(XCLRCNT>7, 7, XCLRCNT)
  *-- IN PRECEDING LINE, THE 7'S ARE PUT IN INSTEAD OF 10. 
  FOR I=1 TO XCNT
    XCLR(I)    = SUBSTR(STYLE,lnmajorLen+2,lnColorLen)
    lcColor(I) = SUBSTR(STYLE,lnmajorLen+2,lnColorLen)
    XSTK(I) = TOTSTK
    SKIP
  ENDFOR
  IF XCLRCNT>7
    MORE = .T.
    XCLR(8) = 'OTHER'
    FOR I=8 TO XCLRCNT
      XSTK(8)=XSTK(8)+TOTSTK
      SKIP
    ENDFOR
  ENDIF
  IF ROW > 54
    ROW=0
    PAGENO=PAGENO+1
    *--I added Title option into OG to enable the user to pour a tiltle
    *--into the report instead or printing SPACES(30) as in 26.
    DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
    ROW=5
  ENDIF
  *-- Display Style, Description, And Range Of Colors
  @ ROW,01 SAY lcStyle+' '+XDESC
  COL =34
  XCNT1=XCNT+1
  FOR I=1 TO XCNT1
    *--CHECK PRECEDING LINE LATER
    @ ROW,COL SAY PADL(ALLTRIM(XCLR(I)),6,' ')
    COL=COL+9
  ENDFOR
  @ ROW,105 SAY 'TOTAL'
  ROW=ROW+1
  @ ROW,01 SAY REPLICATE('Ä',131)
  *-- Get WIP From Purchase Order Temporary File
  SELECT (lcPoTmp)
  GO TOP
  IF SEEK(lcStyle)
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
        ROW=5
        *-- Display Style, Description, And Range Of Colors
        COL=34
        FOR I=1 TO XCNT
          @ ROW,COL SAY XCLR(I)
          COL=COL+9
        ENDFOR
        @ ROW,105 SAY 'TOTAL'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('Ä',131)
        XFIRSTTIME=.T.
      ENDIF
      XPO=PO
      XVENDOR=VENDOR
      XDATE=IIF(lcConsol='N', POSHDR.COMPLETE, DATE)
      SCAN WHILE &XPOSCAN
        *--Not In-Transit
        IF TRANCD<>'3'
          DO CASE
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen) = lcColor(1)
              XPOS(1)=IIF(TRANCD='1',XPOS(1)+TOTQTY, XPOS(1)-TOTQTY)
              XPOS(1)=IIF(XPOS(1)>0, XPOS(1), 0)
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(2)
              XPOS(2)=IIF(TRANCD='1',XPOS(2)+TOTQTY, XPOS(2)-TOTQTY)
              XPOS(2)=IIF(XPOS(2)>0, XPOS(2), 0)
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(3)
              XPOS(3)=IIF(TRANCD='1',XPOS(3)+TOTQTY, XPOS(3)-TOTQTY)
              XPOS(3)=IIF(XPOS(3)>0, XPOS(3), 0)
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(4)
              XPOS(4)=IIF(TRANCD='1',XPOS(4)+TOTQTY, XPOS(4)-TOTQTY)
              XPOS(4)=IIF(XPOS(4)>0, XPOS(4), 0)
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(5)
              XPOS(5)=IIF(TRANCD='1',XPOS(5)+TOTQTY, XPOS(5)-TOTQTY)
              XPOS(5)=IIF(XPOS(5)>0, XPOS(5), 0)
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(6)
              XPOS(6)=IIF(TRANCD='1',XPOS(6)+TOTQTY, XPOS(6)-TOTQTY)
              XPOS(6)=IIF(XPOS(6)>0, XPOS(6), 0)
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(7)
              XPOS(7)=IIF(TRANCD='1',XPOS(7)+TOTQTY, XPOS(7)-TOTQTY)
              XPOS(7)=IIF(XPOS(7)>0, XPOS(7), 0)
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(8)
              XPOS(8)=IIF(TRANCD='1',XPOS(8)+TOTQTY, XPOS(8)-TOTQTY)
              XPOS(8)=IIF(XPOS(8)>0, XPOS(8), 0)
            OTHERWISE
             XPOS(8)=IIF(TRANCD='1',XPOS(8)+TOTQTY, XPOS(8)-TOTQTY)
             XPOS(8)=IIF(XPOS(8)>0, XPOS(8), 0)
          ENDCASE
        ENDIF
      ENDSCAN
      FOR I=1 TO XCNT+1
        XTOTPOS=XTOTPOS+XPOS(I)
        XSUBPOS(I)=XSUBPOS(I)+XPOS(I)
      ENDFOR
      IF llWipDet .AND. XTOTPOS>0
        *-- Display WIP Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XPOHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('-',22)
          XFIRSTTIME=.F.
        ENDIF
        ROW=ROW+1
        DO CASE
          CASE lcRPWIPSor='D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XPO
            @ ROW,16 SAY XVENDOR
          CASE lcRPWIPSor='F'
            @ ROW,01 SAY XVENDOR
            @ ROW,10 SAY XPO
            @ ROW,16 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XPO
            @ ROW,08 SAY XVENDOR
            @ ROW,17 SAY XDATE
        ENDCASE
        COL=33
        *-- Display WIP Total Line Item
        IF XCLRCNT <= 7 
        	XTEMP = XCNT
        ENDIF
        IF XCLRCNT >= 8
        	XTEMP = XCNT1
        ENDIF
        FOR I=1 TO XTEMP
          @ ROW,COL SAY XPOS(I)  PICTURE '9999999'
          COL=COL+9
        ENDFOR
        @ ROW,103 SAY XTOTPOS  PICTURE '9999999'
      ENDIF
      SELECT NOTEPAD
      =SEEK('P'+XPO)
      lcNote=''
      lnMemLins = MEMLINES(NOTEPAD.MNOTES)
      FOR I=1 TO lnMemLins
        lcNote=lcNote+MLINE(NOTEPAD.MNOTES,I)
      ENDFOR
      @ ROW,110 SAY SUBSTR(lcNote,1,22)
      SELECT (lcPoTmp)
      *-- Display Notepad If Requested
      IF llRpPrntNo
        IF lcConsol='N'
            SELECT NOTEPAD                    && Check To see if Notepad Exist For PO
            *--If NotePad Found, Skip Line and Print Notepad
            IF SEEK('P'+XPO)
              ROW=ROW+1
              *--Add this fuction to print notepad
              =lfPrnNotes()
            ENDIF
          SELECT (lcPoTmp)
        ENDIF
        SELECT (lcPoTmp)
      ENDIF
      IF EOF() .OR. STYLE<>lcStyle
        EXIT
      ENDIF
      STORE 0 TO XPOS, XTOTPOS
    ENDDO
    IF llWipDet
      COL=32
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,31 SAY REPLICATE('Ä',99)
      ROW=ROW+1
      @ ROW,06 SAY 'P.O. Subtotals ...'
      IF XCLRCNT <= 7 
      	XTEMP = XCNT
      ENDIF
      IF XCLRCNT >= 8
       	XTEMP = XCNT1
      ENDIF
      FOR I=1 TO XTEMP
        @ ROW,COL SAY XSUBPOS(I)  PICTURE '99999999'
        XSUBTOT=XSUBTOT+XSUBPOS(I)
        COL=COL+9
      ENDFOR
      @ ROW,103 SAY XSUBTOT  PICTURE '999999999'
      ROW=ROW+1
    ENDIF
  ENDIF
  IF lcRPSalSta<>'N'
    *-- Get Sales From Orders Temporary File
    SELECT (lcOrdTmp)
    GO TOP
    IF SEEK (lcStyle)
      XFIRSTTIME=.T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1
          DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
          ROW=5
          *-- Display Style, Description, And Range Of Colors
          @ ROW,01 SAY lcStyle+' '+XDESC
          COL=34
      	  IF XCLRCNT <= 7 
             XTEMP = XCNT
      	  ENDIF
          IF XCLRCNT >= 8
       	     XTEMP = XCNT1
          ENDIF
          FOR I=1 TO XTEMP
            @ ROW,COL SAY XCLR(I)
            COL=COL+9
          ENDFOR
          @ ROW,102 SAY 'TOTAL'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',129)
          XFIRSTTIME=.T.
        ENDIF
        XORDER=ORDER
        XACCOUNT=ACCOUNT
        XDATE=COMPLETE
        SCAN WHILE &XORDSCAN
          DO CASE
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(1)
              XORD(1)=XORD(1)+TOTQTY
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(2)
              XORD(2)=XORD(2)+TOTQTY
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(3)
              XORD(3)=XORD(3)+TOTQTY
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(4)
              XORD(4)=XORD(4)+TOTQTY
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(5)
              XORD(5)=XORD(5)+TOTQTY
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(6)
              XORD(6)=XORD(6)+TOTQTY
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(7)
              XORD(7)=XORD(7)+TOTQTY
            CASE SUBSTR(STYLE,lnmajorLen+2,lnColorLen)=lcColor(8)
              XORD(8)=XORD(8)+TOTQTY
            OTHERWISE
              XORD(8)=XORD(8)+TOTQTY
          ENDCASE
        ENDSCAN
        IF XCLRCNT <= 7
        	XTEMP = XCNT
        ENDIF
        IF XCLRCNT >= 8
        	XTEMP = XCNT1
        ENDIF
        FOR I=1 TO XTEMP
          XTOTORD=XTOTORD+XORD(I)
          XSUBORD(I)=XSUBORD(I)+XORD(I)
        ENDFOR
        IF llOrdDet .AND. XTOTORD>0
          *-- Display ORD Heading
          IF XFIRSTTIME
            ROW=ROW+1
            @ ROW,01 SAY XORDHDR
            ROW=ROW+1
            @ ROW,01 SAY REPLICATE('Ä',22)
            XFIRSTTIME=.F.
          ENDIF
          ROW=ROW+1
          DO CASE
            CASE lcRPSalSor='D'
              @ ROW,01 SAY XDATE
              @ ROW,10 SAY XORDER
              @ ROW,18 SAY XACCOUNT
            CASE lcRPSalSor='A'
              @ ROW,01 SAY XACCOUNT
              @ ROW,07 SAY XORDER
              @ ROW,15 SAY XDATE
            OTHERWISE
              @ ROW,01 SAY XORDER
              @ ROW,08 SAY XACCOUNT
              @ ROW,15 SAY XDATE
          ENDCASE
          COL=33
          *-- Display ORD Total Line Item
          IF XCLRCNT <= 7 
        	XTEMP = XCNT
          ENDIF
          IF XCLRCNT >= 8
        	  XTEMP = XCNT1
          ENDIF
          FOR I=1 TO XTEMP
            @ ROW,COL SAY XORD(I) PICTURE '9999999'
            COL=COL+9
          ENDFOR
          @ ROW,105 SAY XTOTORD  PICTURE '99999999'
        ENDIF
        IF EOF() .OR. STYLE<>lcStyle
          EXIT
        ENDIF
        STORE 0 TO XORD, XTOTORD
      ENDDO
      IF llOrdDet
        COL=32
        XSUBTOT=0
        ROW=ROW+1
        @ ROW,26 SAY REPLICATE('Ä',105)
        ROW=ROW+1
        @ ROW,01 SAY 'Orders Subtotals ......'
        IF XCLRCNT <= 7 
        	XTEMP = XCNT
        ENDIF
        IF XCLRCNT >= 8
        	XTEMP = XCNT1
        ENDIF
        FOR I=1 TO XTEMP
          @ ROW,COL SAY XSUBORD(I)  PICTURE '99999999'
          XSUBTOT=XSUBTOT+XSUBORD(I)
          COL=COL+9
        ENDFOR
        @ ROW,98 SAY XSUBTOT  PICTURE '999999999'
        @ ROW,104 SAY XSUBTOT  PICTURE '999999999'
        ROW=ROW+1
      ENDIF
    ENDIF
  ENDIF         && XPRTOTD='N'
  *-- Display Summary Information
  STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS
  IF ROW > 49
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
    ROW=5
    @ ROW,01 SAY lcStyle+' '+XDESC
    COL=34
    IF XCLRCNT <= 7 
        XTEMP = XCNT
    ENDIF
    IF XCLRCNT >= 8
        XTEMP = XCNT1
    ENDIF
    FOR I=1 TO XTEMP
      @ ROW,COL SAY XCLR(I)
      COL=COL+9
    ENDFOR
    @ ROW,104 SAY 'TOTAL'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('Ä',129)
  ENDIF
  COL=33
  ROW=ROW+1
  @ ROW,13 SAY 'STOCK  ....'
  IF XCLRCNT <= 7 
    XTEMP = XCNT
  ENDIF
  IF XCLRCNT >= 8
    XTEMP = XCNT1
  ENDIF
  FOR I=1 TO XTEMP
    @ ROW,COL SAY XSTK(I)  PICTURE '9999999'
    XTOTSTK=XTOTSTK+XSTK(I)
    XOTS(I)=XOTS(I)+XSTK(I)
    COL=COL+9
  ENDFOR
  @ ROW,102 SAY XTOTSTK    PICTURE '99999999'
  XGRDSTK=XGRDSTK+XTOTSTK
  ROW=ROW+1
  COL=33
  @ ROW,13 SAY 'W.I.P  ....'
  IF XCLRCNT <= 7 
    XTEMP = XCNT
  ENDIF
  IF XCLRCNT >= 8
    XTEMP = XCNT1
  ENDIF
  FOR I=1 TO XTEMP
    @ ROW,COL SAY XSUBPOS(I)  PICTURE '9999999'
    XTOTWIP=XTOTWIP+XSUBPOS(I)
    XOTS(I)=XOTS(I)+XSUBPOS(I)
    COL=COL+9
  ENDFOR
  @ ROW,102 SAY XTOTWIP       PICTURE '99999999'
  XGRDWIP=XGRDWIP+XTOTWIP
  IF lcRPSalSta<>'N'
    ROW=ROW+1
    COL=33
    @ ROW,13 SAY 'ORDERS ....'
    IF XCLRCNT <= 7 
      XTEMP = XCNT
    ENDIF
    IF XCLRCNT >= 8
      XTEMP = XCNT1
    ENDIF
    FOR I=1 TO XTEMP
      @ ROW,COL SAY XSUBORD(I)  PICTURE '9999999'
      XTOTORD=XTOTORD+XSUBORD(I)
      XOTS(I)=XOTS(I)-XSUBORD(I)
      COL=COL+9
    ENDFOR
    @ ROW,102 SAY XTOTORD       PICTURE '99999999'
    XGRDORD=XGRDORD+XTOTORD
  ENDIF
  ROW=ROW+1
  @ ROW,13 SAY REPLICATE('Í',117)
  ROW=ROW+1
  COL=32
  @ ROW,13 SAY 'O.T.S. ....'
  IF XCLRCNT <= 7 
    XTEMP = XCNT
  ENDIF
  IF XCLRCNT >= 8
    XTEMP = XCNT1
  ENDIF
  FOR I=1 TO XTEMP
    @ ROW,COL SAY XOTS(I)     PICTURE '99999999'
    XTOTOTS=XTOTOTS+XOTS(I)
    COL=COL+9
  ENDFOR
  @ ROW,102 SAY XTOTOTS       PICTURE '999999999'
  IF lcConsol='N'
    SELE STYLE
  ELSE
    SELE &STYTEMP
  ENDIF
  SCAN WHILE STYLE=lcStyle
  ENDSCAN
  IF EOF()
    ROW=ROW+2
    IF ROW > 52
      ROW=0
      PAGENO=PAGENO+1
      DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
    ENDIF
    @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
    ROW=ROW+1
    @ ROW,29 SAY 'GRAND TOTALS:  STOCK:'
    @ ROW,51 SAY XGRDSTK  PICTURE '99999999'
    @ ROW,62 SAY 'W.I.P:'
    @ ROW,69 SAY XGRDWIP  PICTURE '99999999'
    IF lcRPSalSta<>'N'              && 4/27/92
      @ ROW,80 SAY 'ORDERS:'
      @ ROW,88 SAY XGRDORD  PICTURE '99999999'
    ENDIF
    @ ROW,99 SAY 'O.T.S:'
    @ ROW,105 SAY XGRDSTK+XGRDWIP-XGRDORD  PICTURE '99999999'
    ROW=ROW+1
    @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
    EXIT
  ENDIF
  ROW=ROW+3
ENDDO
RETURN


****************************************************************************
* PROG: ICPAM102.PRG
* DATE: 08/25/1999
* DESC: REPORT WITH SIZE BREAKDOWN
* AUTH: Adel Mohammed El Gazzar (ADEL)
****************************************************************************
PROCEDURE ICPAM102

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL330              123456789012345678901234567890 - SPECIAL CUT & SOLD REPORT          MM/DD/YY
*PAGE: 123                                                                               HH:MM:SS
*
*------------------------------------------------------------------------------------------------
*XXXXXXX XXX XXXXXXXXXXXXXXXXXXXX                 XXX    XXX    XXX    XXX    XXX    XXX    XXX    XXX   TOTAL   PRICE       EXT S P
*COMPLETE PO #  VENDOR NAME
*99/99/99 123456 12345 123456789012345678901234 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*VENDOR NAME                    PO #   COMPLETE
*12345  12345678901234567890123 123456 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*PO #  VENDOR NAME                     COMPLETE
*123456 12345 123456789012345678901234 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*                    P.O. Subtotals .......    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*START    COMPLETE ORDER  ACCT# NAME
*99/99/99 99/99/99 123456 12345 123456789012345 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*ACCT# NAME            ORDER  START    COMPLETE
*12345 123456789012345 123456 99/99/99 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*ORDER  ACCT# NAME            START    COMPLETE
*123456 12345 123456789012345 99/99/99 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*                     .....    999999 999999 999999 999999 999999 999999 999999 999999 9999999        9999999.99
*
*                          STOCK  .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          W.I.P  .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          ORDERS .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          ===================================================================================
*                          O.T.S. .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*                          ====*====*====*====*====*====*====*====*====*====*====*====*====*====
*                          TOTAL PIECES ORDERED: 12345678     TOTAL $$$ ORDERED $ 999,999,999.99
*                          ====*====*====*====*====*====*====*====*====*====*====*====*====*====
*** Initialize Variables

STORE SPACE(30) TO XNAME
STORE SPACE(6)  TO XPO, XORDER
STORE SPACE(8)  TO XVENDOR
STORE SPACE(5)  TO XACCOUNT
STORE SPACE(6)  TO lcColor
STORE SPACE(1)  TO xstat, XPIKTKT
STORE CTOD('  /  /  ') TO XDATE, XSTART
STORE 0 TO XCNT, XPRICE, XGRDORD, XGRDPRI
DIMENSION XSTK(8),XOTS(8),XSIZE(8),XPOS(8),XSUBPOS(8),XORD(8),XSUBORD(8)
*-- Determine Which Index, Seek, and Scan To Use (WIP)
XPOSCAN='(PO=XPO .AND. VENDOR=XVENDOR .AND. POSHDR->COMPLETE=XDATE) FOR (STYLE=lcStyle)'
DO CASE
  *--Sort by Completion Date
  CASE lcRPWIPSor='D'
    XPOHDR='COMPLETE PO #  VENDOR NAME'
  *--Sort by Contractor
  CASE lcRPWIPSor='F'
    XPOHDR='VENDOR NAME'+SPACE(20)+'PO #   COMPLETE'
  *--Sort by PO    
  OTHERWISE
    XPOHDR='PO #  VENDOR NAME'+SPACE(21)+'COMPLETE'
ENDCASE

*-- Determine Which Index, Seek, and Scan To Use (ORDERS)
DO CASE
  *--Sort by Completion Date
  CASE lcRPSalSor='D'
    XORDHDR='START    COMPLETE ORDER  ACCT# NAME'
  *--Sort by Account
  CASE lcRPSalSor='A'
    XORDHDR='ACCT# NAME'+SPACE(12)+'ORDER  START    COMPLETE'
  *--Sort by Orders
  OTHERWISE
    XORDHDR='ORDER  ACCT# NAME'+SPACE(12)+'START    COMPLETE'
ENDCASE
SELECT STYLE
GO TOP
DO WHILE INKEY() <> 32
  lcStyle = Style
  XDESC   = SUBSTR(DESC,1,19)
  XSCALE  = SCALE
  STORE SPACE(5) TO XSIZE
  STORE 0 TO XSTK, XOTS, XPOS, XSUBPOS, XTOTPOS, XORD, XSUBORD, XTOTORD, XSUBEXT
  *-- To Get Number of Size Breakdown For Each Style
  XCNT = SCALE->CNT
  *-- Load Array With Scale
  FOR I=1 TO XCNT
    Z=STR(I,1)
    XSIZE(I) = SCALE->SZ&Z
    XSTK(I) = STK&Z
  ENDFOR
  IF ROW > 54
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
    ROW=5
  ENDIF
  *-- Display Style, Description, And Scale Breakdown
  @ ROW,01 SAY lcStyle+' '+XDESC
  COL=48
  FOR I=1 TO XCNT
    @ ROW,COL SAY PADL(ALLTRIM(XSIZE(I)),5,' ')
    COL=COL+7
  ENDFOR
  IF ROW=5
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
  ENDIF
  ROW=ROW+1
  @ ROW,01 SAY REPLICATE('Ä',131)
  *-- Get WIP From Purchase Order Temporary File
  SELECT (lcPoTmp)
  GO TOP
  IF SEEK (lcStyle)
    XFIRSTTIME=.T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW=0
        PAGENO=PAGENO+1
        DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
        ROW=5
        *-- Display Style, Description, And Scale Breakdown
        @ ROW,01 SAY lcStyle+' '+XDESC
        COL=48
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSIZE(I)
          COL=COL+7
        ENDFOR
        @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
        ROW=ROW+1
        @ ROW,01 SAY REPLICATE('Ä',131)
        XFIRSTTIME=.T.
      ENDIF
      XPO=PO
      XVENDOR=VENDOR
      XDATE=POSHDR.COMPLETE
      xstat= POSHDR.STATUS
      IF 'AP' $ gcComp_Mdl
        SELECT ApVendor
        =SEEK(XVENDOR)
        XNAME=IIF(FOUND(),cVenComp,SPACE(30))
      ENDIF 
      SELECT (lcPoTmp)
      SCAN WHILE &XPOSCAN
        IF TRANCD<>'3'                    && trancd=3 -> In-transit
          FOR I=1 TO XCNT
            Z=STR(I,1)
            XPOS(I) = IIF(TRANCD='1', XPOS(I)+QTY&Z, XPOS(I)-QTY&Z)
            XPOS(I) = IIF(XPOS(I)>0, XPOS(I), 0)
          ENDFOR
        ENDIF
      ENDSCAN
      FOR I=1 TO XCNT
        XTOTPOS=XTOTPOS+XPOS(I)
        XSUBPOS(I)=XSUBPOS(I)+XPOS(I)
      ENDFOR
      IF llWipDet .AND. XTOTPOS>0
        *-- Display WIP Heading
        IF XFIRSTTIME
          ROW=ROW+1
          @ ROW,01 SAY XPOHDR
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',46)
          XFIRSTTIME=.F.
        ENDIF
        ROW=ROW+1
        DO CASE
          CASE lcRPWIPSor='D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XPO
            @ ROW,17 SAY XVENDOR
            @ ROW,26 SAY SUBSTR(XNAME,1,21)
          CASE lcRPWIPSor='F'
            @ ROW,01 SAY XVENDOR
            @ ROW,11 SAY SUBSTR(XNAME,1,20)
            @ ROW,32 SAY XPO
            @ ROW,39 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XPO
            @ ROW,08 SAY XVENDOR
            @ ROW,17 SAY SUBSTR(XNAME,1,21)
            @ ROW,39 SAY XDATE
        ENDCASE
        COL=48
        *-- Display WIP Total Line Item
        FOR I=1 TO XCNT
          @ ROW,COL SAY XPOS(I)  PICTURE '99999'
          COL=COL+7
        ENDFOR
        @ ROW,104 SAY XTOTPOS  PICTURE '999999'
        @ ROW,111 SAY xstat
      ENDIF
      IF EOF() .OR. STYLE<>lcStyle
        EXIT
      ENDIF
      STORE 0 TO XPOS, XTOTPOS
    ENDDO
    IF llWipDet
      COL=47
      XSUBTOT=0
      ROW=ROW+1
      @ ROW,47 SAY REPLICATE('Ä',85)
      ROW=ROW+1
      @ ROW,21 SAY 'P.O. Subtotals .......'
      FOR I=1 TO XCNT
        @ ROW,COL SAY XSUBPOS(I)  PICTURE '999999'
        XSUBTOT=XSUBTOT+XSUBPOS(I)
        COL=COL+7
      ENDFOR
      @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
      ROW=ROW+1
    ENDIF
  ENDIF
  IF lcRPSalSta<>'N'
    *-- Get Sales From Orders Temporary File
    SELECT (lcOrdTmp)
    GO TOP
    IF SEEK(lcStyle)
      XFIRSTTIME=.T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW=0
          PAGENO=PAGENO+1
          DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
          ROW=5
          *--- Display Style, Description, And Scale Breakdown
          @ ROW,01 SAY lcStyle+' '+XDESC
          COL=48
          FOR I=1 TO XCNT
            @ ROW,COL SAY XSIZE(I)
            COL=COL+7
          ENDFOR
          @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
          ROW=ROW+1
          @ ROW,01 SAY REPLICATE('Ä',131)
          XFIRSTTIME=.T.
        ENDIF
        XORDER=ORDER
        XACCOUNT=ACCOUNT
        XDATE=COMPLETE
        XSTART=START
        IF lcConsol='N'
          xstat=IIF(ORDHDR.APPROVAL='DECLINE','D',ORDHDR.STATUS)
        ELSE
          xstat=IIF(DYELOT='DECLINE','D',PREPAK)
        ENDIF
        XPIKTKT=IIF(PICKED=.T.,'Y',' ')
        XPRICE=PRICE
        XNAME=IIF(SEEK('M'+XACCOUNT,'CUSTOMER'),CUSTOMER.BTNAME,SPACE(30))
        FOR I=1 TO XCNT
          Z=STR(I,1)
          XORD(I)=IIF(QTY&Z>0,QTY&Z,0)
          XTOTORD=XTOTORD+XORD(I)
          XSUBORD(I)=XSUBORD(I)+XORD(I)
        ENDFOR
        XEXT=XTOTORD*XPRICE
        XSUBEXT=XSUBEXT+XEXT
        XGRDORD=XGRDORD+XTOTORD
        XGRDPRI=XGRDPRI+XEXT
        IF llOrdDet .AND. XTOTORD>0
          *-- Display ORD Heading
          IF XFIRSTTIME
            ROW=ROW+1
            @ ROW,01 SAY XORDHDR
            ROW=ROW+1
            @ ROW,01 SAY REPLICATE('Ä',46)
            XFIRSTTIME=.F.
          ENDIF
          ROW=ROW+1
          DO CASE
            CASE lcRPSalSor='D'
              @ ROW,01 SAY XSTART
              @ ROW,10 SAY XDATE
              @ ROW,19 SAY XORDER
              @ ROW,26 SAY XACCOUNT
              @ ROW,32 SAY SUBSTR(XNAME,1,15)
            CASE lcRPSalSor='A'
              @ ROW,01 SAY XACCOUNT
              @ ROW,07 SAY SUBSTR(XNAME,1,15)
              @ ROW,23 SAY XORDER
              @ ROW,30 SAY XSTART
              @ ROW,39 SAY XDATE
            OTHERWISE
              @ ROW,01 SAY XORDER
              @ ROW,08 SAY XACCOUNT
              @ ROW,14 SAY SUBSTR(XNAME,1,15)
              @ ROW,30 SAY XSTART
              @ ROW,39 SAY XDATE
          ENDCASE
          COL=48
          *-- Display ORD Total Line Item
          FOR I=1 TO XCNT
            @ ROW,COL SAY XORD(I) PICTURE '99999'
            COL=COL+7
          ENDFOR
          @ ROW,104 SAY XTOTORD  PICTURE '999999'
          @ ROW,111 SAY XPRICE   PICTURE '9999.99'
          @ ROW,119 SAY XEXT     PICTURE '999999.99'
          @ ROW,129 SAY xstat
          @ ROW,131 SAY XPIKTKT
        ENDIF
        SKIP
        IF EOF() .OR. STYLE<>lcStyle
          EXIT
        ENDIF
        STORE 0 TO XORD, XTOTORD, XEXT
      ENDDO
      IF llOrdDet
        COL=47
        XSUBTOT=0
        ROW=ROW+1
        @ ROW,47 SAY REPLICATE('Ä',85) && 
        ROW=ROW+1
        @ ROW,21 SAY 'Orders Subtotals .....'
        FOR I=1 TO XCNT
          @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
          XSUBTOT=XSUBTOT+XSUBORD(I)
          COL=COL+7
        ENDFOR
        @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
        @ ROW,118 SAY XSUBEXT  PICTURE '9999999.99'
        ROW=ROW+1
      ENDIF
    ENDIF
  ENDIF         && lcRPSalSta='N'
  *-- Display Summary Information
  STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS
  IF ROW > 49
    ROW=0
    PAGENO=PAGENO+1
    DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
    ROW=5
    @ ROW,01 SAY lcStyle+' '+XDESC
    COL=48
    FOR I=1 TO XCNT
      @ ROW,COL SAY XSIZE(I)
      COL=COL+7
    ENDFOR
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
    ROW=ROW+1
    @ ROW,01 SAY REPLICATE('Ä',131)
  ENDIF
  COL=47
  ROW=ROW+1
  @ ROW,27 SAY 'STOCK  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSTK(I)  PICTURE '999999'
    XTOTSTK=XTOTSTK+XSTK(I)
    XOTS(I)=XOTS(I)+XSTK(I)
    COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTSTK    PICTURE '9999999'
  ROW=ROW+1
  COL=47
  @ ROW,27 SAY 'W.I.P  .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XSUBPOS(I)  PICTURE '999999'
    XTOTWIP=XTOTWIP+XSUBPOS(I)
    XOTS(I)=XOTS(I)+XSUBPOS(I)
    COL=COL+7
  ENDFOR
  @ ROW,103 SAY XTOTWIP       PICTURE '9999999'
  IF lcRPSalSta<>'N'
    ROW=ROW+1
    COL=47
    @ ROW,27 SAY 'ORDERS .........'
    FOR I=1 TO XCNT
      @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
      XTOTORD=XTOTORD+XSUBORD(I)
      XOTS(I)=XOTS(I)-XSUBORD(I)
      COL=COL+7
    ENDFOR
    @ ROW,103 SAY XTOTORD       PICTURE '9999999'
  ENDIF
  ROW=ROW+1
  @ ROW,27 SAY REPLICATE('Í',105)
  ROW=ROW+1
  COL=47
  @ ROW,27 SAY 'O.T.S. .........'
  FOR I=1 TO XCNT
    @ ROW,COL SAY XOTS(I)     PICTURE '999999'
    XTOTOTS=XTOTOTS+XOTS(I)
    COL=COL+7
  ENDFOR
  @ ROW,102 SAY XTOTOTS       PICTURE '9999999'
  SELECT STYLE
  SKIP
  IF EOF()
    ROW=ROW+2
    IF ROW > 52
      ROW=0
      PAGENO=PAGENO+1
      DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
    ENDIF
    IF lcRPSalSta<>'N'
      @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
      ROW=ROW+1
      @ ROW,27 SAY 'TOTAL PIECES ORDERED:'
      @ ROW,49 SAY XGRDORD  PICTURE '99999999'
      @ ROW,62 SAY 'TOTAL $$$ ORDERED $'
      @ ROW,82 SAY XGRDPRI  PICTURE '999,999,999.99'
      ROW=ROW+1
      @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
    ENDIF
    EXIT
  ENDIF
  ROW=ROW+3
ENDDO
RETURN

****************************************************************************
* FUNC: lfwRepWhen
* DESC: WHEN function for OG.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfwRepWhen

*--Create temp files.
=lfCreatTmp()
*--Open NotePad file.
IF !llRPBySize
  =gfOpenFile(gcDataDir+'NOTEPAD',gcDataDir+'NOTEPAD','SH')
ENDIF
lnStaPos = ASCAN(laOGFxFlt,"STYLE.STATUS")
IF lnStaPos > 0
  lnStaPos = ASUBSCRIPT(laOGFxFlt,lnStaPos,1)
  laOGFxFlt[lnStaPos,6] = 'A'
ENDIF


****************************************************************************
* FUNC: lfCreatTmp
* DESC: To create the temp files.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/25/1999
****************************************************************************
FUNCTION lfCreatTmp


*--This function is called from the OG when function (lfwRepWhen) so it can be
*--called every CLEAR READ command from lfvpbOk or lfvWIPSor or lfvSalSrt after
*--the program has been created at the first time the OG ran. So the following commands 
*--will be under missing the files.
IF !USED('&lcPoTmp')
  SELECT POSLN 
  COPY STRUCTURE TO (gcWorkDir+lcPoTmp)
  =gfOpenFile(gcWorkDir+lcPoTmp,'','EX')
  SET RELATION TO cstytype+ po INTO Poshdr ADDITIVE  
  ZAP
ELSE
  IF llOgFltCh
    SELECT (lcPoTmp)
    ZAP
  ENDIF  
ENDIF
IF !USED('&lcOrdTmp')
  SELECT ORDLINE
  COPY STRUCTURE TO (gcWorkDir+lcOrdTmp)
  =gfOpenFile(gcWorkDir+lcOrdTmp,'','EX')
  SET RELATION TO cordtype+ order INTO Ordhdr ADDITIVE
  ZAP
ELSE
  IF llOgFltCh
    SELECT (lcOrdTmp)
    ZAP
  ENDIF  
ENDIF

****************************************************************************
* FUNC: lfClearRep
* DESC: To Close the files na derase them.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfClearRep

llOgFltCh = .T.
IF USED(lcPoTmp)
  USE IN (lcPoTmp)
  ERASE (gcWorkDir+lcPoTmp)+'.DBF' 
ENDIF  
IF USED(lcOrdTmp)
  USE IN (lcOrdTmp)
  ERASE (gcWorkDir+lcOrdTmp)+'.DBF' 
ENDIF  
IF USED(lcWrkTmp)
  USE IN (lcWrkTmp)
  ERASE (gcWorkDir+lcWrkTmp)+'.DBF' 
ENDIF  

*!*************************************************************
* Name : lfNonMaj
* AUTH : Adel Mohammed El Gazzar (ADEL)
* DATE : 08/15/1999
* Desc : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
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

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
llStopConc = .F.
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')
    IF laMajSeg[lnI,1] = 'C'
      lnClrPo    = laMajSeg[lnI,4]
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
      EXIT
    ELSE
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
    ENDIF
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]
RETURN ''

****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfvStyle

PRIVATE lnCurSelct,lcStyOrder
lnCurSelct = SELECT(0)
SELECT STYLE
lcStyOrder = ORDER()
SET ORDER TO cStyle 
*-- Varible to hold  the name of the memory variable used to create the current GET field
lcObjName = SYS(18)
*-- Varible to hold  the value of the current GET field
lcObjVal = EVALUATE(SYS(18)) 

*--IF The user want to Browse or if the Style he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'STYLE'))
  lcObjVal = gfStyBrw('M',"","",.F.)  &&Browse style major only.
  lcObjVal = IIF(!EMPTY(lcObjVal) , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF 

SELECT STYLE
SET ORDER TO &lcStyOrder
SELECT (lnCurSelct)

****************************************************************************
* FUNC: lfwOldVal
* DESC: To get the old value.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/15/1999
****************************************************************************
FUNCTION lfwOldVal

lcOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! DATE      : 08/15/1999
*! Purpose   : Validate fabric
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************

FUNCTION lfvFabric1

lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.
IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
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

IF llUseByMe
  USE IN FABRIC
ENDIF  

*!*************************************************************
*! Name      : lfvFabric
*! Developer : Adel Mohammed El Gazzar (ADEK)
*  DATE      : 08/25/99
*!*************************************************************
FUNCTION lfvFabric
PRIVATE lnAlias

lnAlias   = SELECT(0)
lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal  = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field
SELECT Fabric
lcFabOrder = ORDER()
SET ORDER TO Fabric
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(lcObjVal , 'FABRIC'))
  llObjRet = FaBrow(@lcObjVal , '*')
  lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
SELECT Fabric
SET ORDER TO &lcFabOrder
SELECT(lnAlias)


*!*************************************************************
*! Name      : lfvSODate
*! AUTH      : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/28/99
*! Purpose   : Validate sales order date range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSODate()
*!*************************************************************

FUNCTION lfvSODate

PRIVATE ldFrom,ldTo

IF EMPTY(ldRPSLDate) AND EMPTY(ldRPSHDate)
   STORE {} TO ldFrom,ldTo
ELSE
  ldFrom = ldRPSLDate
  ldTo   = ldRPSHDate
ENDIF   

lcTitle = 'Sales order comp. date'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]

ldRPSLDate = ldFrom
ldRPSHDate = ldTo

*!*************************************************************
*! Name      : lfvPODate
*! AUTH      : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/28/99
*! Purpose   : Validate production order date range
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPODate()
*!*************************************************************

FUNCTION lfvPODate

PRIVATE ldFrom,ldTo

IF EMPTY(ldRPPLDate) AND EMPTY(ldRPPHDate)
  STORE {} TO ldFrom,ldTo
ELSE
  ldFrom = ldRPPLDate
  ldTo   = ldRPPHDate
ENDIF  

lcTitle = 'Prod. order comp. date'

*B603955,1 ABD -Call DateRng & ObjRng Screen from one place to solve
*B603955,1 ABD -Failed to convert 00 to 2000, it converts it to 1900. [Begin]
*DO (gcRepHome + gcAct_Appl + '\DateRng.SPR')   && Run the advance payment screen 
DO DateRng.Spx
*B603955,1 ABD - [End]


ldRPPLDate = ldFrom
ldRPPHDate = ldTo


*!*************************************************************
*! Name      : lfvpbOk
*! AUTH      : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/28/99
*! Purpose   : Validate OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************

FUNCTION lfvpbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF


*!*************************************************************
*! Name      : lfvWIPSor
*! AUTH      : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/28/99
*! Purpose   : Validate print WIP Summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvWIPSuDt()
*!*************************************************************

FUNCTION lfvWIPSor
CLEAR READ
*--

*!*************************************************************
*! Name      : lfvSalSrt
*! AUTH      : Adel Mohammed El Gazzar (ADEL)
*! Date      : 08/28/99
*! Purpose   : Validate print Sales Summary or Detail option
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvSalSuDt()
*!*************************************************************

FUNCTION lfvSalSrt
CLEAR READ
*--


*!*************************************************************
* Name : lfGetSeg
* AUTH : Adel Mohammed El Gazzar (ADEL)
* DATE : 08/15/1999
* Desc : To get the style segment
*!*************************************************************
FUNCTION lfGetSeg

*-- Get the major and nonmajor titles and lengths.
lcMajTitle = ALLTRIM(gfItemMask('HM'))
lnMajorLen = LEN(ALLTRIM(gfItemMask('PM')))
lcNonMajTl = ''
lcNonMajPi = ''
*-- No. of major segments.
lnMajSeg    = gfItemMask('SM')
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] = 'C'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = laMajSegs[lnI,4]      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
    EXIT
  ENDIF                     
ENDFOR
STORE LEN(lcNonMajPi) TO lnColorLen
*-- end of .

*!*************************************************************
* Name : lfColData
* AUTH : Adel Mohammed El Gazzar (ADEL)
* DATE : 08/15/1999
* Desc : To collect data.
*!*************************************************************
FUNCTION lfColData

IF llOgFltCh
  *--Zap Temp Files
  IF RECCOUNT(lcPoTmp) > 0
    SELECT (lcPoTmp)
    ZAP
  ENDIF
  IF RECCOUNT(lcOrdTmp) > 0
    SELECT (lcOrdTmp)
    ZAP
  ENDIF
ENDIF

*--Set indexes on temp files.
=lfSetIndex()
*--Collect data only if something changes in OG
IF llOgFltCh
  SELECT Style
  lcStyle = SPACE(lnmajorLen)
  lcColor = SPACE(lnColorLen)
  SCAN WHILE INKEY()<>32
    WAIT WINDOW 'Selecting Records - <Space Bar> To Abort!     '+lcMajTtl+'\'+lcNonMajT+' : '+style NOWAIT
    lcStyle = SUBSTR(STYLE,1,lnMajorLen)
    lcColor = SUBSTR(STYLE,lnmajorLen+2,lnColorLen)
    lcFullSty = STYLE
    *-- Store off Records Into Second Temp File lcPoTmp
    SELECT POSLN
    IF SEEK(lcFullSty)
      DO CASE
        CASE ldRPPLDate<>CTOD('  /  /  ') .AND. !EMPTY(lcRpFact)
          COPY REST TO (gcWorkDir+lcWrkTmp) WHILE STYLE = lcFullSty ;
          FOR (BETWEEN(POSHDR.COMPLETE,ldRPPLDate,ldRPPHDate) .AND. POSHDR.VENDOR = lcRpFact .AND. POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' .AND. TOTQTY<>0)
        CASE ldRPPLDate<>CTOD('  /  /  ') .AND. EMPTY(lcRpFact)
          COPY REST TO (gcWorkDir+lcWrkTmp) WHILE STYLE = lcFullSty ;
          FOR (BETWEEN(POSHDR.COMPLETE,ldRPPLDate,ldRPPHDate) .AND. POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' .AND. TOTQTY<>0)
        CASE ldRPPLDate=CTOD('  /  /  ') .AND. !EMPTY(lcRpFact)
          COPY REST TO (gcWorkDir+lcWrkTmp) WHILE STYLE = lcFullSty ;
          FOR (POSHDR.VENDOR = lcRpFact .AND. POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' .AND. TOTQTY<>0)
        OTHERWISE
          COPY REST TO (gcWorkDir+lcWrkTmp) WHILE STYLE = lcFullSty ;
          FOR (POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' .AND. TOTQTY<>0)
      ENDCASE
      SELECT (lcPoTmp)
      APPEND FROM (gcWorkDir+lcWrkTmp)
    ENDIF
    IF lcRPSalSta<>'N'
      *-- Store off Records Into Third Temp File lcOrdTmp
      SELE ORDLINE
      IF SEEK(lcFullSty)
        IF ldRPSLDate<>CTOD('  /  /  ')
          COPY REST TO (gcWorkDir+lcWrkTmp) WHILE STYLE = lcFullSty ;
          FOR (BETWEEN(COMPLETE,ldRPSLDate,ldRPSHDate) .AND. ORDHDR.STATUS $ lcStatus .AND. TOTQTY<>0)
        ELSE
          COPY REST TO (gcWorkDir+lcWrkTmp) WHILE STYLE = lcFullSty ;
          FOR (ORDHDR.STATUS $ lcStatus .AND. TOTQTY<>0)
        ENDIF
        SELECT (lcOrdTmp)
        APPEND FROM (gcWorkDir+lcWrkTmp)
      ENDIF
    ENDIF
    SELE STYLE
  ENDSCAN
ENDIF
*--Clear wait window
WAIT CLEAR

*!*************************************************************
*! Name      : lfPrnNotes
*: DATE      : 08/52/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To print the po notepad.
*!*************************************************************
*! Example   :  lfPrnNotes()
*!*************************************************************
FUNCTION lfPrnNotes
PRIVATE lnAlias

lnOldMemW = 0
lnMemLins = 0
lnNotLine = 0
lnAlias   = SELECT()
lnOldMemW = SET("MEMOWIDTH")
lnNotLine = 1
SET MEMOWIDTH TO 65
SELECT NotePad
lnMemLins = MEMLINES(NOTEPAD.MNOTES)
DO WHILE lnNotLine <= lnMemLins    
  IF !EMPTY(MLINE(MNOTES,lnNotLine))
    lnLinRow = lnLinRow + 1
    IF ROW > 54
      ROW=0
      PAGENO=PAGENO+1
      *--I added Title option into OG to enable the user to pour a tiltle
      *--into the report instead or printing SPACES(30) as in 26.
      DO RPT_HDR WITH 'ICPAM101',lcRpTitle ,R_WIDTH
      ROW=5
    ENDIF
    @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
  ENDIF
  lnNotLine = lnNotLine + 1
ENDDO     
SET MEMOWIDTH TO lnOldMemW
SELECT(lnAlias)

****************************************************************************
* FUNC: lfSetIndex
* DESC: To set indexes on temp files under the conditions of OG.
* AUTH: Adel Mohammed El Gazzar (ADEL)
* DATE: 08/25/1999
****************************************************************************
FUNCTION lfSetIndex

IF llRpPoSort OR llOgFltCh
  SELECT (lcPoTmp)
  DO CASE
    CASE !llRPBySize .AND. lcRPWIPSta='S'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+PO+VENDOR+DTOS(POSHDR.COMPLETE)+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+TRANCD+STR(RECNO(),7) TAG &lcPoTmp
    CASE llRPBySize .AND. lcRPWIPSta='S'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+PO+VENDOR+DTOS(POSHDR->COMPLETE)+TRANCD+STR(RECNO(),7) TAG &lcPoTmp
    CASE !llRPBySize .AND. lcRPWIPSor='D'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+DTOS(POSHDR->COMPLETE)+PO+VENDOR+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+TRANCD+STR(RECNO(),7) TAG &lcPoTmp
    CASE !llRPBySize .AND. lcRPWIPSor='F'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+VENDOR+PO+DTOS(POSHDR->COMPLETE)+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+TRANCD+STR(RECNO(),7) TAG &lcPoTmp
    CASE !llRPBySize .AND. lcRPWIPSor='P'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+PO+VENDOR+DTOS(POSHDR->COMPLETE)+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+TRANCD+STR(RECNO(),7) TAG &lcPoTmp
    CASE llRPBySize .AND. lcRPWIPSor='D'
      INDEX ON Style+DTOS(POSHDR->COMPLETE)+PO+VENDOR+TRANCD+STR(RECNO(),7) TAG &lcPoTmp
    CASE llRPBySize .AND. lcRPWIPSor='F'
     INDEX ON Style+VENDOR+PO+DTOS(POSHDR->COMPLETE)+TRANCD+STR(RECNO(),7) TAG &lcPoTmp
    CASE llRPBySize .AND. lcRPWIPSor='P'
      INDEX ON Style+PO+VENDOR+DTOS(POSHDR->COMPLETE)+TRANCD+STR(RECNO(),7) TAG &lcPoTmp
  ENDCASE
ENDIF
IF llRpSoSort OR llOgFltCh
  SELECT (lcOrdTmp)
  DO CASE
    CASE !llRPBySize .AND. lcRPSalSta='S'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+ORDER+ACCOUNT+DTOS(COMPLETE)+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+STORE+STR(RECNO(),7) TAG &lcOrdTmp
    CASE llRPBySize .AND. lcRPSalSta='S'
      INDEX ON Style+ORDER+DTOS(COMPLETE)+STORE TAG &lcOrdTmp
    CASE !llRPBySize .AND. lcRPSalSor='D'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+DTOS(COMPLETE)+ORDER+ACCOUNT+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+STORE+STR(RECNO(),7) TAG &lcOrdTmp
    CASE !llRPBySize .AND. lcRPSalSor='A'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+ACCOUNT+ORDER+DTOS(COMPLETE)+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+STORE+STR(RECNO(),7) TAG &lcOrdTmp
    CASE !llRPBySize .AND. lcRPSalSor='O'
      INDEX ON SUBSTR(Style,1,lnmajorLen)+ORDER+ACCOUNT+DTOS(COMPLETE)+SUBSTR(STYLE,lnmajorLen+2,lnColorLen)+STORE+STR(RECNO(),7) TAG &lcOrdTmp
    CASE llRPBySize .AND. lcRPSalSor='D'
      INDEX ON Style+DTOS(COMPLETE)+ORDER+STORE TAG &lcOrdTmp
    CASE llRPBySize .AND. lcRPSalSor='A'
      INDEX ON Style+ACCOUNT+ORDER+DTOS(COMPLETE)+STORE TAG &lcOrdTmp
    CASE llRPBySize .AND. lcRPSalSor='O'
      INDEX ON Style+ORDER+DTOS(COMPLETE)+STORE TAG &lcOrdTmp
  ENDCASE
ENDIF
STORE .F. TO llRpPoSort , llRpSoSort
*-- end of lfSetIndex.

*!*************************************************************
*! Name      : lfsrvSty
*  AUTH      : Adel Mohammed El Gazzar (ADEL)
*  DATE      : 08/25/1999
*! Purpose   : Rise change style flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
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
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
ENDCASE
*-- end of lfsrvSty.

*!*************************************************************
*! Name      : lfStySum
*  AUTH      : Adel Mohammed El Gazzar (ADEL)
*  DATE      : 08/25/1999
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
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) == ALLTRIM(lcSty)
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


*!*************************************************************
*! Name      : lfvWipSort
*: DATE      : 08/52/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To indicate that the sort changes.
*!*************************************************************
FUNCTION lfvWipSort

llRpPoSort = .T.

*!*************************************************************
*! Name      : lfvSoSort
*: DATE      : 08/52/1999
*: Developer : Adel Mohhamed El Gazzar (ADEL)
*! Purpose   : To indicate that the sort changes.
*!*************************************************************
FUNCTION lfvSoSort

llRpSoSort = .T.

