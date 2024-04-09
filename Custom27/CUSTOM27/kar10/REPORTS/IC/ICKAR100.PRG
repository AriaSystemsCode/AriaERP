*:***************************************************************************
*: Program file  : ICKAR100.PRG
*: Program desc. : CUSTOMIZED SPECIAL CUT & SOLD FOR KAR10.
*: Date          : 07/11/2002
*: System        : Aria Advantage Series.
*: Module        : Inventory Control (IC)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: C102669  (REFERE TO C100853)
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ICKAR100
*:***************************************************************************
*: Modifications :
*:***************************************************************************

*-- lcRepNmTtl variable that hold the header of the non major segment for the frx
*-- llDontPrn  variable that showes that no records match report criteria
*-- PAGENO     variable that hold Page No.
*-- ROW        variable that hold Row No.
*-- XTIME      variable that hold time
*-- R_WIDTH    variable that hold report width
*-- R_TITLE    variable that hold report title
*-- WORKTEMP   variable that hold the name of work temp. file
*-- CUTTTEMP   variable that hold the name of cuttkt temp. file
*-- POTEMP     variable that hold the name of PO temp. file
*-- ORDTEMP    variable that hold the name of ORDHDR temp. file
*-- XPRTWIP    variable that showes print WIP Summary or Detail
*-- XBYSIZE    variable that showes print by size
*-- XSLDAT1    variable that hold from sales order comp. date
*-- XSHDAT1    variable that hold to   sales order comp. date
*-- XPLDAT1    variable that hold from Prod. order comp. date
*-- XPHDAT1    variable that hold to   Prod. order comp. date
*-- XWIPSORT   variable that showes if it is sorted by WIP or not
*-- XPRTORD    variable that showes print Sales Summary or Detail
*-- XORDSORT   variable that hold the sales order Sort
*-- XSTATHLD   variable that showes if include Orders on hold
*-- XALLOCAT   variable that showes if include Allocated Qty. or All Qty.
*-- XTITLE     variable that hold optional title
*-- XFILTER    variable that hold report filter expression

STORE SPACE(0) TO WORKTEMP, CUTTTEMP , POTEMP , ORDTEMP , XPRTWIP , XPRTORD , XSTAT
STORE .F.      TO XWIPRPT , XORDRPT

llDontPrn = .F.
PAGENO    = 0
ROW       = 0
XTIME     = TIME()
R_WIDTH   = ''
R_TITLE   = ''

lcRepNmTtl =  gfItemMask("HN")
lcTime     =  gfGetTime()
lnMajLen   =  LEN(SUBSTR(lcMajPic,4))
XBYSIZE    = IIF(llRPBySize,'Y','N')
XSLDATE    = ldRPSLDate
XSHDATE    = ldRPSHDate
XPLDATE    = ldRPPLDate
XPHDATE    = ldRPPHDate
XPRTWIP    = lcRPWIPSta
XWIPSORT   = lcRPWIPSor
XPRTORD    = lcRPSalSta
XORDSORT   = lcRPSALSor
XSTATHLD   = IIF(llRPHolOrd,'Y','N')
XALLOCAT   = lcRPAllQty
XTITLE     = lcRPTitle

PRIVATE lcCustAcnt
STORE "" TO lcCustAcnt
llMore24 = .F.
lnDatPost = 0

lnDataPos = ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT')
IF lnDataPos > 0
  lnDataPos = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
  IF AT('INLIST(CUSTOMER.TYPE',LCRPEXP) > 0
    lnDatPost = AT('INLIST(CUSTOMER.TYPE',LCRPEXP)
  ELSE
    IF AT('BETWEEN(CUSTOMER.TYPE',LCRPEXP) > 0
      lnDatPost = AT('BETWEEN(CUSTOMER.TYPE',LCRPEXP)
      llMore24 = .T.
    ENDIF
  ENDIF

  IF lnDatPost > 0
    IF llMore24
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost) , 2)
    ELSE
      lnPos1 = AT('AND' , SUBSTR(LCRPEXP,lnDatPost))
    ENDIF

    IF lnPos1 > 0
      lcCustAcnt = SUBSTR(lcRpExp ,lnDatPost , lnPos1-1)
      lcRPExp    = STRTRAN(lcRPExp, lcCustAcnt , " .T. ")
      lcCustAcnt = "AND " + lcCustAcnt
    ELSE
      lcCustAcnt = SUBSTR(lcRpExp ,lnDatPost)
      lcRPExp    = STRTRAN(lcRPExp, lcCustAcnt , " .T. )")
      lcCustAcnt = "AND (" + lcCustAcnt
    ENDIF
  ENDIF
ENDIF

XFILTER = lcRPExp

WAIT WINDOW 'Collecting data....' NOWAIT
DO lpCollecData
WAIT CLEAR

IF !llDontPrn
  SET DEVICE TO PRINT
  DO lpPrint
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF

*--Function to delete all the temp files to privent the bug of "Many files open"
DIMENSION laFileName[3]
laFileName[1] = CUTTTEMP
laFileName[2] = POTEMP
laFileName[3] = ORDTEMP
=lfBasToClr(@laFileName , 'A')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpCollecData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/01/2002
*! Purpose   : Collecting data in Temp. File.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpCollecData
*!*************************************************************
PROCEDURE lpCollecData

IF XPRTWIP = 'D'
  XWIPRPT  = .T.
ELSE
  XWIPRPT  = .F.
ENDIF

IF XPRTORD = 'D'
  XORDRPT  = .T.
ELSE
  XORDRPT = .F.
ENDIF

XSTAT = IIF(XSTATHLD = 'N','O','HO')

IF XSLDATE<>CTOD('  /  /    ')
  XSHDATE = IIF(XSHDATE = CTOD('  /  /    '),XSLDATE,XSHDATE)
ENDIF

IF XPLDATE<>CTOD('  /  /    ')
  XPHDATE = IIF(XPHDATE = CTOD('  /  /    '),XPLDATE,XPHDATE)
ENDIF

SELECT STYLE
SET FILTER TO
XFILTER = XFILTER + '.AND. (TOTORD<>0 .OR. TOTWIP<>0 .OR. TOTSTK<>0)'
SET FILTER TO &XFILTER
LOCATE

IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  SET DEVICE TO SCREEN
  llDontPrn = .T.
  RETURN
ENDIF

********  EXTRACT RECORDS FROM CUTTING TICKET, PO, AND ORDERS **************
WORKTEMP = gfTEMPNAME()
CUTTTEMP = gfTEMPNAME()
POTEMP   = gfTEMPNAME()
ORDTEMP  = gfTEMPNAME()

*-- If the "Manufacturing" module is installed
IF 'MF' $ gcComp_Mdl

  SELECT CUTTKTL
  SET ORDER TO CUTTKTLS
  SET RELATION TO CUTTKT INTO CUTTKTH
  COPY STRU TO &gcWorkDir.&CUTTTEMP
  USE &gcWorkDir.&CUTTTEMP IN 0 EXCLUSIVE
  SELECT (CUTTTEMP)
  SET RELATION TO CUTTKT INTO CUTTKTH

  SET ORDER TO TktOper IN MFGOPRHD
  SELECT CUTTKTH
  SET RELATION TO 'M' + CutTkt INTO MFGOPRHD

  SELECT (CUTTTEMP)
  DO CASE
    CASE XBYSIZE = 'N' .AND. XPRTWIP = 'S'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE = 'Y' .AND. XPRTWIP = 'S'
      INDEX ON STYLE + CUTTKT + TRANCD + STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'D'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(CUTTKTH.COMPLETE)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'F'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+MFGOPRHD.cContCode+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'P'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+CUTTKT+TRANCD+STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'D'
      INDEX ON STYLE + DTOS(CUTTKTH.COMPLETE) + CUTTKT + TRANCD + STR(RECNO(),7) TAG &CUTTTEMP
    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'F'
      INDEX ON STYLE + MFGOPRHD.cContCode + CUTTKT + TRANCD + STR(RECNO() , 7) TAG &CUTTTEMP
    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'P'
      INDEX ON STYLE + CUTTKT + TRANCD + STR(RECNO() , 7) TAG &CUTTTEMP
  ENDCASE
ENDIF    && End of IF 'MF' $ gcComp_Mdl

*-- If the "Style Purchase Order" or the "Point of Sale" Modules is installed.
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  SELECT POSLN
  SET RELATION TO cStyType + PO INTO POSHDR
  COPY STRU TO &gcWorkDir.&POTEMP
  USE &gcWorkDir.&POTEMP IN 0 EXCLUSIVE
  SELECT (POTEMP)
  SET RELATION TO cStyType + PO INTO POSHDR

  DO CASE
    CASE XBYSIZE = 'N' .AND. XPRTWIP = 'S'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE = 'Y' .AND. XPRTWIP = 'S'
      INDEX ON STYLE + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'D'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(POSHDR.COMPLETE)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'F'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+VENDOR+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE = 'N' .AND. XWIPSORT = 'P'
      INDEX ON SUBSTR(STYLE,1,lnMajLen)+PO+TRANCD+STR(RECNO(),7) TAG &POTEMP
    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'D'
      INDEX ON STYLE + DTOS(POSHDR.COMPLETE) + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'F'
      INDEX ON STYLE + VENDOR + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
    CASE XBYSIZE = 'Y' .AND. XWIPSORT = 'P'
      INDEX ON STYLE + PO + TRANCD + STR(RECNO() , 7) TAG &POTEMP
  ENDCASE
ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

SELECT ORDLINE
SET RELATION TO cOrdType + Order INTO ORDHDR
COPY STRU TO &gcWorkDir.&ORDTEMP
USE &gcWorkDir.&ORDTEMP IN 0 EXCLUSIVE

SELECT (ORDTEMP)
SET RELATION TO cOrdType + Order INTO ORDHDR

DO CASE
  CASE XBYSIZE = 'N' .AND. XPRTORD = 'S'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE = 'Y' .AND. XPRTORD = 'S'
    INDEX ON STYLE + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO , 6) TAG &ORDTEMP
  CASE XBYSIZE = 'N' .AND. XORDSORT = 'D'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+DTOS(COMPLETE)+ORDER+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE = 'N' .AND. XORDSORT = 'A'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ACCOUNT+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE = 'N' .AND. XORDSORT = 'O'
    INDEX ON SUBSTR(STYLE,1,lnMajLen)+ORDER+DTOS(COMPLETE)+STORE+STR(LINENO,6) TAG &ORDTEMP
  CASE XBYSIZE = 'Y' .AND. XORDSORT = 'D'
    INDEX ON STYLE + DTOS(COMPLETE) + ORDER + STORE + STR(LINENO , 6) TAG &ORDTEMP
  CASE XBYSIZE = 'Y' .AND. XORDSORT = 'A'
    INDEX ON STYLE + ACCOUNT + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO , 6) TAG &ORDTEMP
  CASE XBYSIZE = 'Y' .AND. XORDSORT = 'O'
    INDEX ON STYLE + ORDER + DTOS(COMPLETE) + STORE + STR(LINENO , 6) TAG &ORDTEMP
ENDCASE

IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *-- Openning the file CutPick with a relation to OrdLine according 
  *-- to the CutOrd index.
  =gfOpenFile(gcDataDir+'CutPick','CutOrd','SH')
  SELECT ORDLINE
  SET RELATION TO '2'+Order+STR(LineNo,6) INTO CutPick ADDITIVE
ENDIF

*-- If the "Style Purchase Order" or the "Point of Sale" modules is installed.
IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *-- Openning the file CutPick with a relation to PosLn according
  *-- to the CutPick index in another work area.
  SELECT 0
  USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick2
  SET ORDER TO CutPick
  SELECT PosLn
  SET RELATION TO '2'+PO+Style INTO CutPick2 ADDITIVE
ENDIF

*-- If the "Manufacturing" or the "Style Purchase Order" or the
*-- "Point of Sale" modules is installed
IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  *-- Openning the file CutPick with a relation to OrdLine according
  *-- to the CutOrd index in another work area to check if the order
  *-- was allocated by cuttkt not by PO.
  SELECT 0
  USE '&gcDataDir.CutPick' AGAIN ALIAS CutPick3
  SET ORDER TO CutOrd
  SELECT OrdLine
  SET RELATION TO '1'+Order+STR(LineNo,6) INTO CutPick3 ADDITIVE
ENDIF

SELE STYLE
LOCATE

XDESC  = SPACE(20)
XSTYLE = SPACE(19)
XCOLOR = SPACE(06)

*** SAVE OFF RECORDS INTO THEIR RESPECTIVE TEMPORARY FILE
SCAN
  xStyle = Style
  WAIT WINDOW xStyle NOWAIT 

  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl
    *** Store off Records Into First Temp File CUTTTEMP
    SELE CUTTKTL
    SEEK XSTYLE
    IF FOUND()
      IF XPLDATE<>CTOD('  /  /    ')
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) FOR (BETWEEN(CUTTKTH.COMPLETE,XPLDATE,XPHDATE) .AND. CUTTKTH.STATUS<>'X' .AND. CUTTKTH.STATUS<>'C' .AND. TOTQTY<>0)
      ELSE
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) FOR (CUTTKTH.STATUS<>'X' .AND. CUTTKTH.STATUS<>'C' .AND. TOTQTY<>0)
      ENDIF
      SELE &CUTTTEMP
      APPEND FROM &gcWorkDir.&WORKTEMP
    ENDIF
  ENDIF

  *-- If the " Style Purchase Order " or the "Point of Sale" Modules is installed.
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
    *** Store off Records Into Second Temp File POTEMP
    SELECT POSLN
    SEEK XSTYLE
    IF FOUND()

      IF !('POSLN.PO INTO CUTPICK' $ SET('RELATION'))
        SET RELATION TO "2" + POSLN.PO INTO CUTPICK ADDITIVE
        SELECT CUTPICK
        PRIVATE lcOrdCut
        lcOrdCut = ORDER()
        SET ORDER TO TAG CUTPICK
        SET RELATION TO "O" + CUTPICK.ORDER INTO ORDHDR ADDITIVE
        SELECT ORDHDR
        SET RELATION TO IIF(EMPTY(Store) , 'M' + Account , 'S' + Account + Store) INTO Customer ADDITIVE
        SELECT POSLN
      ENDIF

      XADD = ''
      DO CASE
        CASE XALLOCAT = 'L'
          XADD = ' .AND.  CutPick2.CtktNo = PosLn.Po'
        CASE XALLOCAT = 'N'
          XADD = ' .AND.  CutPick2.CtktNo <> PosLn.Po'
      ENDCASE

      IF XPLDATE<>CTOD('  /  /    ')
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE) ;
             FOR (BETWEEN(POSHDR.COMPLETE,XPLDATE,XPHDATE) .AND. ;
             POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C'   .AND. ;
             TOTQTY<>0 &XADD &lcCustAcnt)
      ELSE
        COPY REST TO &gcWorkDir.&WORKTEMP WHILE (STYLE)=(XSTYLE)    ;
             FOR (POSHDR.STATUS<>'X' .AND. POSHDR.STATUS<>'C' .AND. ;
             TOTQTY<>0 &XADD &lcCustAcnt)
      ENDIF

      SELECT POSLN
      IF 'POSLN.PO INTO CUTPICK' $ SET('RELATION')
        SET RELATION OFF INTO CUTPICK
        SELECT CUTPICK
        SET ORDER TO TAG &lcOrdCut
        SET RELATION OFF INTO ORDHDR
        SELECT ORDHDR
        SET RELATION OFF INTO Customer
        SELECT POSLN
      ENDIF

      SELE &POTEMP
      APPEND FROM &gcWorkDir.&WORKTEMP
    ENDIF
  ENDIF

  *** Store off Records Into Third Temp File ORDTEMP
  SELE ORDLINE
  SEEK XSTYLE
  IF FOUND()
    IF !('ORDLINE.ACCOUNT INTO CUSTOMER' $ SET('RELATION'))
      SET RELATION TO "M" + ORDLINE.ACCOUNT INTO Customer ADDITIVE
    ENDIF

    XADDORD = ''
    *-- If the "Manufacturing" or the "Style Purchase Order" or the
    *-- "Point of Sale" modules is installed
    IF 'MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl
      DO CASE
        CASE XALLOCAT = 'L'
          XADDORD = ' .AND.  (OrdLine.Order = CutPick.Order .OR. ' + ;
                    ' OrdLine.Order = CutPick3.Order) '
        CASE XALLOCAT = 'N'
          XADDORD = ' .AND.  (OrdLine.Order <>CutPick.Order .AND.' + ;
                    ' OrdLine.Order <> CutPick3.Order) '
      ENDCASE
    ENDIF

    IF XSLDATE<>CTOD('  /  /    ')
      COPY REST TO &gcWorkDir.&WORKTEMP WHILE Style = xStyle  ;
           FOR BETWEEN(Complete , xSLDate , xSHDate)    .AND. ;
                ORDHDR.Status $ XStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
    ELSE
      COPY REST TO &gcWorkDir.&WORKTEMP WHILE Style = xStyle  ;
            FOR ORDHDR.Status $ xStat .AND. TotQty <> 0 .AND. ;
                cOrdType = 'O'                          .AND. ;
                IIF(lcRPAloct = "A" , !EMPTY(ORDLINE.PIKTKT) , IIF(lcRPAloct = "U" , EMPTY(ORDLINE.PIKTKT) , .T.));
                &XADDORD &lcCustAcnt
    ENDIF

    SELECT ORDLINE
    IF 'ORDLINE.ACCOUNT INTO CUSTOMER' $ SET('RELATION')
      SET RELATION OFF INTO Customer
    ENDIF

    SELE &ORDTEMP
    APPEND FROM &gcWorkDir.&WORKTEMP
  ENDIF

ENDSCAN

IF USED('CUTPICK')
  USE IN CUTPICK
ENDIF

IF USED('CUTPICK2')
  USE IN CUTPICK2
ENDIF

IF USED('CUTPICK3')
  USE IN CUTPICK3
ENDIF

PAGENO  = 0
ROW     = 99
XTIME   = TIME()
R_WIDTH = 'W'
IF XWIPRPT .OR. XORDRPT
  DO CASE
    CASE XALLOCAT = 'L'
      R_TITLE = 'CUSTOMIZED SPECIAL CUT & SOLD - ALLOCATED DETAIL REPORT'
    CASE XALLOCAT = 'N'
      R_TITLE = 'CUSTOMIZED SPECIAL CUT & SOLD - NOT ALLOCATED DETAIL REPORT'
    CASE XALLOCAT = 'A'
      R_TITLE = 'CUSTOMIZED SPECIAL CUT & SOLD - DETAIL REPORT'
  ENDCASE
ELSE
  DO CASE
    CASE XALLOCAT = 'L'
      R_TITLE = 'CUSTOMIZED SPECIAL CUT & SOLD - ALLOCATED SUMMARY REPORT'
    CASE XALLOCAT = 'N'
      R_TITLE = 'CUSTOMIZED SPECIAL CUT & SOLD - NOT ALLOCATED SUMMARY REPORT'
    CASE XALLOCAT = 'A'
      R_TITLE = 'CUSTOMIZED SPECIAL CUT & SOLD - SUMMARY REPORT'
  ENDCASE
ENDIF

*--End of lpCollecData.
*!*************************************************************
*! Name      : lpPrint
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Print report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lpPrint
*!*************************************************************
PROCEDURE lpPrint

IF XBYSIZE = 'N'
  DO STY961                                 && No Breakdown In Size
ELSE
  DO STY962                                 && Breakdown In Size
ENDIF

*--End of lpPrint.
*!*************************************************************
*! Name      : STY961.PRG
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : REPORT WITHOUT SIZE BREAKDOWN
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : STY961.PRG
*!*************************************************************
PROCEDURE STY961

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*JNL330              123456789012345678901234567890 - SPECIAL CUT & SOLD REPORT          MM/DD/YY
*PAGE: 123                                                                               HH:MM:SS
*
*------------------------------------------------------------------------------------------------
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*XXXXXXX XXXXXXXXXXXXXXXXXXXX       XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX      XXX     TOTAL
*PO #   VENDOR COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     P.O. Subtotals ........  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*CUTTKT CONTR1 COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     Cuttkt Subtotals ......  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*ORDER  ACCT#  COMPLETE
*XXXXXX XXXXX  99/99/99         9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*     Orders Subtotals ......  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*            STOCK  .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            W.I.P  .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            ORDERS .........   9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  9999999  99999999
*            =====================================================================================================================
*            O.T.S. .........  99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 99999999 999999999
*
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                            ====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===
*                            GRAND TOTALS:  STOCK: 12345678   W.I.P: 12345678   ORDERS: 12345678   O.T.S: 12345678
*                            ====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*===

*** Declare & Initialize Variables & Array
STORE SPACE(6)  TO XCUTTKT, XPO, XORDER
STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
STORE SPACE(6)  TO XCOLOR
STORE CTOD('  /  /  ') TO XDATE
STORE 0 TO XCLRCNT, XCNT, XGRDSTK, XGRDWIP, XGRDORD
DIMENSION XCOLOR(10),XCLR(10),XSTK(10),XCUT(10),XSUBCUT(10),XPOS(10),;
          XSUBPOS(10),XORD(10),XSUBORD(10),XOTS(10)

DIMENSION nTotStock(10), nTotWip(10), nTotOrder(10),nTotOts(10)
STORE 0 TO nTotStock, nTotWip, nTotOrder, nTotOts

*** Determine Which Index, Seek, and Scan To Use (WIP)
DO CASE
  CASE XWIPSORT='D'                          && Sort by Completion Date
    XCUTSCAN = '(CUTTKTH.COMPLETE=XDATE .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XPOSCAN  = '(POSHDR.COMPLETE=XDATE .AND. PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR  = 'COMPLETE CUTTKT CONTR1'
    XPOHDR   = 'AVAILABLE PO #   VENDOR'

  CASE XWIPSORT='F'                          && Sort by Contractor
    XCUTSCAN = '(MFGOPRHD.cContCode=XCONTR1 .AND. CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XPOSCAN  = '(POSHDR.VENDOR=XVENDOR .AND. PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR  = 'CONTR1 CUTTKT COMPLETE'
    XPOHDR   = 'VENDOR   PO #   AVAILABLE'

  OTHERWISE                                 && Sort by Cutting Ticket/PO
    XCUTSCAN='(CUTTKTH.CUTTKT=XCUTTKT) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XPOSCAN='(PO=XPO) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XCUTHDR='CUTTKT CONTR1 COMPLETE'
    XPOHDR='PO #   VENDOR   AVAILABLE'
ENDCASE

*** Determine Which Index, Seek, and Scan To Use (ORDERS)
DO CASE
  CASE XORDSORT='D'                         && Sort by Completion Date
    XORDSCAN='(COMPLETE=XDATE .AND. ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XORDHDR='COMPLETE ORDER   ACCT#'
  CASE XORDSORT='A'                         && Sort by Account
    XORDSCAN='(ACCOUNT=XACCOUNT .AND. ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XORDHDR='ACCT# ORDER   COMPLETE'
  OTHERWISE                                 && Sort by Orders
    XORDSCAN='(ORDER=XORDER) FOR SUBSTR(STYLE,1,lnMajLen)=XSTYLE'
    XORDHDR='ORDER  ACCT#  COMPLETE'
ENDCASE

SELE STYLE
LOCATE

DO WHILE INKEY() <> 32
  XSTYLE = SUBSTR(STYLE,1,lnMajLen)
  XDESC  = DESC

  STORE SPACE(5) TO XCLR
  STORE SPACE(6) TO XCOLOR
  STORE 0 TO XSTK, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD,;
             XSUBORD, XTOTORD, XOTS

  *** To Get Number of Color For Each Style
  COUNT ALL FOR (SUBSTR(STYLE,1,lnMajLen) = XSTYLE) TO XCLRCNT

  *** Load Array With Color and Stock
  SEEK XSTYLE                               && To Place Pointer Back to 1st Match

  XCNT = IIF(XCLRCNT>10 , 10 , XCLRCNT)

  FOR I = 1 TO XCNT
    XCLR(I)   = SUBSTR(STYLE.Style,lnNonMajPo)
    XCOLOR(I) = SUBSTR(STYLE.Style,lnNonMajPo)
    XSTK(I)   = TOTSTK
    SKIP
  ENDFOR

  IF XCLRCNT>10
    XCLR(10) = 'OTHER'
    FOR I = 11 TO XCLRCNT
      XSTK(10) = XSTK(10) + TOTSTK
      SKIP
    ENDFOR
  ENDIF

  IF ROW > 54
    ROW    = 0
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
    ROW    = 5
  ENDIF

  *** Display Style, Description, And Range Of Colors
  @ ROW,01 SAY XSTYLE + ' ' + LEFT(XDESC,17)

  COL = 34
  FOR I = 1 TO XCNT
    IF LEN(TRIM(XCLR(I))) < 4
      XCLR(I) = IIF(LEN(TRIM(XCLR(I)))=1 , '    ' + TRIM(XCLR(I)),XCLR(I))
      XCLR(I) = IIF(LEN(TRIM(XCLR(I)))=2 , '   '  + TRIM(XCLR(I)),XCLR(I))
      XCLR(I) = IIF(LEN(TRIM(XCLR(I)))=3 , '  '   + TRIM(XCLR(I)),XCLR(I))
    ENDIF
    @ ROW,COL SAY XCLR(I)
    COL = COL + 9
  ENDFOR

  IF ROW = 5
    @ ROW,125 SAY 'TOTAL'
  ENDIF
  ROW = ROW + 1
  @ ROW,01 SAY REPLICATE('Ä',129)

  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl

    *** Get WIP From Cutting Ticket Temporary File
    SELE &CUTTTEMP
    LOCATE
    SEEK XSTYLE
    IF FOUND()
      XFIRSTTIME = .T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW = 0
          PAGENO = PAGENO + 1
          DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
          ROW = 5
          *** Display Style, Description, And Range Of Colors
          @ ROW,01 SAY XSTYLE+' '+LEFT(XDESC,17)
          COL = 34
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XCLR(I)
            COL = COL + 9
          ENDFOR
          @ ROW,125 SAY 'TOTAL'
          ROW = ROW + 1
          @ ROW,01 SAY REPLICATE('Ä',129)
          XFIRSTTIME = .T.
        ENDIF

        XCUTTKT = CUTTKT
        XDATE   = CUTTKTH.COMPLETE
        XCONTR1 = MFGOPRHD.cContCode

        SCAN WHILE &XCUTSCAN
          DO CASE
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
              XCUT(1) = IIF(TRANCD='1',XCUT(1)+TOTQTY, XCUT(1)-TOTQTY)
              XCUT(1) = IIF(XCUT(1)>0, XCUT(1), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
              XCUT(2) = IIF(TRANCD='1',XCUT(2)+TOTQTY, XCUT(2)-TOTQTY)
              XCUT(2) = IIF(XCUT(2)>0, XCUT(2), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
              XCUT(3) = IIF(TRANCD='1',XCUT(3)+TOTQTY, XCUT(3)-TOTQTY)
              XCUT(3) = IIF(XCUT(3)>0, XCUT(3), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
              XCUT(4) = IIF(TRANCD='1',XCUT(4)+TOTQTY, XCUT(4)-TOTQTY)
              XCUT(4) = IIF(XCUT(4)>0, XCUT(4), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
              XCUT(5) = IIF(TRANCD='1',XCUT(5)+TOTQTY, XCUT(5)-TOTQTY)
              XCUT(5) = IIF(XCUT(5)>0, XCUT(5), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
              XCUT(6) = IIF(TRANCD='1',XCUT(6)+TOTQTY, XCUT(6)-TOTQTY)
              XCUT(6) = IIF(XCUT(6)>0, XCUT(6), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
              XCUT(7) = IIF(TRANCD='1',XCUT(7)+TOTQTY, XCUT(7)-TOTQTY)
              XCUT(7) = IIF(XCUT(7)>0, XCUT(7), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
              XCUT(8) = IIF(TRANCD='1',XCUT(8)+TOTQTY, XCUT(8)-TOTQTY)
              XCUT(8) = IIF(XCUT(8)>0, XCUT(8), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(9)
              XCUT(9) = IIF(TRANCD='1',XCUT(9)+TOTQTY, XCUT(9)-TOTQTY)
              XCUT(9) = IIF(XCUT(9)>0, XCUT(9), 0)
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(10)
              XCUT(10) = IIF(TRANCD='1',XCUT(10)+TOTQTY, XCUT(10)-TOTQTY)
              XCUT(10) = IIF(XCUT(10)>0, XCUT(10), 0)
            OTHERWISE
              XCUT(10) = IIF(TRANCD='1',XCUT(10)+TOTQTY, XCUT(10)-TOTQTY)
              XCUT(10) = IIF(XCUT(10)>0, XCUT(10), 0)
          ENDCASE
        ENDSCAN

        FOR I = 1 TO XCNT
          XTOTCUT    = XTOTCUT    + XCUT(I)
          XSUBCUT(I) = XSUBCUT(I) + XCUT(I)
        ENDFOR

        IF XWIPRPT .AND. XTOTCUT>0
          *** Display WIP Heading
          IF XFIRSTTIME
            ROW = ROW + 1
            @ ROW,01 SAY XCUTHDR
            ROW = ROW + 1
            @ ROW,01 SAY REPLICATE('Ä',22)
            XFIRSTTIME = .F.
          ENDIF
          ROW = ROW + 1
          DO CASE
            CASE XWIPSORT = 'D'
              @ ROW,01 SAY XDATE
              @ ROW,10 SAY XCUTTKT
              @ ROW,17 SAY XCONTR1
            CASE XWIPSORT = 'F'
              @ ROW,01 SAY XCONTR1
              @ ROW,08 SAY XCUTTKT
              @ ROW,15 SAY XDATE
            OTHERWISE
              @ ROW,01 SAY XCUTTKT
              @ ROW,08 SAY XCONTR1
              @ ROW,15 SAY XDATE
          ENDCASE

          COL = 32
          *** Display WIP Total Line Item
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XCUT(I)  PICTURE '9999999'
            COL = COL + 9
          ENDFOR
          @ ROW,122 SAY XTOTCUT  PICTURE '99999999'
        ENDIF

        IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
          EXIT
        ENDIF
        STORE 0 TO XCUT, XTOTCUT
      ENDDO

      IF XWIPRPT
        COL = 31
        XSUBTOT = 0
        ROW = ROW + 1
        @ ROW,31 SAY REPLICATE('Ä',99)
        ROW = ROW + 1
        @ ROW,06 SAY 'Cuttkt Subtotals ......'
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XSUBCUT(I)  PICTURE '99999999'
          XSUBTOT = XSUBTOT + XSUBCUT(I)
          COL = COL + 9
        ENDFOR
        @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
        ROW = ROW + 1
      ENDIF
    ENDIF
  ENDIF

  *-- If the " Style Purchase Order " or the "Point of Sale" Modules
  *-- is installed
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

    *** Get WIP From Purchase Order Temporary File
    SELE &POTEMP
    LOCATE
    SEEK XSTYLE
    IF FOUND()
      XFIRSTTIME = .T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW = 0
          PAGENO = PAGENO + 1
          DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
          ROW = 5
          *** Display Style, Description, And Range Of Colors
          @ ROW,01 SAY XSTYLE+' '+LEFT(XDESC,17)
          COL = 34
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XCLR(I)
            COL = COL + 9
          ENDFOR
          @ ROW,125 SAY 'TOTAL'
          ROW = ROW + 1
          @ ROW,01 SAY REPLICATE('Ä',129)
          XFIRSTTIME = .T.
        ENDIF

        XPO     = PO
        XVENDOR = VENDOR
        XDATE   = POSHDR.COMPLETE
        ldDate  = POSHDR.AVAILABLE

        SCAN WHILE &XPOSCAN
          IF !INLIST(TranCd , '3' , '6')
            DO CASE
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
                xPOS(1) = IIF(TranCd = '1' ,;
                              xPOS(1) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(1) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(1) = IIF(XPOS(1)>0, XPOS(1), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
                xPOS(2) = IIF(TranCd = '1' ,;
                              xPOS(2) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(2) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(2) = IIF(XPOS(2)>0, XPOS(2), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
                xPOS(3) = IIF(TranCd = '1' ,;
                              xPOS(3) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(3) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(3) = IIF(XPOS(3)>0, XPOS(3), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
                xPOS(4) = IIF(TranCd = '1' ,;
                              xPOS(4) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(4) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(4) = IIF(XPOS(4)>0, XPOS(4), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
                xPOS(5) = IIF(TranCd = '1' ,;
                              xPOS(5) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(5) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(5) = IIF(XPOS(5)>0, XPOS(5), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
                xPOS(6) = IIF(TranCd = '1' ,;
                              xPOS(6) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(6) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(6) = IIF(XPOS(6)>0, XPOS(6), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
                xPOS(7) = IIF(TranCd = '1' ,;
                              xPOS(7) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(7) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(7) = IIF(XPOS(7)>0, XPOS(7), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
                xPOS(8) = IIF(TranCd = '1' ,;
                              xPOS(8) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(8) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(8) = IIF(XPOS(8)>0, XPOS(8), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(9)
                xPOS(9) = IIF(TranCd = '1' ,;
                              xPOS(9) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(9) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(9) = IIF(XPOS(9)>0, XPOS(9), 0)
              CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(10)
                xPOS(10) = IIF(TranCd = '1' ,;
                              xPOS(10) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(10) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(10) = IIF(XPOS(10)>0, XPOS(10), 0)
              OTHERWISE
                xPOS(10) = IIF(TranCd = '1' ,;
                              xPOS(10) +;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                              xPOS(10) -;
                              (TotQty * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
                XPOS(10) = IIF(XPOS(10)>0, XPOS(10), 0)
            ENDCASE
          ENDIF
        ENDSCAN

        FOR I = 1 TO XCNT
          XTOTPOS    = XTOTPOS    + XPOS(I)
          XSUBPOS(I) = XSUBPOS(I) + XPOS(I)
        ENDFOR

        IF XWIPRPT .AND. XTOTPOS>0
          *** Display WIP Heading
          IF XFIRSTTIME
            ROW = ROW + 1
            @ ROW,01 SAY XPOHDR
            ROW = ROW + 1
            @ ROW,01 SAY REPLICATE('Ä',22)
            XFIRSTTIME = .F.
          ENDIF
          ROW = ROW + 1
          DO CASE
            CASE XWIPSORT = 'D'
              @ ROW,01 SAY ldDate
              @ ROW,11 SAY XPO
              @ ROW,18 SAY XVENDOR
            CASE XWIPSORT = 'F'
              @ ROW,01 SAY XVENDOR
              @ ROW,10 SAY XPO
              @ ROW,17 SAY ldDate
            OTHERWISE
              @ ROW,01 SAY XPO
              @ ROW,08 SAY XVENDOR
              @ ROW,17 SAY ldDate
          ENDCASE

          COL = 32
          *** Display WIP Total Line Item
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XPOS(I)  PICTURE '9999999'
            COL = COL + 9
          ENDFOR
          @ ROW,122 SAY XTOTPOS  PICTURE '99999999'
        ENDIF

        IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
          EXIT
        ENDIF
        STORE 0 TO XPOS, XTOTPOS
      ENDDO

      IF XWIPRPT
        COL = 31
        XSUBTOT = 0
        ROW = ROW + 1
        @ ROW,31 SAY REPLICATE('Ä',99)
        ROW = ROW + 1
        @ ROW,06 SAY 'P.O. Subtotals ........'
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XSUBPOS(I)  PICTURE '99999999'
          XSUBTOT = XSUBTOT + XSUBPOS(I)
          COL = COL + 9
        ENDFOR
        @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
        ROW = ROW + 1
      ENDIF
    ENDIF
  ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  IF !('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
    SELE STYLE
    LOCATE
    SEEK XSTYLE
    IF FOUND()
      XFIRSTTIME = .T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW    = 0
          PAGENO = PAGENO + 1
          DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
          ROW = 5
          *** Display Style, Description, And Range Of Colors
          @ ROW,01 SAY XSTYLE
          COL = 34
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XCLR(I)
            COL = COL + 9
          ENDFOR
          @ ROW,125 SAY 'TOTAL'
          ROW = ROW + 1
          @ ROW,01 SAY REPLICATE('-',129)
          XFIRSTTIME = .T.
        ENDIF
        SELECT STYLE
        SCAN WHILE SUBSTR(STYLE,1,lnMajLen) = XSTYLE
          DO CASE
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
              XSTY(1) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
              XSTY(2) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
              XSTY(3) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
              XSTY(4) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
              XSTY(5) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
              XSTY(6) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
              XSTY(7) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
              XSTY(8) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(9)
              XSTY(9) = TOTWIP
            CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(10)
              XSTY(10) = TOTWIP
            OTHERWISE
              XSTY(10) = TOTWIP
          ENDCASE
        ENDSCAN

        FOR I = 1 TO XCNT
          XTOTSTY    = XTOTSTY    + XSTY(I)
          XSUBSTY(I) = XSUBSTY(I) + XSTY(I)
        ENDFOR

        IF XWIPRPT .AND. XTOTPOS>0
          *** Display WIP Heading
          ROW = ROW + 1
          COL = 32
          *** Display WIP Total Line Item
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XSTY(I)  PICTURE '9999999'
            COL = COL + 9
          ENDFOR
          @ ROW,122 SAY XTOTSTY  PICTURE '99999999'
        ENDIF

        IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
          EXIT
        ENDIF
        STORE 0 TO XSTY, XTOTSTY
      ENDDO

      IF XWIPRPT
        COL     = 31
        XSUBTOT = 0
        ROW     = ROW + 1
        @ ROW,31 SAY REPLICATE('-',99)
        ROW = ROW + 1
        @ ROW,06 SAY 'P.O. Subtotals ........'
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XSUBSTY(I)  PICTURE '99999999'
          XSUBTOT = XSUBTOT + XSUBSTY(I)
          COL = COL + 9
        ENDFOR
        @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
        ROW = ROW + 1
      ENDIF
    ENDIF
  ENDIF

  *** Get Sales From Orders Temporary File
  SELE &ORDTEMP
  LOCATE
  SEEK XSTYLE
  IF FOUND()
    XFIRSTTIME = .T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW = 0
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
        ROW = 5
        *** Display Style, Description, And Range Of Colors
        @ ROW,01 SAY XSTYLE+' '+LEFT(XDESC,17)
        COL = 34
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XCLR(I)
          COL = COL + 9
        ENDFOR
        @ ROW,125 SAY 'TOTAL'
        ROW = ROW + 1
        @ ROW,01 SAY REPLICATE('Ä',129)
        XFIRSTTIME = .T.
      ENDIF

      XORDER   = ORDHDR.ORDER
      XACCOUNT = ACCOUNT
      XDATE    = COMPLETE

      SCAN WHILE &XORDSCAN
        DO CASE
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(1)
            XORD(1) = XORD(1) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(2)
            XORD(2) = XORD(2) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(3)
            XORD(3) = XORD(3) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(4)
            XORD(4) = XORD(4) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(5)
            XORD(5) = XORD(5) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(6)
            XORD(6) = XORD(6) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(7)
            XORD(7) = XORD(7) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(8)
            XORD(8) = XORD(8) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(9)
            XORD(9) = XORD(9) + TOTQTY
          CASE SUBSTR(STYLE,lnNonMajPo) = XCOLOR(10)
            XORD(10) = XORD(10) + TOTQTY
          OTHERWISE
            XORD(10) = XORD(10) + TOTQTY
        ENDCASE
      ENDSCAN

      FOR I = 1 TO XCNT
        XTOTORD    = XTOTORD    + XORD(I)
        XSUBORD(I) = XSUBORD(I) + XORD(I)
      ENDFOR

      IF XORDRPT .AND. XTOTORD>0
        *** Display ORD Heading
        IF XFIRSTTIME
          ROW = ROW + 1
          @ ROW,01 SAY XORDHDR
          ROW = ROW + 1
          @ ROW,01 SAY REPLICATE('Ä',22)
          XFIRSTTIME = .F.
        ENDIF
        ROW = ROW + 1
        DO CASE
          CASE XORDSORT = 'D'
            @ ROW,01 SAY XDATE
            @ ROW,10 SAY XORDER
            @ ROW,18 SAY XACCOUNT
          CASE XORDSORT = 'A'
            @ ROW,01 SAY XACCOUNT
            @ ROW,07 SAY XORDER
            @ ROW,15 SAY XDATE
          OTHERWISE
            @ ROW,01 SAY XORDER
            @ ROW,08 SAY XACCOUNT
            @ ROW,15 SAY XDATE
        ENDCASE

        COL = 32
        *** Display ORD Total Line Item
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XORD(I) PICTURE '9999999'
          COL = COL + 9
        ENDFOR
        @ ROW,122 SAY XTOTORD  PICTURE '99999999'
      ENDIF

      IF EOF() .OR. SUBSTR(STYLE,1,lnMajLen)<>XSTYLE
        EXIT
      ENDIF
      STORE 0 TO XORD, XTOTORD
    ENDDO

    IF XORDRPT
      COL     = 31
      XSUBTOT = 0
      ROW     = ROW + 1
      @ ROW,31 SAY REPLICATE('Ä',99)
      ROW     = ROW + 1
      @ ROW,06 SAY 'Orders Subtotals ......'
      FOR I = 1 TO XCNT
        @ ROW,COL SAY XSUBORD(I)  PICTURE '99999999'
        XSUBTOT = XSUBTOT + XSUBORD(I)
        COL     =  COL + 9
      ENDFOR
      @ ROW,121 SAY XSUBTOT  PICTURE '999999999'
      ROW = ROW + 1
    ENDIF
  ENDIF

  *** Display Summary Information
  STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS
  IF ROW > 49
    ROW = 0
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
    ROW = 5
    @ ROW,01 SAY XSTYLE+' '+LEFT(XDESC,17)
    COL = 34
    FOR I = 1 TO XCNT
      @ ROW,COL SAY XCLR(I)
      COL = COL + 9
    ENDFOR
    @ ROW,125 SAY 'TOTAL'
    ROW = ROW + 1
    @ ROW,01 SAY REPLICATE('Ä',129)
  ENDIF

  COL = 32
  ROW = ROW + 1
  @ ROW,00 SAY 'TOTALS FOR '+ALLTRIM(Xstyle)+':'
  ROW = ROW + 1
  @ ROW,13 SAY 'STOCK  .........'
  FOR I = 1 TO XCNT
    @ ROW,COL SAY XSTK(I)  PICTURE '9999999'
    XTOTSTK = XTOTSTK + XSTK(I)
    XOTS(I) = XOTS(I) + XSTK(I)
    nTotStock(I) = nTotStock(I) + XSTK(I)
    COL = COL + 9
  ENDFOR
  @ ROW,122 SAY XTOTSTK    PICTURE '99999999'
  XGRDSTK = XGRDSTK + XTOTSTK

  ROW = ROW + 1
  COL = 32
  @ ROW,13 SAY 'W.I.P  .........'
  FOR I = 1 TO XCNT
    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)  PICTURE '9999999'
    XTOTWIP = XTOTWIP + XSUBCUT(I) + XSUBPOS(I)
    XOTS(I) = XOTS(I) + XSUBCUT(I) + XSUBPOS(I)
    nTotWip(I) = nTotWip(I) + XSUBCUT(I)+XSUBPOS(I)
    COL = COL + 9
  ENDFOR
  @ ROW,122 SAY XTOTWIP       PICTURE '99999999'
  XGRDWIP = XGRDWIP + XTOTWIP

  ROW = ROW + 1
  COL = 32
  @ ROW,13 SAY 'ORDERS .........'
  FOR I = 1 TO XCNT
    @ ROW,COL SAY XSUBORD(I)  PICTURE '9999999'
    XTOTORD = XTOTORD + XSUBORD(I)
    XOTS(I) = XOTS(I) - XSUBORD(I)
    nTotOrder(I) = nTotOrder(I) + XSUBORD(I)
    COL = COL + 9
  ENDFOR
  @ ROW,122 SAY XTOTORD       PICTURE '99999999'
  XGRDORD = XGRDORD + XTOTORD

  ROW = ROW + 1
  @ ROW,13 SAY REPLICATE('Í',117)
  ROW = ROW + 1
  COL = 31
  @ ROW,13 SAY 'O.T.S. .........'
  FOR I = 1 TO XCNT
    @ ROW,COL SAY XOTS(I)     PICTURE '99999999'
    XTOTOTS = XTOTOTS + XOTS(I)
    nTotOts(I) = nTotOts(I) + XOTS(I)
    COL = COL + 9
  ENDFOR
  @ ROW,121 SAY XTOTOTS       PICTURE '999999999'

  SELE STYLE
  SCAN WHILE SUBSTR(STYLE,1,lnMajLen) = XSTYLE
  ENDSCAN

  IF EOF()
    ROW = ROW + 2
    IF ROW > 52
      ROW = 0
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
      ROW = 5
    ENDIF
    @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
    ROW = ROW + 1
    @ ROW,29 SAY 'GRAND TOTALS:  STOCK:'
    @ ROW,51 SAY XGRDSTK  PICTURE '99999999'
    @ ROW,62 SAY 'W.I.P:'
    @ ROW,69 SAY XGRDWIP  PICTURE '99999999'
    @ ROW,80 SAY 'ORDERS:'
    @ ROW,88 SAY XGRDORD  PICTURE '99999999'
    @ ROW,99 SAY 'O.T.S:'
    @ ROW,106 SAY XGRDSTK + XGRDWIP - XGRDORD  PICTURE '99999999'
    ROW = ROW + 1
    @ ROW,29 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*====*=====*=====*==='
    EXIT
  ENDIF
  ROW = ROW + 3
ENDDO
RETURN

*--End of STY961.
*!*************************************************************
*! Name      : STY962.PRG
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : REPORT WITH SIZE BREAKDOWN
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : STY962.PRG
*!*************************************************************
PROCEDURE STY962

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
*COMPLETE CUTTKT CONTR NAME
*99/99/99 123456 12345 123456789012345678901234 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*CONTR NAME                     CUTTKT COMPLETE
*12345 123456789012345678901234 123456 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*CUTTKT CONTR NAME                     COMPLETE
*123456 12345 123456789012345678901234 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999                   X
*                    Cuttkt Subtotals .....    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*START    COMPLETE ORDER  ACCT# NAME
*99/99/99 99/99/99 123456 12345 123456789012345 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*ACCT# NAME            ORDER  START    COMPLETE
*12345 123456789012345 123456 99/99/99 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*ORDER  ACCT# NAME            START    COMPLETE
*123456 12345 123456789012345 99/99/99 99/99/99 99999  99999  99999  99999  99999  99999  99999  99999  999999 1234.12 123456.12 X X
*                    Orders Subtotals .....    999999 999999 999999 999999 999999 999999 999999 999999 9999999        9999999.99
*
*                          STOCK  .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          W.I.P  .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          ORDERS .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*                          ===================================================================================
*                          O.T.S. .........    999999 999999 999999 999999 999999 999999 999999 999999 9999999
*
*....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8....+....9....+....0....+....1....+....2....+....3..
*                          ====*====*====*====*====*====*====*====*====*====*====*====*====*====
*                          TOTAL PIECES ORDERED: 12345678     TOTAL $$$ ORDERED $ 999,999,999.99
*                          ====*====*====*====*====*====*====*====*====*====*====*====*====*====
*** Initialize Variables
STORE SPACE(30) TO XNAME
STORE SPACE(6)  TO XCUTTKT, XPO, XORDER
STORE SPACE(5)  TO XVENDOR, XACCOUNT, XCONTR1
STORE SPACE(6)  TO XCOLOR
STORE SPACE(1)  TO XSTAT, XPIKTKT
STORE CTOD('  /  /  ') TO XDATE, XSTART
STORE 0 TO XCNT, XPRICE, XGRDORD, XGRDPRI
DIMENSION XSTK(8),XOTS(8),XSIZE(8),XCUT(8),XSUBCUT(8),XPOS(8),XSUBPOS(8),XORD(8),XSUBORD(8)

DIMENSION nTotStock(10), nTotWip(10), nTotOrder(10),nTotOts(10)
STORE 0 TO nTotStock, nTotWip, nTotOrder, nTotOts

*** Determine Which Index, Seek, and Scan To Use (WIP)
DO CASE
  CASE XWIPSORT = 'D'                          && Sort by Completion Date
    XCUTSCAN    = '(STYLE+DTOS(CUTTKTH.COMPLETE)+CUTTKT)=(lcStyleCod+DTOS(XDATE)+XCUTTKT)'
    XCUTHDR     = 'COMPLETE CUTTKT CONTR NAME'
    XPOSCAN     = '(STYLE+DTOS(POSHDR.COMPLETE)+PO)=(lcStyleCod+DTOS(XDATE)+XPO)'
    XPOHDR      = 'AVAILABLE PO #  VENDOR NAME'
  CASE XWIPSORT = 'F'                          && Sort by Contractor
    XCUTSCAN    = '(STYLE+MFGOPRHD.cContCode+CUTTKT)=(lcStyleCod+XCONTR1+XCUTTKT)'
    XCUTHDR     = 'CONTR NAME'+SPACE(21)+'CUTTKT COMPLETE'
    XPOSCAN     = '(STYLE+VENDOR+PO)=(lcStyleCod+XVENDOR+XPO)'
    XPOHDR      = 'VENDOR NAME'+SPACE(20)+'PO #   AVAILABLE'
  OTHERWISE                                  && Sort by Cutting Ticket/PO
    XCUTSCAN    = '(STYLE+CUTTKT)=(lcStyleCod+XCUTTKT)'
    XCUTHDR     = 'CUTTKT CONTR NAME'+SPACE(21)+'COMPLETE'
    XPOSCAN     = '(STYLE+PO)=(lcStyleCod+XPO)'
    XPOHDR      = 'PO #   VENDOR   NAME'+SPACE(21)+'AVAILABLE'
ENDCASE

*** Determine Which Index, Seek, and Scan To Use (ORDERS)
DO CASE
  CASE XORDSORT = 'D'                         && Sort by Completion Date
    XORDHDR     = 'START    COMPLETE ORDER  ACCT# NAME'
  CASE XORDSORT = 'A'                         && Sort by Account
    XORDHDR     = 'ACCT# NAME'+SPACE(12)+'ORDER  START    COMPLETE'
  OTHERWISE                                  && Sort by Orders
    XORDHDR     = 'ORDER  ACCT# NAME'+SPACE(12)+'START    COMPLETE'
ENDCASE

SELE STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
LOCATE

DO WHILE INKEY() <> 32
  XSTYLE     = SUBSTR(STYLE,1,lnMajLen)
  lcStyleCod = Style
  XDESC      = DESC
  XSCALE     = SCALE
  XCOLOR     = SUBSTR(STYLE.Style,lnNonMajPo)

  STORE SPACE(3) TO XSIZE
  STORE 0 TO XSTK, XOTS, XCUT, XSUBCUT, XTOTCUT, XPOS, XSUBPOS, XTOTPOS, XORD, XSUBORD, XTOTORD, XSUBEXT

  *** To Get Number of Size Breakdown For Each Style
  XCNT = SCALE.CNT

  *** Load Array With Scale
  FOR I = 1 TO XCNT
    Z = STR(I,1)
    XSIZE(I) = SCALE.SZ&Z
    XSTK(I)  = STK&Z
  ENDFOR

  IF ROW > 54
    ROW = 0
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
    ROW = 5
  ENDIF

  *** Display Style, Description, And Scale Breakdown
  @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ LEFT(gfCodDes(XCOLOR,'COLOR'),15) + ' ' + SUBSTR(XDESC,1,13)
  SELE STYLE

  COL = 50
  FOR I = 1 TO XCNT
    IF LEN(TRIM(XSIZE(I))) < 3
      XSIZE(I) = IIF(LEN(TRIM(XSIZE(I)))=1,'  '+TRIM(XSIZE(I)),XSIZE(I))
      XSIZE(I) = IIF(LEN(TRIM(XSIZE(I)))=2,' '+TRIM(XSIZE(I)),XSIZE(I))
    ENDIF
    @ ROW,COL SAY XSIZE(I)
    COL = COL + 7
  ENDFOR
  IF ROW = 5
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
  ENDIF

  ROW = ROW + 1
  @ ROW,01 SAY REPLICATE('Ä',131)

  *-- If the "Manufacturing" module is installed
  IF 'MF' $ gcComp_Mdl

    *** Get WIP From Cutting Ticket Temporary File
    SELE &CUTTTEMP
    LOCATE
    SEEK lcStyleCod

    IF FOUND()
      XFIRSTTIME = .T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW = 0
          PAGENO = PAGENO + 1
          DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
          ROW = 5
          *** Display Style, Description, And Scale Breakdown

          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+LEFT(gfCodDes(XCOLOR,'COLOR'),15)+' '+SUBSTR(XDESC,1,13)
          SELE &CUTTTEMP

          COL = 50
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XSIZE(I)
            COL = COL + 7
          ENDFOR
          @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
          ROW = ROW + 1
          @ ROW,01 SAY REPLICATE('Ä',131)
          XFIRSTTIME = .T.
        ENDIF

        XCUTTKT = CUTTKT
        XDATE   = CUTTKTH.COMPLETE
        XCONTR1 = MFGOPRHD.cContCode
        XSTAT   = CUTTKTH.STATUS

        SELE APVENDOR
        SEEK XCONTR1
        XNAME = IIF(FOUND(),cVenComp,SPACE(30))

        SELE &CUTTTEMP
        SCAN WHILE &XCUTSCAN
          FOR I = 1 TO XCNT
            Z = STR(I,1)
            XCUT(I) = IIF(TRANCD='1', XCUT(I)+QTY&Z, XCUT(I)-QTY&Z)
            XCUT(I) = IIF(XCUT(I)>0, XCUT(I), 0)
          ENDFOR
        ENDSCAN

        FOR I = 1 TO XCNT
          XTOTCUT    = XTOTCUT    + XCUT(I)
          XSUBCUT(I) = XSUBCUT(I) + XCUT(I)
        ENDFOR

        IF XWIPRPT .AND. XTOTCUT>0
          *** Display WIP Heading
          IF XFIRSTTIME
            ROW = ROW + 1
            @ ROW,01 SAY XCUTHDR
            ROW = ROW + 1
            @ ROW,01 SAY REPLICATE('Ä',46)
            XFIRSTTIME = .F.
          ENDIF
          ROW = ROW + 1
          DO CASE
            CASE XWIPSORT = 'D'
              @ ROW,01 SAY XDATE
              @ ROW,10 SAY XCUTTKT
              @ ROW,17 SAY XCONTR1
              @ ROW,23 SAY SUBSTR(XNAME,1,24)
            CASE XWIPSORT = 'F'
              @ ROW,01 SAY XCONTR1
              @ ROW,07 SAY SUBSTR(XNAME,1,24)
              @ ROW,32 SAY XCUTTKT
              @ ROW,39 SAY XDATE
            OTHERWISE
              @ ROW,01 SAY XCUTTKT
              @ ROW,08 SAY XCONTR1
              @ ROW,14 SAY SUBSTR(XNAME,1,24)
              @ ROW,39 SAY XDATE
          ENDCASE

          COL = 48
          *** Display WIP Total Line Item
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XCUT(I)  PICTURE '99999'
            COL = COL + 7
          ENDFOR
          @ ROW,104 SAY XTOTCUT  PICTURE '999999'
          @ ROW,129 SAY XSTAT
        ENDIF

        IF EOF() .OR. STYLE <> lcStyleCod
          EXIT
        ENDIF
        STORE 0 TO XCUT, XTOTCUT
      ENDDO

      IF XWIPRPT
        COL     = 47
        XSUBTOT = 0
        ROW     = ROW + 1
        @ ROW,47 SAY REPLICATE('Ä',85)
        ROW     = ROW + 1
        @ ROW,21 SAY 'Cuttkt Subtotals .....'
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XSUBCUT(I)  PICTURE '999999'
          XSUBTOT = XSUBTOT + XSUBCUT(I)
          COL = COL + 7
        ENDFOR
        @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
        ROW = ROW + 1
      ENDIF
    ENDIF
  ENDIF    && End of IF 'MF' $ gcComp_Mdl

  *-- If the "Style Purchase Order" or the "Point of Sale" Modules
  *-- is installed
  IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

    *** Get WIP From Purchase Order Temporary File
    SELE &POTEMP
    LOCATE
    SEEK lcStyleCod

    IF FOUND()
      XFIRSTTIME = .T.
      DO WHILE INKEY()<>32
        IF ROW > 54 .AND. !EOF()
          ROW = 0
          PAGENO = PAGENO + 1
          DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
          ROW = 5
          *** Display Style, Description, And Scale Breakdown

          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+LEFT(gfCodDes(XCOLOR,'COLOR'),15)+' '+SUBSTR(XDESC,1,13)
          SELE &POTEMP

          COL = 50
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XSIZE(I)
            COL = COL + 7
          ENDFOR
          @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
          ROW = ROW + 1
          @ ROW,01 SAY REPLICATE('Ä',131)
          XFIRSTTIME = .T.
        ENDIF

        XPO     = PO
        XVENDOR = VENDOR
        XDATE   = POSHDR.COMPLETE
        ldDate  = POSHDR.AVAILABLE
        XSTAT   = POSHDR.STATUS
        SELE APVENDOR
        SEEK XVENDOR
        XNAME = IIF(FOUND(),cVenComp,SPACE(30))

        SELE &POTEMP
        SCAN WHILE &XPOSCAN
          IF !INLIST(TranCd , '3' , '6')
            FOR I = 1 TO XCNT
              Z = STR(I,1)
              xPOS(I) = IIF(TranCd = '1' ,;
                            xPOS(I) +;
                            (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)) ,;
                            xPOS(I) -;
                            (Qty&Z * IIF(POSHDR.cStyType = 'R' , -1 , 1)))
              XPOS(I) = IIF(XPOS(I)>0, XPOS(I), 0)
            ENDFOR
          ENDIF
        ENDSCAN

        FOR I = 1 TO XCNT
          XTOTPOS    = XTOTPOS    + XPOS(I)
          XSUBPOS(I) = XSUBPOS(I) + XPOS(I)
        ENDFOR

        IF XWIPRPT .AND. XTOTPOS>0
          *** Display WIP Heading
          IF XFIRSTTIME
            ROW = ROW + 1
            @ ROW,01 SAY XPOHDR
            ROW = ROW + 1
            @ ROW,01 SAY REPLICATE('Ä',52)
            XFIRSTTIME = .F.
          ENDIF
          ROW = ROW + 1
          DO CASE
            CASE XWIPSORT = 'D'
              @ ROW,01 SAY ldDate
              @ ROW,11 SAY XPO
              @ ROW,18 SAY XVENDOR
              @ ROW,24 SAY LEFT(XNAME,24)
            CASE XWIPSORT = 'F'
              @ ROW,01 SAY XVENDOR
              @ ROW,08 SAY LEFT(XNAME,23)
              @ ROW,32 SAY XPO
              @ ROW,39 SAY ldDate
            OTHERWISE
              @ ROW,01 SAY XPO
              @ ROW,08 SAY XVENDOR
              @ ROW,17 SAY LEFT(XNAME,24)
              @ ROW,42 SAY ldDate
          ENDCASE

          COL = 48
          *** Display WIP Total Line Item
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XPOS(I)  PICTURE '99999'
            COL = COL + 7
          ENDFOR
          @ ROW,104 SAY XTOTPOS  PICTURE '999999'
          @ ROW,129 SAY XSTAT
        ENDIF

        IF EOF() .OR. STYLE <> lcStyleCod
          EXIT
        ENDIF
        STORE 0 TO XPOS, XTOTPOS
      ENDDO

      IF XWIPRPT
        COL     = 47
        XSUBTOT = 0
        ROW     = ROW + 1
        @ ROW,47 SAY REPLICATE('Ä',85)
        ROW     = ROW + 1
        @ ROW,21 SAY 'P.O. Subtotals .......'
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XSUBPOS(I)  PICTURE '999999'
          XSUBTOT = XSUBTOT + XSUBPOS(I)
          COL = COL + 7
        ENDFOR
        @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
        ROW = ROW + 1
      ENDIF
    ENDIF
  ENDIF    && End of IF 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl

  IF !('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
    SELE STYLE
    LOCATE
    SEEK lcStyleCod

    IF FOUND()
      XFIRSTTIME = .T.
        IF ROW > 54 .AND. !EOF()
          ROW = 0
          PAGENO = PAGENO + 1
          DO RPT_HDR WITH 'STY962',XTITLE,R_WIDTH
          ROW = 5
          *** Display Style, Description, And Scale Breakdown
  
          @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+ALLTRIM(STYLE.Desc)
          SELE STYLE

          COL = 50
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XSIZE(I)
            COL = COL + 7
          ENDFOR
          @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
          ROW = ROW + 1
          @ ROW,01 SAY REPLICATE('-',131)
          XFIRSTTIME = .T.
        ENDIF

        SELE STYLE
          FOR I = 1 TO XCNT
            Z = STR(I,1)
            XSTY(I) = WIP&Z
          ENDFOR

        FOR I = 1 TO XCNT
          XTOTSTY    = XTOTSTY    + XSTY(I)
          XSUBSTY(I) = XSUBSTY(I) + XSTY(I)
        ENDFOR

        IF XWIPRPT .AND. XTOTPOS>0
          *** Display WIP Heading

          COL = 48
          *** Display WIP Total Line Item
          FOR I = 1 TO XCNT
            @ ROW,COL SAY XSTY(I)  PICTURE '99999'
            COL = COL + 7
          ENDFOR
          @ ROW,104 SAY XTOTSTY  PICTURE '999999'
          @ ROW,129 SAY XSTAT
        ENDIF
        IF EOF() .OR. STYLE <> lcStyleCod
          EXIT
        ENDIF
        STORE 0 TO XSTY, XTOTSTY
    ENDIF
  ENDIF

  *** Get Sales From Orders Temporary File
  SELE &ORDTEMP
  LOCATE
  SEEK lcStyleCod

  IF FOUND()
    XFIRSTTIME = .T.
    DO WHILE INKEY()<>32
      IF ROW > 54 .AND. !EOF()
        ROW = 0
        PAGENO = PAGENO + 1
        DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
        ROW = 5
        *** Display Style, Description, And Scale Breakdown

        @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+LEFT(gfCodDes(XCOLOR,'COLOR'),15)+' '+SUBSTR(XDESC,1,13)
        SELE &ORDTEMP

        COL = 50
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XSIZE(I)
          COL = COL + 7
        ENDFOR
        @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
        ROW = ROW + 1
        @ ROW,01 SAY REPLICATE('Ä',131)
        XFIRSTTIME = .T.
      ENDIF

      XORDER   = ORDER
      XACCOUNT = ACCOUNT
      XDATE    = COMPLETE
      XSTART   = START
      XSTAT    = IIF(ORDHDR.APPROVAL='DECLINE','D',ORDHDR.STATUS)
      XPIKTKT  = IIF(PICKED=.T.,'Y',' ')
      XPRICE   = PRICE
      SELE CUSTOMER
      SEEK 'M'+XACCOUNT
      XNAME = IIF(FOUND(),BTNAME,SPACE(30))

      SELE &ORDTEMP
      FOR I = 1 TO XCNT
        Z = STR(I,1)
        XORD(I) = IIF(QTY&Z>0,QTY&Z,0)
        XTOTORD = XTOTORD + XORD(I)
        XSUBORD(I) = XSUBORD(I) + XORD(I)
      ENDFOR
      XEXT = XTOTORD*XPRICE
      XSUBEXT = XSUBEXT + XEXT
      XGRDORD = XGRDORD + XTOTORD
      XGRDPRI = XGRDPRI + XEXT

      IF XORDRPT .AND. XTOTORD>0
        *** Display ORD Heading
        IF XFIRSTTIME
          ROW = ROW + 1
          @ ROW,01 SAY XORDHDR
          ROW = ROW + 1
          @ ROW,01 SAY REPLICATE('Ä',46)
          XFIRSTTIME = .F.
        ENDIF

        ROW = ROW + 1
        DO CASE
          CASE XORDSORT = 'D'
            @ ROW,01 SAY XSTART
            @ ROW,10 SAY XDATE
            @ ROW,19 SAY XORDER
            @ ROW,26 SAY XACCOUNT
            @ ROW,32 SAY SUBSTR(XNAME,1,15)
          CASE XORDSORT = 'A'
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

        COL = 48
        *** Display ORD Total Line Item
        FOR I = 1 TO XCNT
          @ ROW,COL SAY XORD(I) PICTURE '99999'
          COL = COL + 7
        ENDFOR
        @ ROW,104 SAY XTOTORD  PICTURE '999999'
        @ ROW,111 SAY XPRICE   PICTURE '9999.99'
        @ ROW,119 SAY XEXT     PICTURE '999999.99'
        @ ROW,129 SAY XSTAT
        @ ROW,131 SAY XPIKTKT
      ENDIF
      SKIP
      IF EOF() .OR. STYLE <> lcStyleCod
        EXIT
      ENDIF
      STORE 0 TO XORD, XTOTORD, XEXT
    ENDDO

    IF XORDRPT
      COL     = 47
      XSUBTOT = 0
      ROW     = ROW + 1
      @ ROW,47 SAY REPLICATE('Ä',85)
      ROW     =  ROW + 1
      @ ROW,21 SAY 'Orders Subtotals .....'
      FOR I = 1 TO XCNT
        @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
        XSUBTOT = XSUBTOT + XSUBORD(I)
        COL = COL + 7
      ENDFOR
      @ ROW,103 SAY XSUBTOT  PICTURE '9999999'
      @ ROW,118 SAY XSUBEXT  PICTURE '9999999.99'
      ROW = ROW + 1
    ENDIF
  ENDIF

  *** Display Summary Information
  STORE 0 TO XTOTSTK, XTOTWIP, XTOTORD, XTOTOTS

  IF ROW > 49
    ROW = 0
    PAGENO = PAGENO + 1
    DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
    ROW = 5

    @ ROW,01 SAY XSTYLE+' '+XCOLOR+' '+LEFT(gfCodDes(XCOLOR,'COLOR'),15)+' '+SUBSTR(XDESC,1,13)
    SELE &ORDTEMP

    COL = 50
    FOR I = 1 TO XCNT
      @ ROW,COL SAY XSIZE(I)
      COL = COL + 7
    ENDFOR
    @ ROW,105 SAY 'TOTAL   PRICE       EXT S P'
    ROW = ROW + 1
    @ ROW,01 SAY REPLICATE('Ä',131)
  ENDIF

  COL = 47
  ROW = ROW + 1
  @ ROW,27 SAY 'STOCK  .........'
  FOR I = 1 TO XCNT
    @ ROW,COL SAY XSTK(I)  PICTURE '999999'
    XTOTSTK = XTOTSTK + XSTK(I)
    XOTS(I) = XOTS(I) + XSTK(I)
    nTotStock(I) = nTotStock(I) + XSTK(I)
    COL = COL + 7
  ENDFOR
  @ ROW,103 SAY XTOTSTK    PICTURE '9999999'

  ROW = ROW + 1
  COL = 47
  @ ROW,27 SAY 'W.I.P  .........'
  FOR I = 1 TO XCNT
    @ ROW,COL SAY XSUBCUT(I)+XSUBPOS(I)  PICTURE '999999'
    XTOTWIP = XTOTWIP + XSUBCUT(I) + XSUBPOS(I)
    XOTS(I) = XOTS(I) + XSUBCUT(I) + XSUBPOS(I)
    nTotWip(I) = nTotWip(I) + XSUBCUT(I)+XSUBPOS(I)
    COL = COL + 7
  ENDFOR
  @ ROW,103 SAY XTOTWIP       PICTURE '9999999'

  ROW = ROW + 1
  COL = 47
  @ ROW,27 SAY 'ORDERS .........'
  FOR I = 1 TO XCNT
    @ ROW,COL SAY XSUBORD(I)  PICTURE '999999'
    XTOTORD = XTOTORD + XSUBORD(I)
    XOTS(I) = XOTS(I) - XSUBORD(I)
    nTotOrder(I) = nTotOrder(I) + XSUBORD(I)
    COL = COL + 7
  ENDFOR
  @ ROW,103 SAY XTOTORD       PICTURE '9999999'

  ROW = ROW + 1
  @ ROW,27 SAY REPLICATE('Í',105)
  ROW = ROW + 1
  COL = 47
  @ ROW,27 SAY 'O.T.S. .........'
  FOR I = 1 TO XCNT
    @ ROW,COL SAY XOTS(I)     PICTURE '999999'
    XTOTOTS = XTOTOTS + XOTS(I)
    nTotOts(I) = nTotOts(I) + XOTS(I)
    COL = COL + 7
  ENDFOR
  @ ROW,103 SAY XTOTOTS       PICTURE '9999999'

  SELE STYLE
  SKIP

  IF Style <> Xstyle
    IF ROW > 52
      ROW = 0
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
      ROW = 5
    ENDIF
    STORE 0 TO nGrdStock,nGrdWip,nGrdOrder,nGrdOts
    COL = 47
    ROW = ROW + 2
    @ ROW,00 SAY 'TOTALS FOR '+ALLTRIM(Xstyle)+':'
    ROW = ROW + 1
    @ ROW,27 SAY 'STOCK  .........'
    FOR I = 1 TO XCNT
      @ ROW,COL SAY nTotStock(I)  PICTURE '999999'   
      nGrdStock = nGrdStock + nTotStock(I)
      COL = COL + 7
    ENDFOR
    @ ROW,103 SAY nGrdStock  PICTURE '9999999'  
    ROW = ROW + 1

    COL = 47
    @ ROW,27 SAY 'W.I.P  .........'
    FOR I = 1 TO XCNT
      @ ROW,COL SAY nTotWip(I)  PICTURE '999999'
      nGrdWip = nGrdWip + nTotWip(I)
      COL = COL + 7
    ENDFOR
    @ ROW,103 SAY nGrdWip  PICTURE '9999999'
    ROW = ROW + 1

    COL = 47
    @ ROW,27 SAY 'ORDERS .........'
    FOR I = 1 TO XCNT
      @ ROW,COL SAY nTotOrder(I) PICTURE '999999'
      nGrdOrder = nGrdOrder + nTotOrder(I)
      COL = COL + 7
    ENDFOR
    @ ROW,103 SAY nGrdOrder  PICTURE '9999999'

    ROW = ROW + 1
    @ ROW,27 SAY REPLICATE('Í',105)
    ROW = ROW + 1
    COL = 47
    @ ROW,27 SAY 'O.T.S. .........'
    FOR I = 1 TO XCNT
      @ ROW,COL SAY nTotOts(I) PICTURE '999999'
      nGrdOts = nGrdOts + nTotOts(I)
      COL = COL + 7
    ENDFOR
    @ ROW,103 SAY nGrdOts  PICTURE '9999999'
    STORE 0 TO  nTotStock,nTotWip,nTotOrder,nTotOts
  ENDIF

  IF EOF()
    ROW = ROW + 2
    IF ROW > 52
      ROW = 0
      PAGENO = PAGENO + 1
      DO RPT_HDR WITH 'KAR100',XTITLE,R_WIDTH
      ROW = 5
    ENDIF
    @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
    ROW = ROW + 1
    @ ROW,27 SAY 'TOTAL PIECES ORDERED:'
    @ ROW,49 SAY XGRDORD  PICTURE '99999999'
    @ ROW,62 SAY 'TOTAL $$$ ORDERED $'
    @ ROW,82 SAY XGRDPRI  PICTURE '999,999,999.99'
    ROW = ROW + 1
    @ ROW,27 SAY '====*====*====*====*====*====*====*====*====*====*====*====*====*===='
    EXIT
  ENDIF
  ROW = ROW + 3
ENDDO
RETURN

*--End of STY962.
*!*************************************************************
*! Name      : lfTermnate
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Function to terminate the report if neither
*!             the "Sales Order" nor the "Point of Sale" modules
*!             is installed.
*!*************************************************************
*! Called from : Default value of the report variable (llTermnate).
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfTermnate

*-- If neither the "Sales Order" nor the "Point of Sale" module is installed.
IF !('SO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  
  *** Message : "Since neither the Sales Order nor the Point of Sale module"
  ***           "is installed, you cannot run this report.                 "
  ***
  *** Buttons : "                         < Ok >                           "
  =gfModalgen("TRM42171B00000","ALERT")
  
  *-- Terminate the Option Grid.
  llOgTrmnat = .T.
ENDIF    && End of IF !('SO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)

*--End of lfTermnate.
*!**************************************************************************
*! Name      : lfSetSTY 
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Go top in the style IN RANGE.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Passed Parameters  : None
*!**************************************************************************
*! Returns            : None
*!**************************************************************************
*! Example   : =lfSetSty()
*!**************************************************************************
FUNCTION lfSetSty
PARAMETERS OpGrdParm

DO CASE
  CASE OpGrdParm = 'S'
   SET ORDER TO TAG CSTYLE IN STYLE   
   GO TOP IN STYLE
  CASE OpGrdParm = 'R'
    SET ORDER TO TAG STYLE IN STYLE
ENDCASE

*--End of lfSetSty.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol isReset
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
PRIVATE lcAliasCst

lcAliasCst = SELECT(0)
SELECT Customer
SET ORDER TO Customer
GO TOP
SELECT(lcAliasCst)

*-- End of lfsrAcc.
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
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

*-- Check the cost access
DIMENSION laRPPrnItm[14]

llCostAccs = gfUserPriv('IC','ICSTYLE')
lnClrSgPo  = ASUBSCRIPT(laOGVrFlt,;
             ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

lnFreSgPo  = ASUBSCRIPT(laOGVrFlt,;
             ASCAN(laOGVrFlt,'SUBSTR(STYLE.Style,lnNonMajPo,lnFreeLen)'),1)

*-- Disable/enable Only This colors, Free Segment.
DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')

  CASE lcFreeClr = 'F'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .T.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')

  OTHERWISE
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnFreSgPo] = .F.
    = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnFreSgPo)) + ',6]')
ENDCASE

*-- Disable/enable Only This colors, Free Segment.

=lfvWIPSuDt()
=lfvSalSuDt()

*--End of lfwRepWhen.
*!*************************************************************
*! Name      : lfMajPic
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Get major seg. picture.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajPic()
*!*************************************************************
FUNCTION lfMajPic

lcMajPic = "@! " + gfItemMask("PM")
RETURN lcMajPic

*--ENd of lfMajPic.
*!*************************************************************
*! Name      : lfvSODate
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Validate sales order date range.
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
IF EMPTY(ldRPSLDate) .AND. EMPTY(ldRPSHDate)
  STORE {} TO ldFrom,ldTo
ELSE
  ldFrom = ldRPSLDate
  ldTo   = ldRPSHDate
ENDIF

lcTitle = 'Sales order comp. date'
DO DateRng.Spx

ldRPSLDate = ldFrom
ldRPSHDate = ldTo

*--End of lfvSODate.
*!*************************************************************
*! Name      : lfvPODate
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
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

IF EMPTY(ldRPPLDate) .AND. EMPTY(ldRPPHDate)
  STORE {} TO ldFrom,ldTo
ELSE
  ldFrom = ldRPPLDate
  ldTo   = ldRPPHDate
ENDIF

lcTitle = 'Prod. order comp. date'
DO DateRng.Spx

ldRPPLDate = ldFrom
ldRPPHDate = ldTo

*--End of lfvPODate.
*!*************************************************************
*! Name      : lfvpbOk
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
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

*--End of lfvpbOk.
*!*************************************************************
*! Name      : lfvWIPSuDt
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
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
FUNCTION lfvWIPSuDt

IF ('MF' $ gcComp_Mdl .OR. 'PO' $ gcComp_Mdl .OR. 'PS' $ gcComp_Mdl)
  lcWIPSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPWIPSor'),1)
  laOGObjCnt[lcWIPSorPo] = lcRPWIPSta = 'D'
  = lfOGShowGet('lcRPWIPSor')
ENDIF

*--End of lfvWIPSuDt.
*!*************************************************************
*! Name      : lfvSalSuDt
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Validate print Sales Summary or Detail option.
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
FUNCTION lfvSalSuDt

lcSalSorPo = ASUBSCRIPT(laOGObjType,ASCAN(laOGObjType,'lcRPSalSor'),1)
laOGObjCnt[lcSalSorPo] = lcRPSalSta = 'D'
= lfOGShowGet('lcRPSalSor')

*--End of lfvSalSuDt.
*!*************************************************************
*! Name      : lfvStyle
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Validate Style.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
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

*--End of lfvStyle.
*!*************************************************************
*! Name      : lfvFabric
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : Validate fabric.
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
FUNCTION lfvFabric

lcFabObj  = VARREAD()
lcFab     = &lcFabObj
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

*--End of lfvFabric.
*!*************************************************************
*! Name      : lfMajTtlGet
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : To get the style major segement title.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMajTtlGet()
*!*************************************************************
FUNCTION lfMajTtGet

RETURN gfItemMask("HM")

*--End of lfMajTtlGet.
*!*************************************************************
*! Name      : lfNonMaj
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : To get the style nonmajor segement structure.
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

*-- Compute Free/Color Items in Style Structure.
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
*-- Compute Free/Color Items in Style Structure.

RETURN ''

*--End of lfNonMaj.
*!*************************************************************
*! Name      : lfBasToClr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/11/2002
*! Purpose   : deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.