*:***************************************************************************
*: Program file  : MFCTREC
*: Program desc. : Cutting Ticket receipt
*: System        : Aria Advantage Series.
*: Module        : MANUFACTURING (MF)
*: Developer     : Hossam El Etreby (HDM)
*:***************************************************************************
*: Calls : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO MFCTREC
*:Modifications  :
*:            B602303,1  HDM 01/05/1999 Incorrect Style Range
*             B802034,1 Table Not Ordered - Set the order of table AP Vendor
*             B802190,1 HDM 04/28/1999 Adjust fields picture to accept huge amounts
*             B802278,1 HDM 05/16/1999 Define the break as style major in case 
*                                      of sorting by style/******
*:            B802368,1 HDM 06/28/1999 Assign '' to laRpTarget
*:***************************************************************************
*B#802346,1 (ABD) 07/22/199 CHK IF THE FILED EMPTY OR NOT. 
*B603673,1 ARH 06/04/2000 Fixed the bug of shifing the report to the right in case of 
*B603673,1 ARH            printing reference field fully
*B605456,1 KHM 02/03/2002 Rounding the landed cost by 3.
*************************************************************************

STORE 0 TO lnNonMajPo

llNThing=lfNonMaj()

DIMENSION laContArr[1] , laFileStru[1]
lcVenFile = 'ApVendor'
lnVenPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'MFGOPRHD.CCONTCODE'),1)
lnCutPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CUTTKT'),1)
lnDatePos = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTL.DATE'),1)
lnDivPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CDIVISION'),1)
lnSesPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.SEASON'),1)
lnGrpPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.CSTYGROUP'),1)
lnStyPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTL.STYLE'),1)
lnFabPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'STYLE.FABRIC'),1)

XVEN = laOGFxFlt[lnVenPos,6]

*-- Cutting ticket range [start]
lcCutVal   = laOGFxFlt[lnCutPos,6]
lnSepPos   = ATC('|' , lcCutVal)
LCT        = SUBSTR(lcCutVal , 1 , lnSepPos - 1)
HCT        = SUBSTR(lcCutVal , lnSepPos + 1 ,LEN(lcCutVal))
*-- Cutting ticket tenge [end]

*-- received Date range [start]
lcDateVal = laOGFxFlt[lnDatePos , 6]
lnSepPos  = ATC('|' , lcDateVal)
LRECEIPT  = CTOD(SUBSTR(lcDateVal , 1 , lnSepPos - 1))
HRECEIPT  = CTOD(SUBSTR(lcDateVal , lnSepPos + 1 ,LEN(lcDateVal)))
*-- Received Date range [end]

*-- Style Range [start]
lcStyVal  = laOGFxFlt[lnStyPos , 6]
lnSepPos  = ATC('|' , lcStyVal)
LSTYLE  = SUBSTR(lcStyVal , 1 , lnSepPos - 1)
HSTYLE  = SUBSTR(lcStyVal , lnSepPos + 1 ,LEN(lcStyVal))
*-- Style Range [end]

*-- Fabric [start]
XFABRIC  = laOGFxFlt[lnFabPos , 6]
*-- Fabric [end]

*-- Style Group [start]
XGRP  = laOGFxFlt[lnGrpPos , 6]
*-- Style Group [end]

*-- Division [start]
XDIV  = laOGFxFlt[lnDivPos , 6]
*-- Division [end]

*-- Season [start]
XSEASON  = laOGFxFlt[lnSesPos , 6]
*-- Division [end]

*-- Status [begin]
XSTATUS  = lcRpStatus
*-- Status [end]
XTRNCD = '2'
SCHOICE  = lcSortBy
lcCstDtl = lcStyCostD

*N2,1 TMI 03/26/95 (BEGIN) Restore costing items name.
DIMENSION laCost [5]
STORE SPACE(9) TO laCost [5]
DIMENSION laEst [5]
DIMENSION laLan [5]
DIMENSION laAct [5]

DIMENSION laSEst [5]
DIMENSION laSLan [5]
DIMENSION laSAct [5]

STORE 0 TO laSEst , laSAct , laSlan ,laEst , laAct , laLan ,lnLan ,lnSubEst,lnSubLan,lnSubAct


laCost[1] = (ALLTRIM(gfGetMemVar('M_cMSLbl1',gcAct_Comp)))
laCost[2] = (ALLTRIM(gfGetMemVar('M_cMSLbl2',gcAct_Comp)))
laCost[3] = (ALLTRIM(gfGetMemVar('M_cMSLbl3',gcAct_Comp)))
laCost[4] = (ALLTRIM(gfGetMemVar('M_cMSLbl4',gcAct_Comp)))
laCost[5] = (ALLTRIM(gfGetMemVar('M_cMSLbl5',gcAct_Comp)))

RELEASE ALL LIKE M_*
STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est
STORE 0 TO lnFinEst  , lnFinLan ,lnFinAct
*N6,1 TMI 03/26/95 (END) Restore costing items name.


QTOPIC='CUTTING TICKET RECEIPTS LOG'
R_WIDTH  = 'W'                               && Standard Report is Wide
XREPORT  = 'C/T Receipt'
R_TITLE  = 'CUTTING TICKET RECEIPTS LOG'

lcDyelot = (ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp)))
XDYELOT_S=IIF(lcDyelot='Y',.T.,.F.)

XTITLE = lcOpTtl
  ****** Two Filters are Used, Filter lenght doesn't fit in one ******
  XFILTER = ' '
  XFILTER1= ' '         && Second filter used to Set Filter on TEMP File
  ERRFLAG = .F.

  ****** Setting Filter by Vendor ******

  IF !EMPTY(ALLTRIM(XVEN))
    *-- MFM 07/12/94.
    *SELE &lcVenFile
    *IF !SEEK(XVEN)
    *  IF llApLink
    *    = gfApVnBrow (@xVen)
    *  ELSE
    *    DO VENBROW WITH XVEN
    *  ENDIF
    *  ERRFLAG = .T.
    *ENDIF
    *-- END MFM 07/12/94.
    *XFILTER1 = XFILTER1 + '.AND. (XVEN = &WORKFILE..CONTR1 .OR. XVEN=&WORKFILE..CONTR2 .OR. XVEN=&WORKFILE..CONTR3)'
    XFILTER1  = XFILTER1 + '.AND. (XVEN = &WORKFILE..CONTR1 .OR. XVEN=&WORKFILE..CONTR2)'


  ENDIF

  ****** Setting Filter by Cutting Ticket ******
  IF !EMPTY(ALLTRIM(LCT)) .OR. !EMPTY(ALLTRIM(HCT))
      IF HCT = ' '
         HCT = LCT
      ENDIF
      IF HCT < LCT
         ERRFLAG = .T.
      ENDIF
      XFILTER = XFILTER + '.AND. CUTTKTL.CUTTKT  >= LCT .AND. CUTTKTL.CUTTKT <= HCT'
  ENDIF

  ****** Setting Filter by Date range ******
  IF DTOC(LRECEIPT)<>' ' .OR. DTOC(HRECEIPT)<>' '
      IF DTOC(HRECEIPT)=' '
         HRECEIPT=LRECEIPT
      ENDIF
      IF HRECEIPT<LRECEIPT
         ERRFLAG = .T.
      ENDIF
      XFILTER = XFILTER+'.AND.CUTTKTL->DATE>=LRECEIPT.AND.CUTTKTL->DATE<=HRECEIPT'
  ENDIF

  ****** Setting Filter by Status [A|O|C|H|X] ******
  XSTATUS1 = TRIM(XSTATUS)
  IF LEN(TRIM(XSTATUS1))<>0
     Y=1
     DO WHILE Y<=LEN(XSTATUS1)
       IF !(SUBSTR(XSTATUS1,Y,1) $ 'AOCHXS')
        ERRFLAG=.T.
        EXIT
       ENDIF
       Y=Y+1
     ENDDO
     IF .NOT. ERRFLAG
      XFILTER = XFILTER+'.AND.CUTTKTH->STATUS$XSTATUS1'
     ENDIF
  ENDIF

  ****** Setting Filter by Style Range ******
  
  IF !EMPTY(ALLTRIM(LSTYLE)) .OR. !EMPTY(ALLTRIM(HSTYLE))
      IF HSTYLE = ' '
         HSTYLE = LSTYLE
      ENDIF
      IF LSTYLE > HSTYLE
         ERRFLAG = .T.
      ENDIF
      XSTYLE = LSTYLE
      SELECT STYLE
      *--HDM B602303,1[start]
      lcStyleOrder = ORDER()
      *SET ORDER TO Style
      SET ORDER TO cStyle
      *--HDM B602303,1[end]
      SEEK XSTYLE
      *IF EOF()
      *   XCOLOR = '*'
      *   SEEK TRIM(XSTYLE)
      *   SET COLOR TO &QCLRSAY
      *   LSTYLE = XSTYLE
      *   HSTYLE = LSTYLE
      *ENDIF
      XFILTER = XFILTER + ".AND. BETWEEN(STYLE,LSTYLE,HSTYLE)"
      SET ORDER TO &lcStyleOrder
  ENDIF

  ****** Setting Filter by Primary Fabric ******
  IF LEN(TRIM(XFABRIC)) <>0
      SELECT FABRIC
      SEEK XFABRIC
      IF EOF()
         XCOLOR = SPACE(6)       &&TAK06/16/94
         SEEK TRIM(XFABRIC)
      ENDIF
      XFILTER = XFILTER + ".AND.STYLE->FABRIC='&XFABRIC'"
  ENDIF

  ****** Setting Filter by Style Group ******
  IF LEN(TRIM(XGRP)) <> 0
      IF LEN(TRIM(XGRP)) <> 0
         XFILTER = XFILTER + ".AND.STYLE->CSTYGROUP='&XGRP'"
      ENDIF
  ENDIF

  ****** Setting Filter by Division ******
  IF LEN(TRIM(XDIV)) <> 0
     IF LEN(TRIM(XDIV)) <> 0
       XFILTER1 = XFILTER1 + ".AND.CUTTKTH->CDIVISION='&XDIV'"
     ENDIF
  ENDIF

  ****** Setting Filter by Season ******
  IF LEN(TRIM(XSEASON)) <> 0
     IF LEN(TRIM(XSEASON)) <> 0
       XFILTER1 = XFILTER1 + ".AND.CUTTKTH->SEASON='&XSEASON'"
     ENDIF
  ENDIF

  ****** Setting Filter by Transaction Code ******

  XFILTER = XFILTER + '.AND.TRANCD$XTRNCD'

  ****** Selecting again the Cutting Ticket Header File ******
  SELECT CUTTKTH
  GOTO TOP

  ****** On Error ******
  IF ERRFLAG
     LOOP
  ENDIF
  ****** Prepare Filters for Selection of Records ******
  IF LEN(XFILTER)>1
     XFILTER=SUBSTR(XFILTER,7)
  ELSE
     XFILTER="VAL(CUTTKT)<>0"
  ENDIF

  IF LEN(XFILTER1)>1
     XFILTER1=SUBSTR(XFILTER1,7)
  ENDIF

  *-------------------------------------------
  * SELECT SORT SEQUENCE
  *-------------------------------------------
  XSTATUS   = XSTATUS1
  SORTFIELD = ' '
  BREAK     = ' '
  BREAKD    = ' '

  DO CASE
    CASE SCHOICE = 'C'
      XSTATUS=IIF(EMPTY(XSTATUS1),SPACE(6),XSTATUS1)
      LOOP
    CASE SCHOICE = 'T'
      SORTFIELD = 'CUTTKT+TRANCD+STYLE+STR(RECNO(),7)'
      BREAK     = 'CUTTKT'
      BREAKD    = 'CUTTKT'
    CASE SCHOICE = 'D'
      SORTFIELD = 'DTOS(DATE)+CUTTKT+TRANCD+STYLE+STR(RECNO(),7)'
      BREAK     = 'DATE'
      BREAKD    = 'DATE'
    CASE SCHOICE = 'S'
      SORTFIELD = 'STYLE+TRANCD+CUTTKT+STR(RECNO(),7)'
      *--B802278,1 HDM [Start] Define the break as style major in case of sorting by style/******
      *BREAK     = 'STYLE'
      BREAK     = "SUBSTR(STYLE,1,LEN(gfItemMask('PM')))"
      *--B802278,1 HDM [End]
      BREAKD    = 'STYLE'
    CASE SCHOICE = 'O'
      SORTFIELD = 'STYLE+TRANCD+CUTTKT+STR(RECNO(),7)'
      BREAK     = 'STYLE'
      BREAKD    = 'STYLE'
  ENDCASE

*********************************************************
****** SELECT REPORT FILE & INITIALIZE MEMORY LOOP ******
*********************************************************

*B800014,1 MFM 07/24/95 (Begin) Print the report with no style costing details.

IF lcCstDtl = 'N'
  DO lpNoCosting
   *E300285,1 MAN Change CLOSE DATABASES to gfClsData
   *CLOSE DATA
*   =gfClsData()
  SET DEVICE TO SCREEN
  RETURN
ENDIF
*B800014,1 MFM 07/24/95 (End).
SET DEVICE TO PRINTER

DO WHILE .T.
   SELECT CUTTKTL
   SET FILTER TO
   SET RELATION TO
   SET RELATION TO CUTTKT INTO CUTTKTH
   SET RELATION TO STYLE INTO STYLE ADDITIVE

   WAIT WINDOW 'Selecting records for report ...' NOWAIT
   LOCATE ALL FOR &XFILTER
   IF EOF()
     WAIT WINDOW 'NO RECORDS SELECTED FOR REPORT!'
     SET DEVICE TO SCREEN
     RETURN
   ENDIF
   WORKFILE = gfTempName()
   *SELECT CUTTKTH
   =AFIELDS(laFileStru)
   lnFileStru = ALEN(laFileStru,1)
   DIMENSION laFileStru[lnFileStru+2,4]

   laFileStru[lnFileStru+1,1] = 'CONTR1'
   laFileStru[lnFileStru+1,2] = 'C'
   laFileStru[lnFileStru+1,3] = 8
   laFileStru[lnFileStru+1,4] = 0

   laFileStru[lnFileStru+2,1] = 'CONTR2'
   laFileStru[lnFileStru+2,2] = 'C'
   laFileStru[lnFileStru+2,3] = 8
   laFileStru[lnFileStru+2,4] = 0
   
   CREATE TABLE &gcWorkDir.&WORKFILE FROM ARRAY laFileStru
   SELECT CUTTKTL
   GO TOP
   SET TALK ON
   SCAN FOR &XFILTER
     SCATTER MEMVAR MEMO
     SELECT MFGOPRHD
     = SEEK('M'+CUTTKT)
     lnIndex = 0
     SCAN REST WHILE cimtyp+ctktno+coprcode =;
                  'M'+CUTTKTL.CutTkt ;
               FOR   !lInHouse AND cContCode = ALLTRIM(Xven)
       lnIndex = lnIndex + 1
       DIMENSION laContArr[lnIndex]
       laContArr[lnIndex] = ALLTRIM(cContCode)
     ENDSCAN
     IF lnIndex  > 0
       m.Contr1 = IIF(lnIndex >= 1,laContArr[1],'')
       m.Contr2 = IIF(lnIndex >= 2,laContArr[2],'')
       lnIndex = 0
     ENDIF
     INSERT INTO (gcWorkDir + WORKFILE) FROM MEMVAR
   ENDSCAN

   *COPY REST TO  (gcWorkDir + WORKFILE) FOR &XFILTER
   SET TALK OFF
  IF !USED('CUTTKTL')
    USE (gcDataDir+'CUTTKTL') IN 0 SHARE
  ENDIF
  SELECT CUTTKTL
  SET RELATION TO
  *B801596,1 KHM 05/20/98 (Begin) Commenting the following line in order
  *B801596,1              to do not close the cuttktl file.
  *USE
  *B801596,1 KHM 05/20/98 (End)
   
  *SELECT 0
  *USE (gcWorkDir + WORKFILE)
  SELECT (WORKFILE)
  SET RELATION TO CUTTKT INTO CUTTKTH
  SET RELATION TO STYLE INTO STYLE ADDITIVE

  ***** If XFILTER1 is not empty, the Sort to Workfile Index Begins  ******
  ***** There are 2 FILTERS: when the lenght of 1 FILTER is too long ******
  IF .NOT. EMPTY(XFILTER1)
     SET FILTER TO &XFILTER1
     GOTO TOP
     IF EOF()
        WAIT WINDOW 'NO RECORDS SELECTED FOR REPORT !!!'
        *E300285,1 MAN Change CLOSE DATABASES to gfClsData
        *CLOSE DATA
       SET DEVICE TO SCREEN
       RETURN
     ENDIF
   ENDIF

   ****** Sort to Workfile Index ******
   IF SORTFIELD <> ' '
      Z = LTRIM(STR(RECCOUNT(),7))
      IF QFOXVER2
        INDEX ON &SORTFIELD TAG &WORKFILE
        SET ORDER TO TAG &WORKFILE
      ELSE
        INDEX ON &SORTFIELD TO &QWD.&WORKFILE
        SET INDEX TO (gcWorkDir + WORKFILE)
      ENDIF
*      @ 23,00
   ENDIF

   ****** Variables Initialization ******
   DIMENSION XTOTAL(1,10),XTOTAL1(1,10)
   XTOTAL = 0.00
   XTOTAL1= 0.00
   *B600868,1 TAK 02/28/96 Added.
   STORE 0 TO lnLandCst1,lnLandCst2,lnLandCst3,lnLandCst4,lnLandCst5
   *B801640,1 SSH 06/22/98 Add the following two variables to hold subtotal and grand total.
   lnSubActual = 0
   lnGrActual  = 0
   lnAct1      = 0
   lnAct2      = 0
   lnAct3      = 0
   lnAct4      = 0
   lnAct5      = 0
   lnSubGrd1   = 0
   lnSubGrd2   = 0
   lnSubGrd3   = 0
   lnSubGrd4   = 0
   lnSubGrd5   = 0
   
   *B801640,1 SSH(END)
   XTIME  = TIME()
   PAGENO = 0
   ROW    = 99

   SELECT &WORKFILE
   GOTO TOP

   IF LEN(TRIM(BREAK)) <> 0
      HBREAK = &BREAK
   ENDIF

   ****** Begin Printing ******
*   DO SEL_DEV
   CLEAR TYPEAHEAD
   SET DEVICE TO SCREEN
*   DO MSG22 WITH 'Report printing - <SPACE BAR> to abort','@'
   SET DEVICE TO PRINT
   SELE &WORKFILE
   *---------------------------------------------------------
   * BEGIN [MAIN REPORT] LOOP
   *---------------------------------------------------------
   lcOldCut = ''
   SET DEVICE TO PRINTER
   DO WHILE .T.   &&INKEY() <> 32
      IF ROW >= 53
         PAGENO = PAGENO+1
         
         DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
         *-Changed TAK06/16/94

         *B603673,1 ARH 06/04/2000 (Begin) Shifted the headres starting from season description 
         *B603673,1 ARH            to the right in order for the report to be alligned properly
         *B603673,1 ARH            in case of printing the reference
         *@ 05,00 SAY 'C.T.     Status  Style               Color        WHouse     Season      Divison      Work In Process Account'
         *@ 06,00 SAY '                 Description         Description  Ref/Dyelot Description Description  Received On Received Qty'

         @ 05,00 SAY 'C.T.     Status  Style               Color        WHouse                     Season      Divison     Work In Process Account'
         @ 06,00 SAY '                 Description         Description  Ref/Dyelot                 Description Description Received On Received Qty'
         *B603673,1 ARH 06/04/2000 (End)

         @ 07,00 SAY REPLICATE('=',132)
         ROW = 08
      ENDIF

      ******---- Begin Subtotals Loop ----******
      DO WHILE LEN(TRIM(BREAK)) <> 0

         IF &BREAK = HBREAK
            EXIT
         ENDIF
         ROW = ROW+1
         @ ROW,00 SAY REPLICATE('-',132)
         ROW = ROW+1
         DO CASE
           CASE BREAKD = 'CUTTKT'
             lnSort    = 'CUTTKT'
           CASE BREAKD = 'STYLE'
             lnSort    = 'STYLE'
           CASE BREAKD = 'COLOR'
             lnSort    = 'STYLE / COLOR'
           CASE BREAKD = 'DATE'
             lnSort    = 'DATE'
         ENDCASE

         @ ROW,00 SAY '************************************ TOTAL FOR &lnSort &HBREAK ***************************************************'
         ROW=ROW+1
         @ ROW,000 SAY '**SUB TOTAL**'
         ROW=ROW+1
         *            *    5    10   15   20   25   30   35   40   45   50   55   60   65   70   75   80   85   90   95   100  105  110  115  120  125  130'  
         *            '----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+'
         @ ROW,00 SAY '                Estimated       Landed       Actual    Act - Lan    Lan - Est    Act - Est     Act/Lan    Lan/Est     Act/Est'
         *            '             99999999.999 99999999.999 99999999.999 99999999.999 99999999.999 99999999.999  999999.999 999999.999  999999.999'
        *@ ROW ,14 SAY'============ ============ ============ ============ ============ ============  ========== ==========  =========='
        *@ ROW ,14 SAY'------------ ------------ ------------ ------------ -----------  ------------  ---------- ----------  ----------'

         ROW=ROW+1
         @ROW ,003 SAY laCost[1]
         @ROW ,13 SAY laSEST[1]	  PICTURE '99999999.999'
         lnSubEst=lnSubEst+laSEST[1]
         @ROW ,026 SAY laSLAN[1]  PICTURE '99999999.999'
         lnSubLan=lnSubLan+laSLAN[1]
         IF lcSTATUS <> 'CLOSED' .AND. BREAK = 'CUTTKT'
            lnSubGrd1 = lnSubGrd1 + lnAct1
            laSAct[1] = laSAct[1] + lnAct1
         ENDIF   
         lnSubAct = lnSubAct+laSAct[1]
         *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
         *@ROW ,35 SAY  laSAct[1]  PICTURE '9999999.999'
         @ROW ,39 SAY  laSAct[1]   PICTURE '99999999.999'
         *-- B802190,1 HDM [end]
         @ROW ,52 SAY (laSACT[1] - laSLAN[1]) PICTURE '99999999.999'
         
         @ROW ,65 SAY (laSLAN[1] - laSEST[1]) PICTURE '99999999.999'
         
         @ROW ,78SAY (laSACT[1] - laSEST[1]) PICTURE '99999999.999'
         @ROW ,92 SAY IIF(laSLAN[1]=0, PADL('N/A',5),(laSACT[1] / laSLAN[1])*100) PICTURE '999999.999'
         @ROW ,103 SAY IIF(laSEST[1]=0,PADL('N/A',5),(laSLAN[1] / laSEST[1])*100) PICTURE '999999.999'
         @ROW ,115 SAY IIF(laSEST[1]=0,PADL('N/A',5),(laSACT[1] / laSEST[1])*100) PICTURE '999999.999'
         ROW=ROW+1
         @ROW ,003 SAY laCost[2]
         @ROW ,13 SAY laSEST[2]  PICTURE '99999999.999'
         lnSubEst=lnSubEst+laSEST[2]
         @ROW ,026 SAY laSLAN[2]        PICTURE '99999999.999'
         lnSubLan=lnSubLan+laSLAN[2]
         *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
         *B801640,1              the total level where the subtotal and grand total are printed.
         IF lcSTATUS <> 'CLOSED'.AND. BREAK = 'CUTTKT'
            lnSubGrd2 = lnSubGrd2 + lnAct2
            laSACT[2] = laSAct[2] + lnAct2
         ENDIF   
         *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
         *@ROW ,35 SAY  laSAct[2]  PICTURE '9999999.999'
         @ROW ,39 SAY  laSAct[2]  PICTURE '99999999.999'
         *-- B802190,1 HDM [end]
         lnSubAct=lnSubAct+laSACT[2]
         *B801640,1 SSH(END)
         @ROW ,52 SAY (laSACT[2] - laSLAN[2]) PICTURE '99999999.999'
         @ROW ,65 SAY (laSLAN[2] - laSEST[2]) PICTURE '99999999.999'
         @ROW ,78 SAY (laSACT[2] - laSEST[2]) PICTURE '99999999.999'
         @ROW ,92 SAY IIF(laSLAN[2]=0, PADL('N/A',5),(laSACT[2] / laSLAN[2])*100) PICTURE '999999.999'
         @ROW ,103 SAY IIF(laSEST[2]=0,PADL('N/A',5),(laSLAN[2] / laSEST[2])*100) PICTURE '999999.999'
         @ROW ,115 SAY IIF(laSEST[2]=0,PADL('N/A',5),(laSACT[2] / laSEST[2])*100) PICTURE '999999.999'
         ROW=ROW+1
         @ROW ,003 SAY laCost[3]
         @ROW ,13 SAY laSEST[3]  PICTURE '99999999.999'
         lnSubEst=lnSubEst+laSEST[3]
         @ROW ,026 SAY laSLAN[3]  PICTURE '99999999.999'
         lnSubLan=lnSubLan+laSLAN[3]
         *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
         *B801640,1              the total level where the subtotal and grand total are printed.
         IF lcSTATUS <> 'CLOSED'.AND. BREAK = 'CUTTKT'
            lnSubGrd3 = lnSubGrd3 + lnAct3
            laSACT[3]=laSAct[3] + lnAct3
         ENDIF   
         *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
         *@ROW ,35 SAY  laSAct[3]  PICTURE '999999.999'
         @ROW ,39 SAY  laSAct[3]  PICTURE '99999999.999'
         *-- B802190,1 HDM [end]
         lnSubAct=lnSubAct+laSACT[3]
         *B801640,1 SSH(END)
         @ROW ,52 SAY (laSACT[3] - laSLAN[3]) PICTURE '99999999.999'
         @ROW ,65 SAY (laSLAN[3] - laSEST[3]) PICTURE '99999999.999'
         @ROW ,78 SAY (laSACT[3] - laSEST[3]) PICTURE '99999999.999'
         @ROW ,92 SAY IIF(laSLAN[3]=0, PADL('N/A',5),(laSACT[3] / laSLAN[3])*100) PICTURE '999999.999'
         @ROW ,103 SAY IIF(laSEST[3]=0,PADL('N/A',5),(laSLAN[3] / laSEST[3])*100) PICTURE '999999.999'
         @ROW ,115 SAY IIF(laSEST[3]=0,PADL('N/A',5),(laSACT[3] / laSEST[3])*100) PICTURE '999999.999'
         ROW=ROW+1
         @ROW ,003 SAY ALLTRIM(laCost[4])
         @ROW ,13 SAY laSEST[4]  PICTURE '99999999.999'
         lnSubEst=lnSubEst+laSEST[4]
         @ROW ,026 SAY laSLAN[4]   PICTURE '99999999.999'
         lnSubLan=lnSubLan+laSLAN[4]
         *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
         *B801640,1              the total level where the subtotal and grand total are printed.
         IF lcSTATUS <> 'CLOSED'.AND. BREAK = 'CUTTKT'
            lnSubGrd4 = lnSubGrd4 + lnAct4
            laSACT[4] = laSAct[4] + lnAct4
         ENDIF   
         *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
         *@ROW ,35 SAY  laSAct[4] PICTURE '9999999.999'
         @ROW ,39 SAY  laSAct[4] PICTURE '99999999.999'
         *-- B802190,1 HDM [end]
         lnSubAct=lnSubAct+laSACT[4]
         *B801640,1 SSH(END)
         @ROW ,52 SAY (laSACT[4] - laSLAN[4]) PICTURE '99999999.999'
         @ROW ,65 SAY (laSLAN[4] - laSEST[4]) PICTURE '99999999.999'
         @ROW ,78 SAY (laSACT[4] - laSEST[4]) PICTURE '99999999.999'
         @ROW ,92 SAY IIF(laSLAN[4]=0, PADL('N/A',5),(laSACT[4] / laSLAN[4])*100) PICTURE  '999999.999'
         @ROW ,103 SAY IIF(laSEST[4]=0,PADL('N/A',5),(laSLAN[4] / laSEST[4])*100) PICTURE '999999.999'
         @ROW ,115 SAY IIF(laSEST[4]=0,PADL('N/A',5),(laSACT[4] / laSEST[4])*100) PICTURE '999999.999'
         ROW=ROW+1
         @ROW ,003 SAY laCost[5]
         @ROW ,13 SAY laSEST[5]  PICTURE '99999999.999'
         lnSubEst=lnSubEst+laSEST[5]
         @ROW ,026 SAY laSLAN[5]  PICTURE '99999999.999'
         lnSubLan=lnSubLan+laSLAN[5]
         *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
         *B801640,1              the total level where the subtotal and grand total are printed.
         IF lcSTATUS <> 'CLOSED'.AND. BREAK = 'CUTTKT'
            lnSubGrd5 = lnSubGrd5 + lnAct5
            laSact[5]=laSAct[5] + lnAct5
         ENDIF   
         *--HDM
         *@ROW ,35 SAY  laSAct[5]  PICTURE '999999.999'
         @ROW ,39 SAY  laSAct[5]  PICTURE '99999999.999'
         lnSubAct=lnSubAct+laSact[5]
         *B801640,1 SSH(END)
         @ROW ,52 SAY (laSACT[5] - laSLAN[5]) PICTURE '99999999.999'
         @ROW ,65 SAY (laSLAN[5] - laSEST[5]) PICTURE '99999999.999'
         @ROW ,78 SAY (laSACT[5] - laSEST[5]) PICTURE '99999999.999'
         @ROW ,92 SAY IIF(laSLAN[5]=0, PADL('N/A',5),(laSACT[5] / laSLAN[5])*100) PICTURE  '999999.999'
         @ROW ,103 SAY IIF(laSEST[5]=0,PADL('N/A',5),(laSLAN[5] / laSEST[5])*100) PICTURE '999999.999'
         @ROW ,115 SAY IIF(laSEST[5]=0,PADL('N/A',5),(laSACT[5] / laSEST[5])*100) PICTURE '999999.999'
         ROW = ROW + 1
         *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
         *@ ROW ,14 SAY '---------- ----------  --------- ----------  --------- ----------  -------   ------- --------'
         @ ROW ,14 SAY'------------ ------------ ------------ ------------ -----------  ------------  ---------- ----------  ----------'
         *-- B802190,1 HDM [end]

         ROW = ROW + 1

         @ ROW , 03 SAY 'Total Cost'
         @ ROW , 13 SAY lnSubEst   PICTURE '99999999.999'
         lnEst = lnEst + laSEST[1]
         @ ROW , 26 SAY lnSubLan   PICTURE '99999999.999'
         lnLan = lnLan+laSlan[1]
         *B801640,1 SSH 06/22/98 Comment out the following line to get the
         *B801640,1 SSH          actual cost from cuttkth in case of not closed cuttkt.
         *@ ROW , 35 SAY  lnSubAct  PICTURE '9999999.999'
         IF lcSTATUS <> 'CLOSED'.AND. BREAK = 'CUTTKT'
            *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
            *@ ROW , 35 SAY lnActual   PICTURE '9999999.999'
            @ ROW , 39 SAY lnActual   PICTURE '99999999.999'
            *-- B802190,1 HDM [end]
            lnGrActual = lnGrActual + lnActual
         ELSE
            *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
            *@ ROW , 35 SAY  lnSubAct  PICTURE '9999999.999'
            @ ROW , 39 SAY  lnSubAct  PICTURE '99999999.999'
            *-- B802190,1 HDM [end]
         ENDIF 
        *B801640,1 SSH(END).
         lnAcT  = lnAcT + laSact[1]
         @ ROW , 52 SAY  lnSubAct-lnSubLan                                                PICTURE '99999999.999'
         @ ROW , 65 SAY  lnSubLan - lnSubEst                                              PICTURE '99999999.999'
         @ ROW , 78 SAY  lnSubAct - lnSubEst                                              PICTURE '99999999.999'
         @ ROW , 92 SAY  IIF(lnSubLan=0, PADL('N/A',5),(lnSubAct / lnSubLan)*100)                  PICTURE '999999.999'
         @ ROW , 103 SAY  IIF(lnSubEst=0,PADL('N/A',5),(lnSubLan / lnSubEst)*100)                 PICTURE '999999.999'
         @ ROW , 115 SAY IIF(lnSubEst=0, PADL('N/A',5),(lnSubAct / lnSubEst)*100)                  PICTURE '999999.999'
         ROW = ROW+1
         *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
         *@ ROW ,14 SAY '========== ========== ==========  ========= ==========  =========  =======  ======== ======== '

         @ ROW ,14 SAY'============ ============ ============ ============ ============ ============  ========== ==========  =========='
         *-- B802190,1 HDM [end]

         STORE 0 TO laSEst , laSAct , laSlan
         *B600868,1 TAK 02/28/96 Added.
         STORE 0 TO lnLandCst1,lnLandCst2,lnLandCst3,lnLandCst4,lnLandCst5
         lnTotQty = totqty
         HBREAK = &BREAK
         EXIT
      ENDDO
      *--------------------- End Subtotals Loop -----------------------

      IF EOF()
         EXIT
      ENDIF

      IF ROW >=53
         ROW = 99
         LOOP
      ENDIF

      DO CASE
        CASE CUTTKTH.STATUS = 'O'
           lcSTATUS='OPEN'
        CASE CUTTKTH.STATUS = 'C'
           lcSTATUS= 'COMPLETE'
        CASE CUTTKTH.STATUS = 'X'
           lcSTATUS='CANCEL'
        CASE CUTTKTH.STATUS = 'S'
           lcSTATUS= 'CLOSED'
        CASE CUTTKTH.STATUS = 'H'
           lcSTATUS= 'HOLD'
        *-- YMA 08/14/95 Include the actualized status
        CASE CUTTKTH.STATUS = 'A'
           lcStatus = "ACTUAL"
        *-- YMA 08/14/95 End.
      ENDCASE

      SELECT &WORKFILE
      ROW=ROW+1
      
      *--HDM B602303,1[START]
      lcColor = SUBSTR(STYLE,lnNonMajPo,19)
      lcColorTtl = gfCodDes(lcColor,'COLOR')
      *--HDM B602303,1[END]

      @ ROW,000 SAY CUTTKT
      @ ROW,010 SAY lcSTATUS
      @ ROW,018 SAY STYLE
      
      *--HDM B602303,1[START]
      *@ ROW,038 SAY COLOR
      @ ROW,038 SAY lcColor
      *--HDM B602303,1[END]
      
      @ ROW,051 SAY CWARECODE

      *B603673,1 ARH 06/04/2000 (Begin) Shifted 15 characters to the right
      *@ ROW,062 SAY SUBSTR(ALLTRIM(CUTTKTH.SEASON),1,2)
      *@ ROW,074 SAY SUBSTR(ALLTRIM(CUTTKTH.CDIVISION),1,2)

      @ ROW,077 SAY SUBSTR(ALLTRIM(CUTTKTH.SEASON),1,2)
      @ ROW,089 SAY SUBSTR(ALLTRIM(CUTTKTH.CDIVISION),1,2)
      *B603673,1 ARH 06/04/2000 (End)

      SELECT GL_LINK
      = SEEK(CUTTKTH.LINK_CODE)

      *B603673,1 ARH 06/04/2000 (Begin) Shifted 15 characters to the right
      *@ ROW , 86 SAY LINK_CODE + '-'
      *@ ROW , 93 SAY IIF(SEEK(CUTTKTH.LINK_CODE+'013'),GLACNT,'')

      @ ROW , 101 SAY LINK_CODE + '-'
      @ ROW , 108 SAY IIF(SEEK(CUTTKTH.LINK_CODE+'013'),GLACNT,'')
      *B603673,1 ARH 06/04/2000 (End)

      SELECT &WORKFILE
      ROW= ROW+1
      
      @ ROW,018 SAY IIF(SEEK(STYLE,'STYLE'),SUBSTR(STYLE.DESC,1,19),SPACE(19))
      *--HDM B602303,1[START]

      *@ ROW,038 SAY IIF(SEEK('C'+COLOR,'CODES'),SUBSTR(CODES.CDATA,1,10),SPACE(10))
      @ ROW,038 SAY SUBSTR(ALLTRIM(lcColorTtl),1,10)
      *--HDM B602303,1[END]

      @ ROW,051 SAY IIF(XDYELOT_S,DYELOT,ALLTRIM(REFERENCE))

*      @ ROW,062 SAY IIF(SEEK('Z'+CUTTKTH.SEASON,'CODES'),SUBSTR(CODES.CDATA,1,10),SPACE(10))
      lcSeasonDs = gfCodDes(CUTTKTH.SEASON,'SEASON')

      *B603673,1 ARH 06/04/2000 (Begin) Shifted 15 characters to the right
      *@ ROW,062 SAY SUBSTR(lcSeasonDs,1,10)

      @ ROW,077 SAY SUBSTR(lcSeasonDs,1,10)
      *B603673,1 ARH 06/04/2000 (End)

      lcDivDsc = gfCodDes(CUTTKTH.CDIVISION,'CDIVISION')

*      @ ROW,074 SAY IIF(SEEK('D'+CUTTKTH.CDIVISION,'CODES'),SUBSTR(CODES.CDATA,1,10),SPACE(10))

      *B603673,1 ARH 06/04/2000 (Begin) Shifted 15 characters to the right
      *@ ROW,074 SAY SUBSTR(lcDivDsc,1,10)
      *@ ROW,86 SAY DATE
      *@ ROW,103 SAY TOTQTY

      @ ROW,089 SAY SUBSTR(lcDivDsc,1,10)
      @ ROW,101 SAY DATE
      @ ROW,118 SAY TOTQTY
      *B603673,1 ARH 06/04/2000 (End)

      ROW=ROW+1
      @ ROW,00 SAY REPLICATE('=',132)
      ROW=ROW+1

      *@ ROW,015 SAY 'Estimated     Landed     Actual  Act - Lan  Lan - Est  Act - Est  Act/Lan   Lan/Est  Act/Est'

      *            *    5    10   15   20   25   30   35   40   45   50   55   60   65   70   75   80   85   90   95   100  105  110  115  120  125  130'  
      *            '----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+'
      @ ROW,00 SAY '                Estimated       Landed       Actual    Act - Lan    Lan - Est    Act - Est     Act/Lan   Lan/Est     Act/Est'
      *            '            99999999.999  99999999.999 99999999.999 99999999.999 99999999.999 99999999.999 99999.999    99999.999   99999.999'
      *@ ROW ,14 SAY' ============  ============ ============ ============ ============ ============ =========    =========   ========='
      *@ ROW ,14 SAY' ------------  ------------ ------------ ------------ -----------  ------------ ---------    ---------   ---------'
      ROW=ROW+1

      *B600868,1 TAK 02/28/96 Changed all nLan_Cost by lnLandCst.
      *--Read the landed cost as ROUND(,2).
      
      *B605456,1 KHM 02/03/2002 (Begin) Removing the rounding.
      *lnLandCst1=ROUND(nLan_Cst1,2)
      *lnLandCst2=ROUND(nLan_Cst2,2)
      *lnLandCst3=ROUND(nLan_Cst3,2)
      *lnLandCst4=ROUND(nLan_Cst4,2)
      *lnLandCst5=ROUND(nLan_Cst5,2)
      
      lnLandCst1 = nLan_Cst1
      lnLandCst2 = nLan_Cst2
      lnLandCst3 = nLan_Cst3
      lnLandCst4 = nLan_Cst4
      lnLandCst5 = nLan_Cst5
      *B605456,1 KHM 02/03/2002 (End)
      
      *B600868,1 TAK 02/28/96 End.

      *B801596,1 KHM 05/20/98 (Begin) Seeking in the cuttktl for transaction
      *B801596,1            of type '1' in order to get the estimated costs.
*      =SEEK(CutTkt+Style+Color+Dyelot+'1','CutTktL')
      =SEEK(CutTkt+Style+Dyelot+'1','CutTktL')

      *B801596,1 KHM 05/20/98 (End)

      @ ROW , 03 SAY laCost[1]

      *B801596,1 KHM 05/20/98 (Begin) Using nCost1 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 13 SAY NCOST1*TotQty                   PICTURE '9999999.999'
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 13 SAY CutTktL.NCOST1*TotQty            PICTURE '9999999.999'
      @ ROW , 13 SAY CutTktL.NCOST1*TotQty            PICTURE '99999999.999'
      *-- B802190,1 HDM [end]

      *B801596,1 KHM 05/20/98 (End)

      @ ROW , 26 SAY lnLandCst1*TotQty                PICTURE '99999999.999'
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 35 SAY NACT_CST1*TotQty                PICTURE '9999999.999'
      @ ROW , 39 SAY NACT_CST1*TotQty                PICTURE '99999999.999'
      
      *@ ROW , 46 SAY (NACT_CST1 - lnLandCst1)*TotQty PICTURE '9999999.999'
      @ ROW , 52 SAY (NACT_CST1 - lnLandCst1)*TotQty PICTURE '99999999.999'
      *-- B802190,1 HDM [end]

      lnAct_Lan= lnAct_Lan+(NACT_CST1 -  lnLandCst1)*TotQty

      *B801596,1 KHM 05/20/98 (Begin) Using nCost1 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 57 SAY (lnLandCst1 - NCST1)*TotQty    PICTURE '9999999.999'
      *lnLan_Est=lnLan_Est+(lnLandCst1 - NCOST1)*TotQty
      *@ ROW , 68 SAY (NACT_CST1 - NCOST1)*TotQty             PICTURE '9999999.999'
      *lnAct_Est=lnAct_Est+(NACT_CST1 - NCOST1)*TotQty
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 57 SAY (lnLandCst1 - CutTktL.NCOST1)*TotQty    PICTURE '9999999.999'
      @ ROW , 65 SAY (lnLandCst1 - CutTktL.NCOST1)*TotQty    PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      lnLan_Est=lnLan_Est+(lnLandCst1 - CutTktL.NCOST1)*TotQty
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 68 SAY (NACT_CST1 - CutTktL.NCOST1)*TotQty             PICTURE '9999999.999'
      @ ROW , 78 SAY (NACT_CST1 - CutTktL.NCOST1)*TotQty             PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      lnAct_Est=lnAct_Est+(NACT_CST1 - CutTktL.NCOST1)*TotQty
      *B801596,1 KHM 05/20/98 (End)
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 81 SAY IIF(lnLandCst1=0,PADL('N/A',5),(NACT_CST1 / lnLandCst1)*100)  PICTURE '9999.99'
      @ ROW , 92 SAY IIF(lnLandCst1=0, PADL('N/A',5),(NACT_CST1 / lnLandCst1)*100)  PICTURE '999999.999'
      *-- B802190,1 HDM [end]

      *B801596,1 KHM 05/20/98 (Begin) Using nCost1 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 91 SAY IIF(NCOST1=0,PADL('N/A',5),(lnLandCst1 / NCOST1)*100)     PICTURE '9999.99'
      *@ ROW , 100 SAY IIF(NCOST1=0,PADL('N/A',5),(NACT_CST1 / NCOST1)*100)   PICTURE '9999.99'
      
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 91 SAY IIF(CutTktL.NCOST1=0,'      N/A',(lnLandCst1 / CutTktL.NCOST1)*100)     PICTURE '9999.99'
      @ ROW , 103 SAY IIF(CutTktL.NCOST1=0,PADL('N/A',5),(lnLandCst1 / CutTktL.NCOST1)*100)     PICTURE '99999.999'
      *-- B802190,1 HDM [end]
      
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 100 SAY IIF(CutTktL.NCOST1=0,'     N/A',(NACT_CST1 / CutTktL.NCOST1)*100)   PICTURE '9999.99'
      @ ROW , 115 SAY IIF(CutTktL.NCOST1=0,PADL('N/A',5),(NACT_CST1 / CutTktL.NCOST1)*100)   PICTURE '99999.999'
      *-- B802190,1 HDM [end]
      *B801596,1 KHM 05/20/98 (End)
      
      ROW = ROW+1
      @ ROW , 03 SAY laCost[2]

      *B801596,1 KHM 05/20/98 (Begin) Using nCost2 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 13 SAY NCOST2 *TotQty                           PICTURE '9999999.999'
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 13 SAY CutTktL.NCOST2 *TotQty                    PICTURE '9999999.999'
      @ ROW , 13 SAY CutTktL.NCOST2 *TotQty                    PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      
      *B801596,1 KHM 05/20/98 (End)
      
      @ ROW , 26 SAY lnLandCst2 *TotQty                       PICTURE '99999999.999'
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 35 SAY NACT_CST2 *TotQty                       PICTURE '99999999.999'
      @ ROW , 39 SAY NACT_CST2 *TotQty                       PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 46 SAY (NACT_CST2 -  lnLandCst2)*TotQty        PICTURE '9999999.999'
      @ ROW , 52 SAY (NACT_CST2 -  lnLandCst2)*TotQty        PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      lnAct_Lan= lnAct_Lan+(NACT_CST1 -  lnLandCst2)*TotQty

      *B801596,1 KHM 05/20/98 (Begin) Using nCost2 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 57 SAY (lnLandCst2 - NCOST2)*TotQty             PICTURE '9999999.999'
      *lnLan_Est=lnLan_Est+(lnLandCst2 - NCOST2)*TotQty
      *@ ROW , 68 SAY (NACT_CST2 - NCOST2)*TotQty             PICTURE '9999999.999'
      *lnAct_Est=lnAct_Est+(NACT_CST2 - NCOST2)*TotQty
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      
      *@ ROW , 57 SAY (lnLandCst2 - CutTktL.NCOST2)*TotQty             PICTURE '9999999.999'
      @ ROW , 65 SAY (lnLandCst2 - CutTktL.NCOST2)*TotQty             PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      
      lnLan_Est=lnLan_Est+(lnLandCst2 - CutTktL.NCOST2)*TotQty
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 68 SAY (NACT_CST2 - CutTktL.NCOST2)*TotQty             PICTURE '9999999.999'
      @ ROW , 78 SAY (NACT_CST2 - CutTktL.NCOST2)*TotQty             PICTURE '99999999.999'
      
      lnAct_Est=lnAct_Est+(NACT_CST2 - CutTktL.NCOST2)*TotQty
      *B801596,1 KHM 05/20/98 (End)
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 81 SAY IIF(lnLandCst2=0,'      N/A',(NACT_CST2 / lnLandCst2)*100)  PICTURE '99999.999'
      @ ROW , 92 SAY IIF(lnLandCst2=0,PADL('N/A',5),(NACT_CST2 / lnLandCst2)*100)  PICTURE '999999.999'
      *-- B802190,1 HDM [end]

      *B801596,1 KHM 05/20/98 (Begin) Using nCost2 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 91 SAY IIF(NCOST2=0,'      N/A',(lnLandCst2 / NCOST2)*100)     PICTURE '9999.99'
      *@ ROW , 100 SAY IIF(NCOST2=0,'      N/A',(NACT_COST2 / NCOST2)*100)   PICTURE '9999.99'
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 91 SAY IIF(CutTktL.NCOST2=0,'      N/A',(lnLandCst2 / CutTktL.NCOST2)*100)     PICTURE '9999.99'
      @ ROW , 103 SAY IIF(CutTktL.NCOST2=0,PADL('N/A',5),(lnLandCst2 / CutTktL.NCOST2)*100)     PICTURE '99999.999'
      *-- B802190,1 HDM [end]
      
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      
      *@ ROW , 100 SAY IIF(CutTktL.NCOST2=0,'      N/A',(NACT_CST2 / CutTktL.NCOST2)*100)   PICTURE '9999.99'
      @ ROW , 115 SAY IIF(CutTktL.NCOST2=0,PADL('N/A',5),(NACT_CST2 / CutTktL.NCOST2)*100)   PICTURE '99999.999'
      *-- B802190,1 HDM [end]
      *B801596,1 KHM 05/20/98 (End)
      
      ROW = ROW+1
      @ ROW , 03 SAY laCost[3]

      *B801596,1 KHM 05/20/98 (Begin) Using nCost3 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 13 SAY NCOST3 *TotQty                  PICTURE '9999999.999'
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 13 SAY CutTktL.NCOST3 * TotQty          PICTURE '9999999.999'
      @ ROW , 13 SAY CutTktL.NCOST3 * TotQty          PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      *B801596,1 KHM 05/20/98 (End)

      @ ROW , 26 SAY lnLandCst3*TotQty                PICTURE '99999999.999'
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 35 SAY NACT_CST3 *TotQty               PICTURE '9999999.999'
      @ ROW , 39 SAY NACT_CST3 *TotQty               PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      @ ROW , 52 SAY (NACT_CST3 - lnLandCst3)*TotQty PICTURE '99999999.999'
      
      lnAct_Lan= lnAct_Lan+(NACT_CST1 -  lnLandCst3)*TotQty

      *B801596,1 KHM 05/20/98 (Begin) Using nCost3 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 57 SAY (lnLandCst3 - NCOST3)*TotQty     PICTURE '9999999.999'
      *lnLan_Est=lnLan_Est+(lnLandCst3 - NCOST3)*TotQty
      *@ ROW , 68 SAY (NACT_COST3 - NCOST3)*TotQty             PICTURE '9999999.999'
      *lnAct_Est=lnAct_Est+(NACT_COST3 - NCOST3)*TotQty

      @ ROW , 65 SAY (lnLandCst3 - CutTktL.NCOST3)*TotQty     PICTURE '99999999.999'
      lnLan_Est=lnLan_Est+(lnLandCst3 - CutTktL.NCOST3)*TotQty
      @ ROW , 78 SAY (NACT_CST3 - CutTktL.NCOST3)*TotQty             PICTURE '99999999.999'
      lnAct_Est=lnAct_Est+(NACT_CST3 - CutTktL.NCOST3)*TotQty
      *B801596,1 KHM 05/20/98 (End)
      
      @ ROW , 92 SAY IIF(lnLandCst3=0,PADL('N/A',5),(NACT_CST3 / lnLandCst3)*100)  PICTURE '999999.999'
      *B801596,1 KHM 05/20/98 (Begin) Using nCost3 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 91 SAY IIF(NCOST3=0,'      N/A',(lnLandCst3 / NCOST3)*100)     PICTURE '9999.99'
      *@ ROW , 100 SAY IIF(NCOST3=0,'      N/A',(NACT_COST3 / NCOST3)*100)   PICTURE '9999.99'
      @ ROW , 103 SAY IIF(CutTktL.NCOST3=0,PADL('N/A',5),(lnLandCst3 / CutTktL.NCOST3)*100)     PICTURE '99999.999'
      @ ROW , 115 SAY IIF(CutTktL.NCOST3=0,PADL('N/A',5),(NACT_CST3 / CutTktL.NCOST3)*100)   PICTURE '99999.999'
      *B801596,1 KHM 05/20/98 (End)
      
      ROW = ROW+1
      @ ROW , 03 SAY laCost[4]

      *B801596,1 KHM 05/20/98 (Begin) Using nCost4 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 13 SAY NCOST4 *TotQty                   PICTURE '9999999.999'
      @ ROW , 13 SAY CutTktL.NCOST4 *TotQty            PICTURE '99999999.999'
      *B801596,1 KHM 05/20/98 (End)

      @ ROW , 26 SAY lnLandCst4 *TotQty                PICTURE '99999999.999'
      *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
      *@ ROW , 35 SAY NACT_CST4 *TotQty                PICTURE '9999999.999'
      @ ROW , 39 SAY NACT_CST4 *TotQty                PICTURE '99999999.999'
      *-- B802190,1 HDM [end]
      @ ROW , 52 SAY (NACT_CST4 -  lnLandCst4)*TotQty PICTURE '99999999.999'
      lnAct_Lan= lnAct_Lan+(NACT_CST1 -  lnLandCst4)*TotQty

      *B801596,1 KHM 05/20/98 (Begin) Using nCost4 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 57 SAY (lnLandCst4 - NCOST4)*TotQty            PICTURE '9999999.999'
      *lnLan_Est=lnLan_Est+(lnLandCst4 - NCOST4)*TotQty
      *@ ROW , 68 SAY (NACT_COST4 - NCOST4)*TotQty             PICTURE '9999999.999'
      *lnAct_Est=lnAct_Est+(NACT_COST4 - NCOST4)*TotQty
      
      @ ROW , 65 SAY (lnLandCst4 - CutTktL.NCOST4)*TotQty            PICTURE '99999999.999'
      lnLan_Est=lnLan_Est+(lnLandCst4 - CutTktL.NCOST4)*TotQty
      @ ROW , 67 SAY (NACT_CST4 - CutTktL.NCOST4)*TotQty             PICTURE '99999999.999'
      lnAct_Est=lnAct_Est+(NACT_CST4 - CutTktL.NCOST4)*TotQty
      *B801596,1 KHM 05/20/98 (End)
      
      @ ROW , 92 SAY IIF(lnLandCst4=0,PADL('N/A',5),(NACT_CST4 / lnLandCst4)*100)  PICTURE '999999.999'

      *B801596,1 KHM 05/20/98 (Begin) Using nCost4 from the master
      *B801596,1              cuttktl file instead of the temporary file.
      *@ ROW , 91 SAY IIF(NCOST4=0,'     N/A',(lnLandCst4 / NCOST4)*100)     PICTURE '9999.99'
      *@ ROW , 100 SAY IIF(NCOST4=0,'      N/A',(NACT_COST4 / NCOST4)*100)   PICTURE '9999.99'
      @ ROW , 103 SAY IIF(CutTktL.NCOST4=0,PADL('N/A',5),(lnLandCst4 / CutTktL.NCOST4)*100)     PICTURE '99999.999'
      @ ROW , 115 SAY IIF(CutTktL.NCOST4=0,PADL('N/A',5),(NACT_CST4 / CutTktL.NCOST4)*100)   PICTURE '99999.999'
      *B801596,1 KHM 05/20/98 (End)
      
     ROW = ROW+1
     @ ROW , 03 SAY laCost[5]

     *B801596,1 KHM 05/20/98 (Begin) Using nCost5 from the master
     *B801596,1              cuttktl file instead of the temporary file.
     *@ ROW , 13 SAY NCOST5 *TotQty                   PICTURE '9999999.999'
     @ ROW , 13 SAY CutTktL.NCOST5 *TotQty            PICTURE '99999999.999'
     *B801596,1 KHM 05/20/98 (End)

     @ ROW , 26 SAY lnLandCst5 *TotQty                 PICTURE '99999999.999'
     *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
     *@ ROW , 35 SAY NACT_CST5 *TotQty                 PICTURE '9999999.999'
     @ ROW , 39 SAY NACT_CST5 *TotQty                 PICTURE '99999999.999'
     
     @ ROW , 52 SAY (NACT_CST5 -  lnLandCst5)*TotQty  PICTURE '99999999.999'
     lnAct_Lan= lnAct_Lan+(NACT_CST1 -  lnLandCst5)*TotQty

     *B801596,1 KHM 05/20/98 (Begin) Using nCost5 from the master
     *B801596,1              cuttktl file instead of the temporary file.
     *@ ROW , 57 SAY (lnLandCst5 - NCOST5)*TotQty       PICTURE '9999999.999'
     *lnLan_Est=lnLan_Est+(lnLandCst5 - NCOST5)*TotQty
     *@ ROW , 68 SAY (NACT_COST5 - NCOST5)*TotQty       PICTURE '9999999.999'
     *lnAct_Est=lnAct_Est+(NACT_COST5 - NCOST5)*TotQty

     @ ROW , 65 SAY (lnLandCst5 - CutTktL.NCOST5)*TotQty       PICTURE '99999999.999'
     lnLan_Est=lnLan_Est+(lnLandCst5 - CutTktL.NCOST5)*TotQty
     @ ROW , 78 SAY (NACT_CST5 - CutTktL.NCOST5)*TotQty       PICTURE '99999999.999'
     lnAct_Est=lnAct_Est+(NACT_CST5 - CutTktL.NCOST5)*TotQty
     *B801596,1 KHM 05/20/98 (End)
     
     @ ROW , 92 SAY IIF(lnLandCst5=0,PADL('N/A',5),(NACT_CST5 / lnLandCst5)*100)  PICTURE '999999.999'

     *B801596,1 KHM 05/20/98 (Begin) Using nCost5 from the master
     *B801596,1              cuttktl file instead of the temporary file.
     *@ ROW , 91 SAY IIF(NCOST5=0,PADL('N/A',5),(lnLandCst5 / NCOST5)*100)     PICTURE '9999.99'
     @ ROW , 103 SAY IIF(CutTktL.NCost5=0,PADL(PADL('N/A',5),5),(lnLandCst5/CutTktL.NCost5)*100)     PICTURE '999999.999'
     *@ ROW , 100 SAY IIF(NCOST5=0,'     N/A',(NACT_COST5 / NCOST5)*100)   PICTURE '9999.99'
     @ ROW , 115 SAY IIF(CutTktL.NCost5=0,PADL('N/A',5),(NACT_CST5/CutTktL.NCost5)*100)   PICTURE '999999.999'
     *B801596,1 KHM 05/20/98 (End)

     ROW = ROW + 1
     *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
     *@ ROW ,14 SAY '---------- ----------  --------- ----------  --------- ----------  -------   ------- --------'
     @ ROW ,14 SAY'------------ ------------ ------------ ------------ -----------  ------------  ---------- ----------  ----------'
     *-- B802190,1 HDM [end]
     ROW = ROW + 1
     

     @ ROW , 03 SAY 'Total Cost'

    *B801596,1 KHM 05/20/98 (Begin) Using the estimated costs from the 
    *B801596,1              master cuttktl file instead of the temporary file.
     *@ ROW , 13 SAY (NCOST1+NCOST2+NCOST3+NCOST4+NCOST5)*TotQty           PICTURE '9999999.999'
     @ ROW , 13 SAY (CutTktL.NCost1+CutTktL.NCost2+CutTktL.NCost3+;
                    CutTktL.NCost4+CutTktL.NCost5)*TotQty PICTURE '99999999.999'
     *lnEst = (NCOST1+NCOST2+NCOST3+NCOST4+NCOST5)*TotQty
     lnEst = (CutTktL.NCost1+CutTktL.NCost2+CutTktL.NCost3+CutTktL.NCost4+;
              CutTktL.NCost5)*TotQty
     *B801596,1 KHM 05/20/98 (End)
     
     @ ROW , 26 SAY (lnLandCst1+lnLandCst2+lnLandCst3+lnLandCst4+lnLandCst5)*TotQty   PICTURE '99999999.999'
     lnLand = (lnLandCst1+lnLandCst2+lnLandCst3+lnLandCst4+lnLandCst5)*TotQty

     *B801640,1 SSH 06/22/98 Comment out the following line to say the
     *B801640,1 SSH          actual cost from cuttkth in case of not closed cuttkt.
     *@ ROW , 35 SAY (NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5)*TotQty   PICTURE '9999999.999'
     IF lcSTATUS <> 'CLOSED'.AND. BREAK = 'CUTTKT'
       lnActual   = CutTktH.NACT_COST1 + CutTktH.NACT_COST2 +CutTktH.NACT_COST3 +CutTktH.NACT_COST4 +CutTktH.NACT_COST5     
       lnAct1     = CutTktH.NACT_COST1
       lnAct2     = CutTktH.NACT_COST2
       lnAct3     = CutTktH.NACT_COST3
       lnAct4     = CutTktH.NACT_COST4
       lnAct5     = CutTktH.NACT_COST5
     ENDIF
     *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
     *@ ROW , 35 SAY (NACT_CST1+NACT_CST2+NACT_CST3+NACT_CST4+NACT_CST5)*TotQty   PICTURE '9999999.999'
     @ ROW , 39 SAY (NACT_CST1+NACT_CST2+NACT_CST3+NACT_CST4+NACT_CST5)*TotQty   PICTURE '99999999.999'
     *-- B802190,1 HDM [end]
     *B801640,1 SSH(END)

     lnAcT  = (NACT_CST1+NACT_CST2+NACT_CST3+NACT_CST4+NACT_CST5)*TotQty
     @ ROW , 52 SAY lnAct - LNLand                                               PICTURE '99999999.999'
     @ ROW , 65 SAY lnLand - lnEst                                               PICTURE '99999999.999'
     @ ROW , 78 SAY lnAct - lnEst                                                PICTURE '99999999.999'

     @ ROW , 92 SAY IIF(lnLand=0,PADL('N/A',5),(lnAcT / lnLand)*100)                 PICTURE '999999.999'
     @ ROW , 103 SAY IIF(lnEst=0,PADL('N/A',5),(lnLand / lnEst)*100)                  PICTURE '999999.999'
     @ ROW , 115 SAY IIF(lnEst=0,PADL('N/A',5),(lnAcT / lnEst)*100)                   PICTURE '999999.999'
     ROW = ROW + 1
     *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
     *@ ROW ,14 SAY '========== ========== ==========  ========= ==========  =========  =======  ======== ======== '

     @ ROW ,14 SAY'============ ============ ============ ============ ============ ============  ========== ==========  =========='
     *-- B802190,1 HDM [end]
     
     FOR I = 1 TO 5
         H= STR(I,1)
         *B801596,1 KHM 05/20/98 (Begin) Using the estimated costs from the 
         *B801596,1              master cuttktl file instead of the 
         *B801596,1              temporary file.
         *laSEST[I] = laSEST[I] + NCOST&H*TOTQTY
         *laEST[I]  = laEST[I]  + NCOST&H*TOTQTY
         laSEST[I] = laSEST[I] + CutTktL.NCost&H*TotQty
         laEST[I]  = laEST[I]  + CutTktL.NCost&H*TotQty
         *B801596,1 KHM 05/20/98 (End)
     ENDFOR
     *B801640,1 SSH 06/22/98 Calculate actual cost to print in sub and grand total.
     *B801711,1 SSH 08/11/98 fix the bug of duplicate actual cost in case of
     *B801711,1               create cutting receipt for cutting ticket thru AP module.
     IF !(lcSTATUS <> 'CLOSED'.AND. BREAK = 'CUTTKT')
       FOR I = 1 TO 5
           H= STR(I,1)
           laSACT[I] = laSACT[I] + NACT_CST&H*TOTQTY
           laACT[I]  = laACT[I]  + NACT_CST&H*TOTQTY
       ENDFOR
     ENDIF
     *B801711,1 SSH(END)
     *B801640,1 SSH(END)
     FOR I = 1 TO 5
         H= STR(I,1)
         *B605456,1 KHM 02/03/2002 (Begin) Rounding the result.
         *laSLAN[I] = laSLAN[I] + lnLandCst&H*TOTQTY
         *laLAN[I]  = laLAN[I]  + lnLandCst&H*TOTQTY
         laSLAN[I] = laSLAN[I] + ROUND(lnLandCst&H*TOTQTY,3)
         laLAN[I]  = laLAN[I]  + ROUND(lnLandCst&H*TOTQTY,3)
         *B605456,1 KHM 02/03/2002 (End)
     ENDFOR

     *B600868,1 TAK 02/28/96 Added to reset also the lnSub???
     STORE 0 TO lnAct_Lan , lnLan_Est , lnAct_Est,lnSubEst,lnSubLan,lnSubAct
      XTOTAL(1,1)=XTOTAL(1,1)+QTY1
      XTOTAL(1,2)=XTOTAL(1,2)+QTY2
      XTOTAL(1,3)=XTOTAL(1,3)+QTY3
      XTOTAL(1,4)=XTOTAL(1,4)+QTY4
      XTOTAL(1,5)=XTOTAL(1,5)+QTY5
      XTOTAL(1,6)=XTOTAL(1,6)+QTY6
      XTOTAL(1,7)=XTOTAL(1,7)+QTY7
      XTOTAL(1,8)=XTOTAL(1,8)+QTY8
      XTOTAL(1,9)=XTOTAL(1,9)+TOTQTY
      SELECT &WORKFILE
        SKIP
   ENDDO
   *------------------ END MAIN REPORT LOOP --------------------

   X = 2
   IF LEN(TRIM(BREAK)) = 0
      X =1
   ENDIF
   ROW=ROW+2
   @ ROW,00 SAY REPLICATE('=',132)
   ROW = ROW+1
   @ ROW,000 SAY '* GRAND TOTAL *'

   ROW = ROW+1
   @ ROW , 03 SAY laCost[1]
   @ ROW , 13 SAY laEST[1]                         PICTURE '99999999.999'
   @ ROW , 26 SAY lalan[1]                         PICTURE '99999999.999'
   *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
   *B801640,1              the total level where the subtotal and grand total are printed.
   *@ ROW , 35 SAY laact[1]                         PICTURE '999999.999'
   laact[1] = laact[1]+lnSubGrd1
   *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
   *@ ROW , 35 SAY laact[1]                PICTURE '999999.999'
   @ ROW , 39 SAY laact[1]                PICTURE '99999999.999'
   *-- B802190,1 HDM [end]
   
   *B801640,1 SSH(END)
   @ ROW , 52 SAY (laAct[1] -  lalan[1])           PICTURE '99999999.999'
   lnAct_Lan= lnAct_Lan+(laAct[1] -  lalan[1])
   @ ROW , 65 SAY (lalan[1] - laEst[1])            PICTURE '99999999.999'
   lnLan_Est=lnLan_Est+(lalan[1] - laEst[1])
   @ ROW , 78 SAY (laAct[1] - laEst[1])            PICTURE '99999999.999'
   lnAct_Est=lnAct_Est+(laAct[1] - laEst[1])
   @ ROW , 92 SAY IIF(lalan[1]=0,PADL('N/A',5),(laAct[1] / lalan[1])*100)  PICTURE '999999.999'
   @ ROW , 103 SAY IIF(laest[1]=0,PADL('N/A',5),(lalan[1] / laest[1])*100)     PICTURE '999999.999'
   @ ROW , 115 SAY IIF(laEst[1]=0,PADL('N/A',5),(laact[1] / laest[1])*100)   PICTURE '999999.999'

   ROW = ROW + 1
   @ ROW , 03 SAY laCost[2]
   @ ROW , 13 SAY laEST[2]                         PICTURE '99999999.999'
   @ ROW , 26 SAY lalan[2]                         PICTURE '99999999.999'
   *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
   *B801640,1              the total level where the subtotal and grand total are printed.
   *@ ROW , 35 SAY laact[2]                         PICTURE '999999.999'
   laact[2] = laact[2]+lnSubGrd2
   *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
   *@ ROW , 35 SAY laact[2]               PICTURE '999999.999'
   @ ROW , 39 SAY laact[2]               PICTURE '99999999.999'
   *-- B802190,1 HDM [end]
   *B801640,1 SSH(END)
   @ ROW , 52 SAY (laAct[2] -  lalan[2])           PICTURE '99999999.999'
   lnAct_Lan= lnAct_Lan+(laAct[2] -  lalan[2])
   @ ROW , 65 SAY (lalan[2] - laEst[2])            PICTURE '99999999.999'
   lnLan_Est=lnLan_Est+(lalan[2] - laEst[2])
   @ ROW , 78 SAY (laAct[2] - laEst[2])            PICTURE '99999999.999'
   lnAct_Est=lnAct_Est+(laAct[2] - laEst[2])
   @ ROW , 92 SAY IIF(lalan[2]=0,PADL('N/A',5),(laAct[2] / lalan[2])*100)  PICTURE '999999.999'
   @ ROW , 103 SAY IIF(laest[2]=0,PADL('N/A',5),(lalan[2] / laest[2])*100)     PICTURE '999999.999'
   @ ROW , 115 SAY IIF(laEst[2]=0,PADL('N/A',5),(laact[2] / laest[2])*100)   PICTURE '999999.999'
   ROW= ROW+1
   @ ROW , 03 SAY laCost[3]
   @ ROW , 13 SAY laEST[3]                         PICTURE '99999999.999'
   @ ROW , 26 SAY lalan[3]                         PICTURE '99999999.999'
   *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
   *B801640,1              the total level where the subtotal and grand total are printed.
   *@ ROW , 35 SAY laact[3]                         PICTURE '999999.999'
   laact[3] = laact[3]+lnSubGrd3
   *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
   *@ ROW , 35 SAY laact[3]               PICTURE '999999.999'
   @ ROW , 39 SAY laact[3]               PICTURE '99999999.999'
   *-- B802190,1 HDM [end]
   *B801640,1 SSH(END)
   @ ROW , 52 SAY (laAct[3] -  lalan[3])           PICTURE '99999999.999'
   lnAct_Lan= lnAct_Lan+(laAct[3] -  lalan[3])
   @ ROW , 65 SAY (lalan[3] - laEst[3])            PICTURE '99999999.999'
   lnLan_Est=lnLan_Est+(lalan[3] - laEst[3])
   @ ROW , 78 SAY (laAct[3] - laEst[3])            PICTURE '99999999.999'
   lnAct_Est=lnAct_Est+(laAct[3] - laEst[3])
   @ ROW , 92 SAY IIF(lalan[3]=0,PADL('N/A',5),(laAct[3] / lalan[3])*100)  PICTURE '999999.999'
   @ ROW , 103 SAY IIF(laest[3]=0,PADL('N/A',5),(lalan[3] / laest[3])*100)     PICTURE '999999.999'
   @ ROW , 115 SAY IIF(laEst[3]=0,PADL('N/A',5),(laact[3] / laest[3])*100)   PICTURE '999999.999'
   ROW= ROW+1
   @ ROW , 03 SAY laCost[4]
   @ ROW , 13 SAY laEST[4]                         PICTURE '99999999.999'
   @ ROW , 26 SAY lalan[4]                         PICTURE '99999999.999'
   *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
   *B801640,1              the total level where the subtotal and grand total are printed.
   *@ ROW , 35 SAY laact[4]                         PICTURE '999999.999'
   laact[4] = laact[4]+lnSubGrd4
   *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
   *@ ROW , 35 SAY laact[4]               PICTURE '999999.999'
   @ ROW , 39 SAY laact[4]               PICTURE '99999999.999'
   *-- B802190,1 HDM [end]
   
   *B801640,1 SSH(END)
   @ ROW , 52 SAY (laAct[4] -  lalan[4])           PICTURE '99999999.999'
   lnAct_Lan= lnAct_Lan+(laAct[4] -  lalan[4])
   @ ROW , 65 SAY (lalan[4] - laEst[4])            PICTURE '99999999.999'
   lnLan_Est=lnLan_Est+(lalan[4] - laEst[4])
   @ ROW , 78 SAY (laAct[4] - laEst[4])            PICTURE '99999999.999'
   lnAct_Est=lnAct_Est+(laAct[4] - laEst[4])
   @ ROW , 92 SAY IIF(lalan[4]=0,PADL('N/A',5),(laAct[4] / lalan[4])*100)  PICTURE '999999.999'
   @ ROW , 103 SAY IIF(laest[4]=0,PADL('N/A',5),(lalan[4] / laest[4])*100)     PICTURE '999999.999'
   @ ROW , 115 SAY IIF(laEst[4]=0,PADL('N/A',5),(laact[4] / laest[4])*100)   PICTURE '999999.999'
   ROW= ROW+1
   @ ROW , 03 SAY laCost[5]
   @ ROW , 13 SAY laEST[5]                         PICTURE '99999999.999'
   @ ROW , 26 SAY lalan[5]                         PICTURE '99999999.999'
   *B801640,1 SSH 06/22/98 Print the actual cost for each costing category at
   *B801640,1              the total level where the subtotal and grand total are printed.
   *@ ROW , 35 SAY laact[5]                         PICTURE '999999.999'
   laact[5] = laact[5]+lnSubGrd5
   *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
   *@ ROW , 35 SAY laact[5]                PICTURE '999999.999'
   @ ROW , 39 SAY laact[5]                PICTURE '99999999.999'
   *-- B802190,1 HDM [end]
   *B801640,1 SSH(END)
   @ ROW , 52 SAY (laAct[5] -  lalan[5])           PICTURE '99999999.999'
   lnAct_Lan= lnAct_Lan+(laAct[5] -  lalan[5])
   @ ROW , 65 SAY (lalan[5] - laEst[5])            PICTURE '99999999.999'
   lnLan_Est=lnLan_Est+(lalan[5] - laEst[5])
   @ ROW , 78 SAY (laAct[5] - laEst[5])            PICTURE '99999999.999'
   lnAct_Est=lnAct_Est+(laAct[5] - laEst[5])
   @ ROW , 92 SAY IIF(lalan[5]=0,PADL('N/A',5),(laAct[5] / lalan[5])*100)  PICTURE '999999.999'
   @ ROW , 103 SAY IIF(laest[5]=0,PADL('N/A',5),(lalan[5] / laest[5])*100)     PICTURE '999999.999'
   @ ROW , 115 SAY IIF(laEst[5]=0,PADL('N/A',5),(laact[5] / laest[5])*100)   PICTURE '999999.999'
   ROW= ROW+1

   FOR I = 1 TO 5
     lnFinEst=lnFinEst+ laEST[I]
     lnFinLan=lnFinLan+ laLan[I]
     lnFinAct=lnFinAct+ laAct[I]
   ENDFOR
   *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
   *@ ROW ,14 SAY '---------- ----------  --------- ----------  --------- ----------  -------   ------- --------'
   @ ROW ,14 SAY'------------ ------------ ------------ ------------ -----------  ------------  ---------- ----------  ----------'
   *-- B802190,1 HDM [end]

   ROW = ROW + 1

   @ ROW , 03 SAY 'Total Cost'
   *B600868,1 TAK 02/28/96
   *lnLand = (lnLandCst1+lnLandCst2+lnLandCst3+lnLandCst4+lnLandCst5)*TotQty
   *lnEst = (NCOST1+NCOST2+NCOST3+NCOST4+NCOST5)*TotQty
   *lnAcT  = (NACT_COST1+NACT_COST2+NACT_COST3+NACT_COST4+NACT_COST5)*TotQty
   *@ ROW , 13 SAY lnFinEst *TotQty  PICTURE '999999.999'
   @ ROW , 13 SAY lnFinEst PICTURE '99999999.999'
   @ ROW , 26 SAY lnFinLan PICTURE '99999999.999'

   *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
   *@ ROW , 35 SAY lnFinAct  PICTURE '9999999.999'
   @ ROW , 39 SAY lnFinAct  PICTURE '99999999.999'
   *-- B802190,1 HDM [end]
   
*B801640,1 SSH(END).

   @ ROW , 52 SAY (lnFinAct-lnFinLan)                                             PICTURE '99999999.999'
   @ ROW , 65 SAY (lnFinLan-lnFinEst)                                             PICTURE '99999999.999'
   @ ROW , 78 SAY (lnFinAct-lnFinEst)                                             PICTURE '99999999.999'
   @ ROW , 92 SAY IIF(lnFinLan=0,PADL('N/A',5),(lnFinAct / lnFinLan)*100)                 PICTURE '999999.999'
   @ ROW , 103 SAY IIF(lnFinEst=0,PADL('N/A',5),(lnFinLan / lnFinEst)*100)                PICTURE '999999.999'
   @ ROW , 115 SAY IIF(lnFinEst=0,PADL('N/A',5),(lnFinAct / lnFinEst)*100)                PICTURE '999999.999'
   ROW = ROW + 1
   *-- B802190,1 HDM [Start] Adjust fields picture to accept huge amounts
   *@ ROW ,14 SAY '========== ========== ==========  ========= ==========  =========  =======  ======== ======== '

   @ ROW ,14 SAY'============ ============ ============ ============ ============ ============  ========== ==========  =========='
   *N2,1 TMI 03/26/95 (ENDING) New layout for the report.
*-- B802190,1 HDM [end]
   ROW = ROW+1
   @ ROW,00 SAY REPLICATE('=',132)
   EXIT
ENDDO

*DO ENDPRT               && END THE REPORT OR DISPLAY ON SCREEN
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN
*** END MFG970.PRG

*:************************************************************************
*: Program file  : lpNoCosting
*: Program desc. : print the report with no style costing details.
*: B800014,1 MFM 07/24/95.
*:*************************************************************
PROCEDURE lpNoCosting

*-- MFM 07/12/94.
*-- In order to increse the contractor code to 8 cahracters we had to remove
*-- contractor3 from the report.
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*MFG970                             CUTTKT RECEIPTS      - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*CUTTKT S CONTR1   CONTR2   SE DV RECEIVED REFERENCE  STYLE        COLOR   QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY
*XXXXXX X XXXXXXXX XXXXXXXX XX XX MM/DD/YY XXXXXXXXXX 123456789012 123456 99999 99999 99999 99999 99999 99999 99999 99999  999999
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
* *SUB TOTALS*    XXXXXXXXXXXX                                            99999 99999 99999 99999 99999 99999 99999 99999  999999
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*-- END MFM 07/12/94.
SET DEVICE TO PRINTER

DO WHILE .T.
   SELECT CUTTKTL
   SET FILTER TO
   SET RELATION TO
   SET RELATION TO CUTTKT INTO CUTTKTH
   SET RELATION TO STYLE INTO STYLE ADDITIVE

   LOCATE ALL FOR &XFILTER
   IF EOF()
     WAIT WINDOW 'NO RECORDS SELECTED FOR REPORT!'
     SET DEVICE TO SCREEN
     RETURN
   ENDIF
   WORKFILE = gfTempName()
   
   =AFIELDS(laFileStru)
   lnFileStru = ALEN(laFileStru,1)
   DIMENSION laFileStru[lnFileStru+2,4]


   laFileStru[lnFileStru+1,1] = 'CONTR1'
   laFileStru[lnFileStru+1,2] = 'C'
   laFileStru[lnFileStru+1,3] = 8
   laFileStru[lnFileStru+1,4] = 0

   laFileStru[lnFileStru+2,1] = 'CONTR2'
   laFileStru[lnFileStru+2,2] = 'C'
   laFileStru[lnFileStru+2,3] = 8
   laFileStru[lnFileStru+2,4] = 0
   
   CREATE TABLE &gcWorkDir.&WORKFILE FROM ARRAY laFileStru

   SET TALK ON
   SELECT CUTTKTL
   GO TOP
   SCAN FOR &XFILTER
     SCATTER MEMVAR MEMO
     SELECT MFGOPRHD
     = SEEK('M'+CUTTKT)
     lnIndex = 0
     SCAN REST WHILE cimtyp+ctktno+coprcode =;
                  'M'+CUTTKTL.CutTkt ;
               FOR   !lInHouse AND cContCode = ALLTRIM(Xven)
       lnIndex = lnIndex + 1
       DIMENSION laContArr[lnIndex]
       laContArr[lnIndex] = ALLTRIM(cContCode)
     ENDSCAN
     IF lnIndex  > 0
       m.Contr1 = IIF(lnIndex >= 1,laContArr[1],'')
       m.Contr2 = IIF(lnIndex >= 2,laContArr[2],'')
       lnIndex = 0
     ENDIF
     INSERT INTO (gcWorkDir + WORKFILE) FROM MEMVAR
   ENDSCAN

   *COPY REST TO (gcWorkDir + WORKFILE) FOR &XFILTER
   SET TALK OFF
   @ 23,00 CLEAR
  IF !USED('CUTTKTL')
    USE (gcDataDir+'CUTTKTL') IN 0 SHARE
  ENDIF

   SELECT CUTTKTL
   SET RELATION TO
   *SELECT 0
   *USE (gcWorkDir + WORKFILE)
   SELECT(WORKFILE)
   SET RELATION TO CUTTKT INTO CUTTKTH
   SET RELATION TO STYLE INTO STYLE ADDITIVE

   ***** If XFILTER1 is not empty, the Sort to Workfile Index Begins  ******
   ***** There are 2 FILTERS: when the lenght of 1 FILTER is too long ******

   IF .NOT. EMPTY(XFILTER1)
     SET FILTER TO &XFILTER1
     GOTO TOP
     IF EOF()
*       DO MSG22 WITH 'NO RECORDS SELECTED FOR REPORT !!!','$'
        *E300285,1 MAN Change CLOSE DATABASES to gfClsData
        *CLOSE DATA
*        =gfClsData()
       SET DEVICE TO SCREEN
       RETURN
     ENDIF
   ENDIF

   ****** Sort to Workfile Index ******
   IF SORTFIELD <> ' '
      Z = LTRIM(STR(RECCOUNT(),7))
*      DO MSG22 WITH  'SORTING &Z RECORDS FOR CUTTING TICKET RECEIPTS LOG ...','@'
      IF QFOXVER2                      && ADDED BY RAA ON 03/10/93
        INDEX ON &SORTFIELD TAG &WORKFILE
        SET ORDER TO TAG &WORKFILE
      ELSE
        INDEX ON &SORTFIELD TO &QWD.&WORKFILE
        SET INDEX TO (gcWorkDir + WORKFILE)
      ENDIF
      @ 23,00 CLEAR
   ENDIF

*-- MFM 07/12/94.
*-- In order to increse the contractor code to 8 cahracters we had to remove
*-- contractor3 from the report.
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*MFG970                             CUTTKT RECEIPTS      - 123456789012345678901234567890                                 PAGE: 1234
*MM/DD/YY
*CUTTKT S CONTR1   CONTR2   SE DV RECEIVED REFERENCE  STYLE        COLOR   QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY
*XXXXXX X XXXXXXXX XXXXXXXX XX XX MM/DD/YY XXXXXXXXXX 123456789012 123456 99999 99999 99999 99999 99999 99999 99999 99999  999999
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
* *SUB TOTALS*    XXXXXXXXXXXX                                            99999 99999 99999 99999 99999 99999 99999 99999  999999
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*-- END MFM 07/12/94.

   ****** Variables Initialization ******
   DIMENSION XTOTAL(1,10),XTOTAL1(1,10)
   XTOTAL = 0.00
   XTOTAL1= 0.00
   XTIME  = TIME()
   PAGENO = 0
   ROW    = 99

   SELECT &WORKFILE
   GOTO TOP

   IF LEN(TRIM(BREAK)) <> 0
      HBREAK = &BREAK
   ENDIF

   ****** Begin Printing ******
*   DO SEL_DEV
   CLEAR TYPEAHEAD
   SET DEVICE TO SCREEN
*   DO MSG22 WITH 'Report printing - <SPACE BAR> to abort','@'
   SET DEVICE TO PRINT
   SELE &WORKFILE

   *---------------------------------------------------------
   * BEGIN [MAIN REPORT] LOOP
   *---------------------------------------------------------
   DO WHILE .T. && HDM -- INKEY() <> 32

      IF ROW >= 53
         PAGENO = PAGENO+1
         
         DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH
         IF XDYELOT_S
           @ 05,00 SAY 'CUTTKT S CONTR1   CONTR2   SE DV RECEIVED DYELOT     STYLE        COLOR   QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY'
         ELSE
           @ 05,00 SAY 'CUTTKT S CONTR1   CONTR2   SE DV RECEIVED REFERENCE  STYLE        COLOR   QTY1  QTY2  QTY3  QTY4  QTY5  QTY6  QTY7  QTY8  TOTQTY'
         ENDIF
         @ 06,00 SAY REPLICATE('=',132)
         ROW = 06
      ENDIF

      ******---- Begin Subtotals Loop ----******
      DO WHILE LEN(TRIM(BREAK)) <> 0
         IF &BREAK = HBREAK
            EXIT
         ENDIF
         ROW = ROW+1
         @ ROW,00 SAY REPLICATE('-',132)
         ROW = ROW+1

         @ ROW,000 SAY '*SUB TOTALS*'
         @ ROW,015 SAY HBREAK
         @ ROW,073 SAY XTOTAL(1,1)  PICTURE '99999'
         @ ROW,079 SAY XTOTAL(1,2)  PICTURE '99999'
         @ ROW,085 SAY XTOTAL(1,3)  PICTURE '99999'
         @ ROW,091 SAY XTOTAL(1,4)  PICTURE '99999'
         @ ROW,096 SAY XTOTAL(1,5)  PICTURE '99999'
         @ ROW,103 SAY XTOTAL(1,6)  PICTURE '99999'
         @ ROW,109 SAY XTOTAL(1,7)  PICTURE '99999'
         @ ROW,115 SAY XTOTAL(1,8)  PICTURE '99999'
         @ ROW,122 SAY XTOTAL(1,9)  PICTURE '999999'
         ROW = ROW+1
         @ ROW,000 SAY REPLICATE('-',132)
         ROW = ROW+1
         X = 1
         DO WHILE X<=10
            XTOTAL1(1,X) = XTOTAL1(1,X) + XTOTAL(1,X)
            XTOTAL(1,X) = 0.00
            X = X+1
         ENDDO

         HBREAK = &BREAK
         EXIT

      ENDDO
      *--------------------- End Subtotals Loop -----------------------
      IF EOF()
         EXIT
      ENDIF

      IF ROW >=53
         ROW = 99
         LOOP
      ENDIF
      SELECT &WORKFILE
      ROW=ROW+1
      @ ROW,000 SAY CUTTKT
      @ ROW,007 SAY CUTTKTH->STATUS
      @ ROW,009 SAY ALLTRIM(&WORKFILE..CONTR1)
      @ ROW,018 SAY ALLTRIM(&WORKFILE..CONTR2)
      @ ROW,027 SAY SUBSTR(ALLTRIM(CUTTKTH->SEASON),1,2)
      @ ROW,030 SAY SUBSTR(ALLTRIM(CUTTKTH->CDIVISION),1,2)
      @ ROW,033 SAY DATE
      IF XDYELOT_S
        @ ROW,042 SAY DYELOT
      ELSE

        *B603673,1 ARH 06/04/2000 (Begin) Adjusted the reference print out to fix the bug of 
        *B603673,1 ARH            shifting the ret of the fields to the right in case of 
        *B603673,1 ARH            printing the reference
        *@ ROW,042 SAY ALLTRIM(REFERENCE)
        @ ROW,042 SAY SUBSTR(ALLTRIM(REFERENCE),1,10)
        *B603673,1 ARH 06/04/2000 (End)
        
      ENDIF
      @ ROW,053 SAY ALLTRIM(SUBSTR(STYLE,1,lnNonMajPo-1))
      *--HDM B602303,1[START]
      lcColor = SUBSTR(STYLE,lnNonMajPo,19)
      lcColorTtl = gfCodDes(lcColor,'COLOR')
      *@ ROW,066 SAY COLOR
      @ ROW,066 SAY lcColor
      *--HDM B602303,1[END]


      @ ROW,073 SAY QTY1 PICTURE '99999'
      @ ROW,079 SAY QTY2 PICTURE '99999'
      @ ROW,085 SAY QTY3 PICTURE '99999'
      @ ROW,091 SAY QTY4 PICTURE '99999'
      @ ROW,096 SAY QTY5 PICTURE '99999'
      @ ROW,103 SAY QTY6 PICTURE '99999'
      @ ROW,109 SAY QTY7 PICTURE '99999'
      @ ROW,115 SAY QTY8 PICTURE '99999'
      @ ROW,122 SAY TOTQTY PICTURE '999999'

      XTOTAL(1,1)=XTOTAL(1,1)+QTY1
      XTOTAL(1,2)=XTOTAL(1,2)+QTY2
      XTOTAL(1,3)=XTOTAL(1,3)+QTY3
      XTOTAL(1,4)=XTOTAL(1,4)+QTY4
      XTOTAL(1,5)=XTOTAL(1,5)+QTY5
      XTOTAL(1,6)=XTOTAL(1,6)+QTY6
      XTOTAL(1,7)=XTOTAL(1,7)+QTY7
      XTOTAL(1,8)=XTOTAL(1,8)+QTY8
      XTOTAL(1,9)=XTOTAL(1,9)+TOTQTY

      SELECT &WORKFILE
      SKIP

   ENDDO
   *------------------ END MAIN REPORT LOOP --------------------

   X = 2
   IF LEN(TRIM(BREAK)) = 0
      X =1
   ENDIF
   ROW=ROW+2
   @ ROW,00 SAY REPLICATE('=',132)
   ROW = ROW+1
   @ ROW,000 SAY '* GRAND TOTAL *'
   @ ROW,073 SAY XTOTAL1(1,1)  PICTURE '99999'
   @ ROW,079 SAY XTOTAL1(1,2)  PICTURE '99999'
   @ ROW,085 SAY XTOTAL1(1,3)  PICTURE '99999'
   @ ROW,091 SAY XTOTAL1(1,4)  PICTURE '99999'
   @ ROW,096 SAY XTOTAL1(1,5)  PICTURE '99999'
   @ ROW,103 SAY XTOTAL1(1,6)  PICTURE '99999'
   @ ROW,109 SAY XTOTAL1(1,7)  PICTURE '99999'
   @ ROW,115 SAY XTOTAL1(1,8)  PICTURE '99999'
   @ ROW,122 SAY XTOTAL1(1,9)  PICTURE '999999'

   ROW = ROW+1
   @ ROW,00 SAY REPLICATE('=',132)
   EXIT
ENDDO

*DO ENDPRT               && END THE REPORT OR DISPLAY ON SCREEN
DO ENDREPORT
SET DEVICE TO SCREEN
RETURN
*********************** END OF 2.6 REPORT ******************************************************
************************************************************************************************
************************************************************************************************
*!*************************************************************
*! Name      : lfvStyle
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for Validating Style Code
*!*************************************************************
*! Called from : Style Field [Option Grid]
*!*************************************************************
*! Calls       : gfStyBrw()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvStyle

lcStyle = VARREAD()
*B602303,1  HDM Incorrect Style Range[START]
*Check if the current value is empty -> do nothing
IF EMPTY(EVAL(lcStyle))
  RETURN
ENDIF
*B602303,1  HDM Incorrect Style Range[END]
SELECT STYLE
lcTag = ORDER()
SET ORDER TO cStyle

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = ALLTRIM(STYLE.cStyMajor)
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF
SET ORDER TO lcTag IN STYLE
*!*************************************************************
*! Name      : lfOldVal
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Keeps the old value of the editor when cancel the browse
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfOldVal

laOldVal = EVALUATE(SYS(18))
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby   (HDM)
*! Date      : 04/08/1998
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

FUNCTION lfwRepWhen
DECLARE laRpSource[6],laRpTarget[1]

*--HDM B802368,1 [Start] assign '' to laRpTarget
STORE '' TO laRpTarget
*--HDM B802368,1 [End]

lnStatus = lcRpStatus
STORE 'Open'     TO laRpSource[1]
STORE 'Hold'     TO laRpSource[2]
STORE 'Canceled' TO laRpSource[3]
STORE 'Complete' TO laRpSource[4]
STORE 'Closed'   TO laRpSource[5]
STORE 'Actual'   TO laRpSource[6]
lcRpStatus = ''

*!*************************************************************
*! Name      : lfvAccount
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for the Vendor field
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
*--HDM B802034,1 Table Not Ordered[start]

SELECT APVENDOR
SET ORDER TO TAG Vencode
*--HDM B802034,1 Table Not Ordered[start]
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK(ALLTRIM(lcObjVal) , 'APVENDOR'))
  SELECT APVENDOR
  SET ORDER TO TAG VenCode 
  llObjRet = gfApVnBrow(@lcObjVal)
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF
*--------------------------------------------------------------------------
FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
FOR lnI = 1 TO ALEN(laRpTarget,1)
  lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
                            IIF(laRpTarget[lnI] = 'Hold','H',;
                            IIF(laRpTarget[lnI] = 'Canceled','X',;
                            IIF(laRpTarget[lnI] = 'Complete','C',;
                            IIF(laRpTarget[lnI] = 'CLOSED'  ,'S',;
                            IIF(laRpTarget[lnI] = 'Actual'  ,'A',''))))))

ENDFOR  && end Loop to make Status expression.

lcRpStatus = IIF(EMPTY(lcRpStatus),lcRpStatus,ALLTRIM(lcRpStatus))

*-- Compare current selected status with old value  [begin]
*-- to rise change status flag.

*-- if length of current selected status differ from previous length 
IF LEN(lcOldStat) != LEN(lcRpStatus) 
  llChStatus = .T.

ELSE  && else if length of current selected status equal previous length
  *-- loop to check if it's the same selected status or not.
  FOR lnJ = 1 TO LEN(lcOldStat)
    lcCurrChr = SUBSTR(lcOldStat,lnJ,lnJ)
    IF !(lcCurrChr $ lcRpStatus)
      llChStatus = .T.
      EXIT
    ENDIF
  ENDFOR  && end loop to check if it's the same selected status or not.
ENDIF
*-- Compare current selected status with old value  [end]
*-- end of lfvOStatus.
*------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvAccount
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for Validating Transaction #
*!*************************************************************
*! Called from : Transaction # [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvCutNum
PRIVATE lcVar , lcObj , laTemp,lcBrowFields ,llFromBrw

lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
lcPrevAl = SELECT()
STORE '' TO lcBrowCond
STORE .F. TO llFromBrw

DECLARE laTemp[1]
  SELECT CUTTKTH
  SET ORDER TO CUTTKTH
  DIMENSION laTemp[1]
  IF !EMPTY(lcObj) AND !(SEEK(lcObj , 'CUTTKTH'))
    lcBrFields ="CUTTKT    :R :H='CUTTKT'   ,"+;
                "STYLE     :R :H='Style'    ,"+;
                "STATUS    :R :H='Status'   ,"+;
                "ENTERED   :R :H='Issue'    ,"+;
                "COMPLETE  :R :H='Complete' ,"+;
                "SEASON    :R :H='Season'   ,"+;
                "CDIVISION :R :H='Division' ,"+;
                "PCS_BUD   :R :H='Budget'   ,"+;
                "PCS_REC   :R :H='Received' ,"+;
                "PCS_DAM   :R :H='Damaged'  ,"+;
                "PCS_OPN   :R :H='Open' "
    lNThing    = gfBrows(lcBrowCond,'CUTTKT','laTemp')
    llFromBrw = .T.
  ENDIF
*-- IF The user selected a record
IF llFromBrw
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF
ENDIF
&lcVar = lcObj      && Update the field

SELECT (lcPrevAl)
*!*************************************************************
*! Name      : lfvFabric
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvFabric

lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.
IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

*B#802346,1  CHK IF THE FILED EMPTY OR NOT [ BEGIN ]
IF LASTKEY() = 13 AND !MDOWN() .OR.  !EMPTY(lcFab)
*B#802346,1 [END]
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ENDIF

SET ORDER TO FABRIC IN FABRIC

IF llUseByMe
  USE IN FABRIC
ENDIF  
*---------------------------------------------------

FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)
  IF laMajSeg[lnI,1] = 'F'
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)
    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])
    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))
  ENDIF
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'
      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
      lcNonMajPi = laMajSeg[lnI,3]
      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
    ENDIF
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
