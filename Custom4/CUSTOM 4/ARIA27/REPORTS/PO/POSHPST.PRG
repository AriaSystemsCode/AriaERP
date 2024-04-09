*:***************************************************************************
*: Program file  : POSHPST
*: Program desc. : P/O Shipment Status (PO)
*: System        : Aria Advantage Series.
*: Module        : Style Purchase Orders
*: Developer     : Hossam El Etreby (HDM)
*:***************************************************************************
*: Calls :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example       : DO POSHPST
*:***************************************************************************
*B802621,1 RAMY 09/28/1999 when I filter to see one shipment status,
*                               it still gives all status
*B603340,1 WMA 03/22/2000  When I Enter Shipment # I can't do it without doing
*                               browes
*                          When I saving setting with title , title not found
*                               if I select this sitting again
*E301404,1 RAMY 05/21/2000 Add new form to the report
*B803882,1 AMH 12/04/2000 Fix Sorting by E.T.A. Date.
*B604535,1 KHM 06/11/2001 Fix the bug that cause the printer to be locked.
*B605305,1 AMH 01/01/2002 Remove the default date in the date entered and eta options.
*B605820,1 KHM 04/11/2002 Fix the problem of not printing the shipvia descrip.
*B605820,1                in the FRX
*B606565,1 KHM 10/24/2002 Fix the problem of alias shpdates not found.
*B123167,1 BWA 10/06/2004 Fix the bug of printing more records than the wanted one.
*B608448,1 AKA 21/02/2008 Fix the bug of Incorrect Fields on Shipment Report. the fix done in the FRX as well 
*:***************************************************************************

*E301404,1 RAMY start

*B123167,1 BWA 10/06/2004 Get the value of the lcRpStatus variable everytime the user run the program.[START]
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
      IIF(laRpTarget[lnI] = 'Canceled','X',;
      IIF(laRpTarget[lnI] = 'Complete','C','')))
  ENDFOR  && end Loop to make Status expression.
  lcRpExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp +' .AND. SHPMTHDR.STATUS $ (lcRpStatus)' )
ENDIF
*B123167,1 BWA 10/06/2004.[END]

lcFormName = lcRpForm
lcRpOpTitl = lcRpOpTtl
IF lcRpChos = 'A'
  *--'HDM' Preparing Filter Variables [Start]
  lnShpPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.SHIPNO'),1)
  lnEntPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.ENTERED'),1)
  lnETAPos  = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'SHPMTHDR.ETA'),1)

  *-- SHP# Range [start]
  lcShpVal   = laOGFxFlt[lnShpPos,6]
  lnSepPos   = ATC('|' , lcShpVal)
  LSHPNO     = SUBSTR(lcShpVal , 1 , lnSepPos - 1)
  HSHPNO     = SUBSTR(lcShpVal , lnSepPos + 1 ,LEN(lcShpVal))
  *-- SHP# Renge [end]

  *-- Entered Range [start]
  lcEntVal   = laOGFxFlt[lnEntPos,6]
  lnSepPos   = ATC('|' , lcEntVal)
  LENTERED   = CTOD(SUBSTR(lcEntVal , 1 , lnSepPos - 1))
  HENTERED   = CTOD(SUBSTR(lcEntVal , lnSepPos + 1 ,LEN(lcEntVal)))
  *-- Entered Renge [end]

  *-- ETA Range [start]
  lcETAVal   = laOGFxFlt[lnETAPos,6]
  lnSepPos   = ATC('|' , lcETAVal)
  LETADAY    = CTOD(SUBSTR(lcETAVal , 1 , lnSepPos - 1))
  HETADAY    = CTOD(SUBSTR(lcETAVal , lnSepPos + 1 ,LEN(lcETAVal)))

  *-- ETA Renge [end]
  XSTATUS  = lcRpStatus
  XSTATUS1 = lcRpStatus
  *RIRI
  *SCHOICE  = lcSortBy
  SCHOICE  = lcRpSortBy
  XREPORT  = 'POSHPST'

  *B603340,1 (WMA) Change Variable Name in Option Grade and PRG
  *XTITLE   = lcOpTtl
  XTITLE   = lcRpOpTitl
  *B603340,1 [END]

  R_WIDTH  = 'W'
  R_TITLE  = 'SHIPMENT STATUS REPORT'
  *--'HDM' Preparing Filter Variables [End]

  ****** Setting Filter by Ship Number range ******
  IF (LSHPNO <> ' ') .OR. (HSHPNO <> ' ')
    IF HSHPNO=' '
      HSHPNO = LSHPNO
    ENDIF
    IF HSHPNO < LSHPNO
      *    DO MSG22 WITH 'SHIPMENT NUMBER FROM/THRU ERROR!','$'
      WAIT WINDOW 'SHIPMENT NUMBER FROM/THRU ERROR!'
      *    ERRFLAG = .T.
    ENDIF
    XFILTER = 'SHIPNO >= LSHPNO .AND. SHIPNO <= HSHPNO'
  ENDIF
  ****** Setting Filter by Date Entered range ******
  IF DTOC(LENTERED)<>' ' .OR. DTOC(HENTERED)<>' '
    IF DTOC(HENTERED)=' '
      HENTERED=LENTERED
    ENDIF
    IF HENTERED<LENTERED
      *    DO MSG22 WITH 'FROM/THRU ERROR - ENTERED DATE!','$'
      WAIT WINDOW 'FROM/THRU ERROR - ENTERED DATE!'
      ERRFLAG = .T.
    ENDIF
    XFILTER = XFILTER + ' .AND. ENTERED >= LENTERED .AND. ENTERED <= HENTERED'
  ENDIF
  ****** Setting Filter by E.T.A. Date range ******
  IF DTOC(LETADAY)<>' ' .OR. DTOC(HETADAY)<>' '
    IF DTOC(HETADAY)=' '
      HETADAY=LETADAY
    ENDIF
    IF HETADAY<LETADAY
      *    DO MSG22 WITH 'FROM/THRU ERROR - E.T.A. DATE!','$'
      WAIT WINDOW 'FROM/THRU ERROR - E.T.A. DATE!'
      ERRFLAG = .T.
    ENDIF
    XFILTER = XFILTER+' .AND. ETA>=LETADAY .AND. ETA<=HETADAY'
  ENDIF


  *B802621,1 RAMY Add this line to add the shipment status to the filter
  *B802621,1      expr of the report [start]

  IF EMPTY(lcRpStatus)
    lcRpStatus = 'OCX'
  ENDIF

  XFILTER = XFILTER+' .AND.  Status $ (lcRpStatus)'

  *B802621 Add this line to add the shipment status to the filter [end]

  ****** On Error ******
  *--HDM
  *IF ERRFLAG
  *  LOOP
  *ENDIF
  *--HDM
  ****** Prepare Filter for Selection of Records ******

  IF LEN(XFILTER)>1
    XFILTER = XFILTER                    && HDM SUBSTR(XFILTER,7)
  ELSE
    XFILTER = "VAL(SHIPNO)<>0"
  ENDIF
  *----------------Printing Report
  *-------------------------------------------
  * SELECT SORT SEQUENCE
  *-------------------------------------------
  *    SAVE SCREEN TO STY6D0_1
  *    DO MSG22 WITH 'Sort by: <S>hpmt#  s<T>atus  <E>TA  e<N>ntered   <C>ancel','CNETS'

  XSTATUS   = XSTATUS1
  SORTFIELD = ' '
  BREAK     = ' '

  DO CASE
    * SUMMARY BY SHIPMENT#
  CASE SCHOICE = 'S'
    SORTFIELD = 'SHIPNO'
    BREAK     = ''

    * SUMMARY BY STATUS
  CASE SCHOICE = 'T'
    SORTFIELD = 'STATUS+SHIPNO'
    BREAK     = 'STATUS'

    * SUMMARY BY E.T.A. DATE
  CASE SCHOICE = 'E'
    *B803882,1 AMH Fix Sorting by E.T.A. Date [Start]
    *SORTFIELD = 'DTOC(ETA)+SHIPNO'
    SORTFIELD = 'DTOS(ETA)+SHIPNO'
    *B803882,1 AMH [End]
    BREAK     = 'ETA'

    * SUMMARY BY ENTERED DATE
  CASE SCHOICE = 'N'
    *B803882,1 AMH Fix Sorting by Entered Date [Start]
    *SORTFIELD = 'DTOC(ENTERED)+SHIPNO'
    SORTFIELD = 'DTOS(ENTERED)+SHIPNO'
    *B803882,1 AMH [End]
    BREAK     = 'ENTERED'

  ENDCASE
  *********************************************************
  ****** SELECT REPORT FILE & INITIALIZE MEMORY LOOP ******
  *********************************************************

  ****** Report File & Memory Loop Begin ******
  *-- HDM
  SET DEVICE TO PRINTER
  *--HDM
  DO WHILE .T.
    SELECT SHPMTHDR
    IF QFOXVER2                   && ADDED BY RAA ON 03/09/93
      SET ORDER TO 0
    ELSE
      SET INDEX TO
    ENDIF
    SET RELATION TO
    SET FILTER TO
    GOTO TOP
    *    DO MSG22 WITH 'Selecting records for report ...','@'
    WAIT WINDOW 'Selecting records for report ...' NOWAIT
    LOCATE ALL FOR &XFILTER

    IF EOF()
      *      DO MSG22 WITH 'NO RECORDS SELECTED FOR REPORT!','$'
      WAIT WINDOW 'NO RECORDS SELECTED FOR REPORT!'
      SET DEVICE TO SCREEN
      RETURN
    ENDIF

    WORKFILE = gfTempName()
    SET TALK ON
    COPY REST TO (gcWorkDir + WORKFILE) FOR &XFILTER
    SET TALK OFF
    *    @ 23,00 CLEAR

    SELECT 0
    USE (gcWorkDir + WORKFILE) IN 0 SHARED
    *-- HDM    DO NETUSE WITH '&QWD.&WORKFILE','','EX'

    ****** Sort to Workfile Index ******
    IF SORTFIELD<>' '
      Z = LTRIM(STR(RECCOUNT(),7))
      *      DO MSG22 WITH  'SORTING &Z RECORDS FOR THE SHIPMENT STATUS REPORT ...','@'
      WAIT WINDOW 'SORTING RECORDS FOR THE SHIPMENT STATUS REPORT ...' NOWAIT
      IF QFOXVER2                   && ADDED BY RAA ON 03/09/93
        INDEX ON &SORTFIELD TAG &WORKFILE
        SET ORDER TO TAG &WORKFILE
      ELSE
        INDEX ON &SORTFIELD TO (gcWorkDir + WORKFILE)
        SET INDEX TO (gcWorkDir + WORKFILE)
      ENDIF
      *      @ 23,00
    ENDIF

    *0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
    *STY6D0                            SHIPMENT STATUS REPORT - 123456789012345678901234567890                                 PAGE: 1234
    *MM/DD/YY
    *
    *                                                                             ORG <ÄÄÄÄÄÄ RECEIVED ÄÄÄÄÄÄ>
    *SHPMT#  STAT  ENTERED   E.T.A.    AIRWAY BILL #                     #CTS  TOTQTY   STOCK DAMAGED CANCELD
    *XXXXXX  XXXX  MM/DD/YY  MM/DD/YY  XXXXXXXXXXXXXXX                   9999  999999  999999  999999  999999
    *
    * *SUB TOTALS*  XXXXXXXXXXXXXX                                     999999 9999999 9999999 9999999 9999999
    *0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..

    *-- YMA 05/12/94 The new layout.
    *0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
    *                                                                                                      ORG <ÄÄÄÄÄÄ RECEIVED ÄÄÄÄÄ>
    *SHPMT# STAT ENTERED  E.T.A.   Vessel                         AIRWAY BILL #                   #CTS  TOTQTY   STOCK DAMAGED CANCELD
    *XXXXXX XXXX MM/DD/YY MM/DD/YY XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXX                 9999  999999  999999  999999  999999
    * *SUB TOTALS*  XXXXXXXXXXXXXX                                                              999999 9999999 9999999 9999999 9999999
    *0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..

    ****** Variables Initialization ******
    DIMENSION XTOTAL(1,5),XTOTAL1(1,5)   && ARH 10/12/93 Added 3 elements
    && for STOCK/DAMAGED/CANCELLED
    && sub totals and grand totals
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
    *    DO SEL_DEV
    CLEAR TYPEAHEAD
    SET DEVICE TO SCREEN
    *    DO MSG22 WITH 'Report printing - <SPACE BAR> to abort','@'
    SET DEVICE TO PRINTER
    SELECT &WORKFILE

    *---------------------------------------------------------
    * BEGIN [MAIN REPORT] LOOP
    *---------------------------------------------------------
    DO WHILE .T.                            && HDM.. INKEY() <>32

      IF ROW >=53
        PAGENO = PAGENO+1
        DO RPT_HDR WITH XREPORT,XTITLE,R_WIDTH

        *-- YMA 05/12/94 Updated the header, and put the new field "Vessel".
        *-- MFM 12/26/94.
        @ 05,00 SAY '                                                                                                      ORG <ÄÄÄÄÄÄ RECEIVED ÄÄÄÄÄ>'
        *@ 06,00 SAY 'SHPMT# STAT ENTERED  E.T.A.   VESSEL                         AIRWAY BILL #   LETTER OF CR #  #CTS  TOTQTY   STOCK DAMAGED CANCELD'
        @ 06,00 SAY 'SHPMT# STAT ENTERED  E.T.A.   VESSEL                         AIRWAY BILL #                   #CTS  TOTQTY   STOCK DAMAGED CANCELD'
        @ 07,00 SAY REPLICATE('=',132)   && ARH 10/12/93 Because the report
        && width changed from NARROW to be
        && WIDE
        ROW = 07
      ENDIF

      ******---- Begin Subtotals Loop ----******
      DO WHILE LEN(TRIM(BREAK)) <>0
        IF &BREAK = HBREAK
          EXIT
        ENDIF
        ROW=ROW+1

        @ ROW,00 SAY REPLICATE('-',132)   && ARH 10/12/93 Because the
        && report width changed from
        && NARROW to be WIDE
        ROW=ROW+1
        @ ROW,00 SAY '**SUB TOTAL**'

        IF BREAK=='STATUS'
          *** prints status in characters ****
          DO CASE
          CASE HBREAK $ 'O'
            @ ROW,15 SAY 'OPEN'
          CASE HBREAK $ 'C'
            @ ROW,15 SAY 'COMPLETE'
          CASE HBREAK $ 'X'
            @ ROW,15 SAY 'CANCELLED'
          ENDCASE
        ELSE
          @ ROW,15 SAY HBREAK
        ENDIF

        *-- YMA 05/12/94 Modified the coordinates.
        @ ROW,91 SAY XTOTAL(1,1)  PICTURE '999999'
        @ ROW,98 SAY XTOTAL(1,2)  PICTURE '9999999'
        ******
        ***Added by ARH on 10/12/93 to print SUB TOTALS for
        ***STOCK/DAMAGED/CANCELLED
        ******
        @ ROW,106 SAY XTOTAL(1,3)  PICTURE '9999999'
        @ ROW,114 SAY XTOTAL(1,4)  PICTURE '9999999'
        @ ROW,122 SAY XTOTAL(1,5)  PICTURE '9999999'
        ***END    ARH     10/12/93

        ROW = ROW+1
        @ ROW,00 SAY REPLICATE('-',132)        && ARH 10/12/93
        ROW = ROW+1
        XTOTAL(1,1) = 0.00
        XTOTAL(1,2) = 0.00

        ******
        ***Added by ARH on 10/12/93 to reinitialize the SUB TOTALS for
        ***STOCK/DAMAGED/CANCELLED
        ******
        XTOTAL(1,3) = 0.00
        XTOTAL(1,4) = 0.00
        XTOTAL(1,5) = 0.00
        ***END    ARH    10/12/93

        HBREAK = &BREAK
        EXIT

      ENDDO
      ******---- End Subtotals Loop ----******

      IF EOF()
        EXIT
      ENDIF

      IF ROW >=53
        ROW = 99
        LOOP
      ENDIF

      SELECT &WORKFILE

      ROW=ROW+1
      @ ROW,00 SAY SHIPNO
      *-- YMA 05/12/94 Modifies the coordinates, And display the new field.
      DO CASE
      CASE STATUS='O'
        @ ROW,07 SAY 'OPEN'
      CASE STATUS='C'
        @ ROW,07 SAY 'COMP'
      CASE STATUS='X'
        @ ROW,07 SAY 'CANC'
      ENDCASE
      @ ROW,12 SAY ENTERED
      @ ROW,21 SAY ETA
      @ ROW,30 SAY cVessel          && YMA 05/12/94 Display the new field.
      @ ROW,61 SAY AIRWAYB
      *@ ROW,77 SAY LCNO            && MFM 12/26/94.
      @ ROW,93 SAY CARTONS PICTURE '9999'
      @ ROW,99 SAY TOTQTY PICTURE '999999'

      ******
      ***Added by ARH on 10/12/93 to print the three added fields
      ***STOCK/DAMAGED/CANCELLED
      ******
      @ ROW,107 SAY RECV_STK PICTURE '999999'
      @ ROW,115 SAY RECV_DAM PICTURE '999999'
      @ ROW,123 SAY RECV_CAN PICTURE '999999'
      ***END    ARH    10/12/93

      XTOTAL(1,1)=XTOTAL(1,1)+CARTONS
      XTOTAL(1,2)=XTOTAL(1,2)+TOTQTY

      ******
      ***Added by ARH on 10/12/93 to accumulate the STOCK/DAMAGED/CANCELLED
      ***in it's SUB TOTALS vars.
      ******
      XTOTAL(1,3)=XTOTAL(1,3)+RECV_STK
      XTOTAL(1,4)=XTOTAL(1,4)+RECV_DAM
      XTOTAL(1,5)=XTOTAL(1,5)+RECV_CAN
      ***END    ARH    10/12/93

      XTOTAL1(1,1)=XTOTAL1(1,1)+CARTONS
      XTOTAL1(1,2)=XTOTAL1(1,2)+TOTQTY

      ******
      ***Added by ARH on 10/12/93 to accumulate the STOCK/DAMAGED/CANCELLED
      ***in it's GRAND TOTALS vars.
      ******
      XTOTAL1(1,3)=XTOTAL1(1,3)+RECV_STK
      XTOTAL1(1,4)=XTOTAL1(1,4)+RECV_DAM
      XTOTAL1(1,5)=XTOTAL1(1,5)+RECV_CAN
      ***END   ARH   10/12/93

      SELECT &WORKFILE
      SKIP

    ENDDO
    *------------------ END [MAIN REPORT] LOOP --------------------

    X = 2
    IF LEN(TRIM(BREAK)) = 0
      X = 1
    ENDIF
    ROW=ROW+2
    @ ROW,00 SAY REPLICATE('=',132)    && ARH 10/12/93
    ROW = ROW+1
    @ ROW,00 SAY '* GRAND TOTAL *'

    *-- YMA 05/12/94 Modified the coordinates.
    @ ROW,91 SAY XTOTAL1(1,1)  PICTURE '999999'
    @ ROW,98 SAY XTOTAL1(1,2)  PICTURE '9999999'

    ******
    ***Added by ARH on 10/12/93 to print the GRAND TOTALS for
    ***STOCK/DAMAGED/CANCELLED quantities
    ******
    @ ROW,106 SAY XTOTAL1(1,3)  PICTURE '9999999'
    @ ROW,114 SAY XTOTAL1(1,4)  PICTURE '9999999'
    @ ROW,122 SAY XTOTAL1(1,5)  PICTURE '9999999'
    ***END    ARH     10/12/93

    ROW = ROW+1
    @ ROW,00 SAY REPLICATE('=',132)    && ARH 10/12/93
    EXIT
  ENDDO

  *-- HDM
  *DO ENDPRT         && END THE REPORT OR DISPLAY ON SCREEN

  DO ENDREPORT

  *-- HDM
  SET DEVICE TO SCREEN
  *RETURN

  *------Report body Finished-------------------------
*E301404,1  RAMY
ELSE
  llIsAparel = .F.
  *--- Initializing Variables
  lnRecNo   = 0
  lcOldVal  = ''
  lcDivName = ''
  lnMajPic  = LEN(gfItemMask("PM"))
  lcSeaName = ''
  *--Initialize variables on which the OG criteria will be printed.
  STORE .F. TO llSeason,llDivision,llShipMent,llStatus,llEntered,llEta,llStyle,llFabric,llGroupCh
  STORE '' TO lcSeason,lcStatus,lcDivision,lcShipMent,lcEntered,lcEta,lcStyle,lcFabric
  lcSortFld = ' '
  DO CASE
    *-- SUMMARY BY SHIPMENT#
  CASE lcRPSortBy = 'S'
    lcSortFld = 'SHIPNO'
    *-- SUMMARY BY Style
  CASE lcRPSortBy  = 'T'
    lcSortFld  = 'SUBSTR(STYLE,1,lnMajPic)'
    *--SUMMARY BY E.T.A. DATE
  CASE lcRPSortBy = 'E'
    lcSortFld = 'DTOS(ETA)+SHIPNO'
    *-- SUMMARY BY ENTERED DATE
  CASE lcRPSortBy = 'N'
    lcSortFld = 'DTOS(ENTERED)+SHIPNO'
  ENDCASE
  lcSortFld = 'SEASON+DIVISION+'+lcSortFld
  ****** Report File & Memory Loop Begin ******
  SELECT SHPMTHDR

  SET RELATION TO SHIPNO INTO POSLN ADDITIVE
  SELECT POSLN
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SELECT SHPMTHDR
  GOTO TOP
  WAIT WINDOW  'Selecting records for report ...' NOWAIT
  LOCATE ALL FOR &lcRpExp
  IF EOF()
    *--- Text  'NO RECORDS SELECTED FOR REPORT!'
    = gfModalGen('TRM00052B00000','DIALOG' )
    RETURN
  ENDIF

  WORKFILE = gfTempName()
  SET TALK ON
  COPY REST TO &gcWorkDir.&WORKFILE FOR &lcRpExp
  SET TALK OFF
  =gfOpenFile('&gcWorkDir.&WORKFILE','','EX')
  lcAllTmp = gfTempName()

  *--Add this function to collect data.
  SELECT SHPMTHDR
  SET RELATION TO
  SELECT POSLN
  SET RELATION TO SHIPNO INTO SHPMTHDR ADDITIVE
  =lfCollect()
  *---Erase Temp. Files
  USE IN &WORKFILE
  ERASE &gcWorkDir.&WORKFILE+'.DBF'

  SET RELATION TO "P"+Po INTO POSHDR
  *--Activate the relation.
  GO TOP

  IF EOF()
    *---Text 'NO RECORDS SELECTED FOR REPORT!'
    = gfModalGen('TRM00052B00000','DIALOG' )
    USE IN &lcAllTmp
    ERASE &gcWorkDir.&lcAllTmp+'.DBF'
    ERASE &gcWorkDir.&lcAllTmp+'.CDX'
    
    *B606565,1 KHM 10/24/2002 (Begin) Commented out.
    *USE IN SHPDATES
    *B606565,1 KHM 10/24/2002 (End)
    
    RETURN
  ENDIF
  *--- Sort to lcAllTmp Index
  Z = LTRIM(STR(RECCOUNT(),7))
  WAIT WINDOW  'SORTING &Z RECORDS FOR THE CUSTOM SHIPMENT STATUS REPORT ...' NOWAIT
  INDEX ON &lcSortFld TAG &lcAllTmp
  SET ORDER TO TAG &lcAllTmp

  *--Set a relation to SHPDATES file to get the customs,trucker and warehouse dates in the FRX.
  SET RELATION TO ShipNo INTO SHPMTHDR ADDITIVE
  *--Print Landscape
  *IF gcDevice='PRINTER'
  *  @ PROW(),PCOL() SAY [&l1O&l14]
  *ENDIF

  SELECT (lcAllTmp)
  *--Count records to be used in FRX
  COUNT TO lnRecNo
  GO TOP
  =lfCriteria()
  SET DEVICE TO SCREEN
  DO gfDispRe WITH EVAL('lcRpForm')
  *--Release Landscape
  
  *B604535,1 KHM 06/11/2001 (Begin) Commenting the following code as it cause
  *B604535,1                the printer to be locked.
  *SET DEVICE TO PRINTER
  *IF gcDevice='PRINTER'
  *  @ PROW(),PCOL() SAY [&l0O]
  *ENDIF
  *B604535,1 KHM 06/11/2001 (End)
  SET DEVICE TO SCREEN
  USE IN &lcAllTmp
  ERASE &gcWorkDir.&lcAllTmp+'.DBF'
  ERASE &gcWorkDir.&lcAllTmp+'.CDX'

ENDIF
*E301404,1 RAMY [END]

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

*E301404,1 RAMY
lcOldVal = EVALUATE(SYS(18))

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
DECLARE laRpSource[3],laRpTarget[1]

lnStatus = lcRpStatus
*B802621,1 RAMY Changed the status values to show COMPLETE instead of HOLD
*B802621,1      and empty the selected status by default instead of
*B802621,1      defaulting the selected status to OPEN (Start)

*STORE 'Open'     TO laRpSource[1] ,laRpTarget[1]
*STORE 'Hold'     TO laRpSource[2] &&,laRpTarget[2]
*STORE 'Canceled' TO laRpSource[3]

STORE 'Open'      TO laRpSource[1]
STORE 'Completed' TO laRpSource[2]
STORE 'Canceled'  TO laRpSource[3]

*B802621,1 RAMY (End)

lcRpStatus = ''


*E301404,1 RAMY [START]
*-- Initilize the date range with system date.

*B605305,1 AMH Remove the default date in the date entered and eta options [Start]
*lnDatePos = ASCAN(laOGFxFlt,"SHPMTHDR.ENTERED")
*IF lnDatePos > 0
*  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
*  IF EMPTY(laOGFxFlt[lnDatePos,6])
*    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
*  ENDIF  
*ENDIF

*lnDatePos = ASCAN(laOGFxFlt,"SHPMTHDR.ETA")
*IF lnDatePos > 0
*  lnDatePos = ASUBSCRIPT(laOGFxFlt,lnDatePos,1)
*  IF EMPTY(laOGFxFlt[lnDatePos,6])
*    laOGFxFlt[lnDatePos,6] = DTOC(gdSysDate) + '|' + DTOC(gdSysDate)
*  ENDIF  
*ENDIF
*B605305,1 AMH [End]

R_WIDTH    = 'XW'
lcFormName = 'AND700A'
lcOldVal = SPACE(01)
IF !USED('SYCCOMP')
  =gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'Ccomp_id','SH')
ENDIF
=SEEK(gcAct_Comp)
*--Now we are on the proper record and will print from file directly.
*E301404,1 RAMY end
*-----------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvOStatus
*! Developer : Hossam El Etreby   (HDM)
*! Date      : 04/08/1998
*! Purpose   : Valid function for STSTUS Button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************

FUNCTION lfvOStatus
PRIVATE lcOldStat,lcCurrChr

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Order Status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
*B802621,1 RAMY Added the IF condition in case of we don`t select any
*B802621,1 RAMY entry from the STATUS mover (Start)

*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTarget[1])
  *B802621,1 RAMY (End)

  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Open','O',;
      IIF(laRpTarget[lnI] = 'Canceled','X',;
      IIF(laRpTarget[lnI] = 'Complete','C','')))

  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '

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

*------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvShpNo
*! Developer : Hossam El Etreby   (HDM)
*! Date      : 04/08/1998
*! Purpose   : Valid function for Shipment #
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
FUNCTION lfvShpNo
PRIVATE lcVar , lcObj , laTemp,lcBrowFields

lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))

*B603340,1 (WMA)
*lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
*B603340,1 [End]

lcPrevAl = SELECT()
STORE '' TO lcBrowCond

DECLARE laTemp[1]
SELECT SHPMTHDR
SET ORDER TO Shpmthdr
DIMENSION laTemp[1]


IF !EMPTY(lcObj) AND !(SEEK(lcObj , 'SHPMTHDR'))

  *B802621,1 RAMY Changed the status field to show the completed shipments
  *B802621,1      instead of showing it as cancelled shipments (Start)
  *lcBrFields = "SHIPNO    :R :H='Shipment #',"+;
  "lcSt = IIF(STATUS='O','Open',IIF(STATUS='H','Hold','Cancelled')) :R :H='Status',"+;
  "ENTERED   :R :H='Entered'      ,"+;
  "ETA       :R :H='E.T.A.'       ,"+;
  "TOTQTY    :R :H='TOTQTY'       ,"+;
  "RECV_STK  :R :H='Received'     ,"+;
  "RECV_DAM  :R :H='Damaged'      ,"+;
  "CVESSEL   :R :H='VESSEL'      ,"+;
  "RECV_CAN  :R :H='Cancelled' "

lcBrFields = "SHIPNO    :R :H='Shipment #',"+;
  "lcSt = IIF(STATUS='O','Open',IIF(STATUS='X','Cancelled','Completed')) :R :H='Status',"+;
  "ENTERED   :R :H='Entered'      ,"+;
  "ETA       :R :H='E.T.A.'       ,"+;
  "TOTQTY    :R :H='TOTQTY'       ,"+;
  "RECV_STK  :R :H='Received'     ,"+;
  "RECV_DAM  :R :H='Damaged'      ,"+;
  "CVESSEL   :R :H='VESSEL'      ,"+;
  "RECV_CAN  :R :H='Cancelled' "
*B802621,1 RAMY (End)

lNThing    = gfBrows(lcBrowCond,'SHIPNO','laTemp')
*B603340,1 (WMA) Added Statement to Execute When Entering Shipment # is True
ELSE
laTemp=lcObj
*B603340,1 [END]
ENDIF


IF !EMPTY(laTemp[1])
  lcObj = laTemp[1]
ELSE
  *B603340,1 (WMA)
  *lcObj = laOldVal
  lcObj = ''
  *B603340,1 [End]
ENDIF


&lcVar = lcObj      && Update the field

SELECT (lcPrevAl)



*!*************************************************************
*! Name      : lfClrRead
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Function called to rebuld the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       :
*!*************************************************************
FUNCTION lfClrRead

CLEAR READ

*!*************************************************************
*! Name      : lfvFabric
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Validation function for validating Fabric Code
*!*************************************************************
*! Called from : Only this color [Option Grid]
*!*************************************************************
*! Calls       : FaBrow()
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

****************************************************************************
* FUNC: lfvStyle
* DESC: To valid the style.
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
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
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
****************************************************************************
FUNCTION lfvChoose

IF lcRpChos = 'A'
  lcRpForm = 'POSHPST'
  =lfRepPltFr(lcRpForm)
  llFrxForm   = .F.
  llOGRefForm = .F.
  IF !EMPTY(LCOGTMPFORM)
    ERASE (gcWorkDir+LCOGTMPFORM+'.FRX')
    ERASE (gcWorkDir+LCOGTMPFORM+'.FRT')
  ENDIF
  LCOGTMPFORM = ""
  IF EMPTY(lcOGFormV)
    lcOGFormV = "lcRpForm"
  ENDIF
ELSE
  lcRpForm = 'POSHPSTB'
  =lfRepPltFr(lcRpForm)
  llFrxForm   = .T.
  llOGRefForm = .T.
  IF EMPTY(LCOGTMPFORM)
    LCOGTMPFORM = gfTempName()
  ENDIF
  IF EMPTY(lcOGFormV)
    lcOGFormV = "lcRpForm"
  ENDIF
ENDIF


*!*************************************************************
*! Name      : lfShpVia
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Print POSHDR division
*!*************************************************************
*! Called from : REPORT FRX
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfShpVia()
*!*************************************************************
FUNCTION lfShpVia

*B608448,1 AKA 21/02/2008 Fix the bug of Incorrect Fields on Shipment Report [Start]
*IF SEEK('N' + 'SHIPVIA   ' + SHPMTHDR.SHIPVIA , 'CODES')
*  RETURN (SUBSTR(ALLTRIM(CODES.cDiscRep),1,3))
*ELSE
*  RETURN (SPACE(03))
*ENDIF

RETURN ALLTRIM(gfCodDes(SHPMTHDR.ShipVia,'SHIPVIA'))

*B608448,1 AKA 21/02/2008 Fix the bug of Incorrect Fields on Shipment Report [End]





*!*************************************************************
*! Name      : lfCollect
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Collect data.
*!*************************************************************
FUNCTION lfCollect

SELECT POSLN
=AFIELDS(laFldsStru)
lnNoOfArr = ALEN(laFldsStru,1)
DIMENSION laFldsStru[lnNoOfArr+6,4]
*-- cVessel field
laFldsStru[lnNoOfArr+1,1] = 'cVessel'
laFldsStru[lnNoOfArr+1,2] = 'C'
laFldsStru[lnNoOfArr+1,3] = 30
laFldsStru[lnNoOfArr+1,4] = 0
*-- Entered field
laFldsStru[lnNoOfArr+2,1] = 'Entered'
laFldsStru[lnNoOfArr+2,2] = 'D'
laFldsStru[lnNoOfArr+2,3] = 8
laFldsStru[lnNoOfArr+2,4] = 0
*-- ETA field
laFldsStru[lnNoOfArr+3,1] = 'ETA'
laFldsStru[lnNoOfArr+3,2] = 'D'
laFldsStru[lnNoOfArr+3,3] = 8
laFldsStru[lnNoOfArr+3,4] = 0
*-- Season field
laFldsStru[lnNoOfArr+4,1] = 'Season'
laFldsStru[lnNoOfArr+4,2] = 'C'
laFldsStru[lnNoOfArr+4,3] = 6
laFldsStru[lnNoOfArr+4,4] = 0
*-- Division field
laFldsStru[lnNoOfArr+5,1] = 'Division'
laFldsStru[lnNoOfArr+5,2] = 'C'
laFldsStru[lnNoOfArr+5,3] = 6
laFldsStru[lnNoOfArr+5,4] = 0
*--Season+Division change field
laFldsStru[lnNoOfArr+6,1] = 'llGrpChng'
laFldsStru[lnNoOfArr+6,2] = 'l'
laFldsStru[lnNoOfArr+6,3] = 1
laFldsStru[lnNoOfArr+6,4] = 0

*--Create table .
CREATE DBF (gcWorkDir+lcAllTmp) FROM ARRAY laFldsStru
*--Get line for filtered WORKFILE .
SELECT (WORKFILE)
lcGrpDiv = &lcAllTmp..Season+&lcAllTmp..Division
SCAN
  lcShipNo = ShipNo
  IF SEEK(lcShipNo,'POSLN')
    SELECT POSLN
    SCAN WHILE shipno+cstytype+po+style+STR(lineno,6)+trancd  = lcShipNo;
         FOR   INLIST(TranCd,'2','3') AND &lcRpExp
      SCATTER MEMVAR MEMO
      m.cVessel = &WORKFILE..cVessel
      m.Entered = &WORKFILE..Entered
      m.Eta     = &WORKFILE..Eta
      =SEEK("P"+Po,'POSHDR')
      m.Division = POSHDR.cDivision
      =SEEK(Style,'STYLE')
      m.Season = STYLE.Season
      INSERT INTO (lcAllTmp) FROM MEMVAR
      IF &lcAllTmp..Season+&lcAllTmp..Division <>lcGrpDiv
        REPLACE &lcAllTmp..llGrpChng with .T.
        lcGrpDiv = &lcAllTmp..Season+&lcAllTmp..Division    
      ENDIF
    ENDSCAN
  ENDIF  
ENDSCAN
SELECT (lcAllTmp)

*!***************************************************************************
*! Name      : lfCriteria
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Prepare selected criteria to be printed on the first page of FRX.
*!*****************************************************************************
FUNCTION lfCriteria

lcSeaVal   = lfRtFltVal('STYLE.SEASON')
lcSeason   = 'Season         [' +lcSeaVal+' ]'
lcDivVal   = lfRtFltVal('STYLE.CDIVISION')
lcDivision = 'Division        [' +lcDivVal+' ]'
lcShipVal  = lfRtFltVal('SHPMTHDR.SHIPNO')
llShipMent = !EMPTY(lcShipMent)
lcShipMent = 'Shipment#    [ From : ' +lcShipVal+' ]'
lcStatVal  = lfRtFltVal('SHPMTHDR.STATUS')
lcStatus   = 'Status          [ ' +lcStatVal+' ]'
lcStatus   = STRTRAN(lcstatus,'O','Open')
lcStatus   = STRTRAN(lcstatus,'C','Complete')
lcStatus   = STRTRAN(lcstatus,'X','Canceled')
lcEntered  = 'Ship Date     [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'SHPMTHDR.ENTERED'),1),6]),'|',' TO:   ')+' ]'
lcEtA      = 'E.T.A Date   [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFXFlt,'SHPMTHDR.ETA'),1),6]),'|',' TO:   ')+' ]'
lcStyle    = 'Style            [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,'STYLE.CSTYMAJOR'),1),6]),'|',' TO:   ')+' ]'
lcFabric   = 'Fabric          [ From : ' +STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,'STYLE.FABRIC'),1),6]),'|',' TO:   ')+' ]'
llSeason   = !EMPTY(lcSeaVal) 
llDivision = !EMPTY(lcDivVal)
llStatus   = !EMPTY(lcStatVal) 
llEntered  = ATC('ENTER',lcRpExp) >0
llEta      = ATC('ETA',lcRpExp) >0
llStyle    = ATC('CSTYMAJOR',lcRpExp) >0
llFabric   = ATC('STYLE.FABRIC',lcRpExp) >0
RETURN ''


*!***************************************************************************
*! Name      : lfRtFltVal
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Return filter options value.
*!*****************************************************************************
FUNCTION lfRtFltVal
PARAMETERS lcStrVal
PRIVATE lnPos

lnPos = ASCAN(laOgFXFlt,lcStrVal)
IF lnPos >0 
  RETURN STRTRAN(ALLTRIM(laOgFxFlt[ASUBSCRIPT(laOgFxFlt , lnPos,1),6]),'|',', ')
ELSE
  RETURN ''
ENDIF    


FUNCTION lfGroupCh

SKIP
llGroupCh = llGrpChng OR EOF()
SKIP-1
RETURN ''


*!*************************************************************
*! Name      : lfGetNames
*! Developer : RAMY MABROUK [RAMY]
*! Date      : 05/21/2000
*! Refer to  : E301404,1
*! Purpose   : Get seasons and divisions' names.
*!*************************************************************
*! Called from : FRX.
*!*************************************************************
FUNCTION lfGetNames

lcDivName = gfCodDes(Division,'CDIVISION ')
lcSeaName = gfCodDes(Season,'SEASON    ')
RETURN ''
