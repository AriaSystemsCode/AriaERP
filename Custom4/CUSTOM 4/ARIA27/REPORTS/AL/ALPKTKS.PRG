*:***************************************************************************
*: Program file  : ALPKTKS.PRG
*: Program desc. : CUSTOMIZED SALES ORDER ALLOCATION PICKING TICKET Form FOR BRO10
*: Date          : 08/20/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)
*: Tracking Job Number: B606405 [This form [S] from the FXP maild to Egypt from USA]
*:
*:***************************************************************************
*: Calls : 
*:    Procedures : lpPrtSku,lpNewFoter,lpOldFoter,lpHrdLabls,lpSidLabls,lpFilEmpLn
*:    Functions  : lfContuLoc(),lfGetLoc(),lfPrnStrn()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*: Example : DO ALPKTKS
*:***************************************************************************
*: Modification:
*****************************************************************************
*: B606405,1 BWA 08/20/2002 Fix the bug of Print the complation date for the 1st P/T
*: B605945,1 BWA 10/14/2002 Fix the bug of printing the right SKU.
*****************************************************************************
           *--Section of Initializing the variables --*
STORE SPACE(1) TO LCSTRTOPRN, LCSKUSIZE, LCSCLSTRN, LCSTR, HLINE1
LLNOREC    = .F.                && Variable used to make the endreport function in the main program.
LNLENGTH   = 7
LNLEN      = 0

*--variables hold the addressess of the company.
HLINE2 = TRIM(LACOMPADD(1))
HLINE3 = TRIM(LACOMPADD(2))
HLINE4 = TRIM(LACOMPADD(3))
IF LEN(TRIM(HLINE3))=0
   HLINE3 = HLINE4
   HLINE4 = SPACE(1)
ENDIF

DIMENSION LADIVLNAME[ 1, 2]
LADIVLNAME[ 1, 1] = 'DIVLNAME'
LADIVLNAME[ 1, 2] = 'HLINE1'

lcCustPoVl = ''
ROW        = 0
LLSCALE    = .T.
LLENDPT    = .F.
NEWDOC     = .T.
LNNOTLINE  = 1
LLRET      = .F.

DIMENSION LASCALESZ[5]
STORE SPACE(0) TO LASCALESZ

*B605945,1 BWA 10/14/2002 Initial variable hold the length og the sku in spck_hdr file.[START]
*--Variable hold the length of the sku in the spck_hdr.
STORE 0 TO lnLenDm1
*B605945,1 BWA 10/14/2002.[END]

*--If EOF it will return to the main program
IF EOF(lcTmpOrdL)
  llNoRec = .T.
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

*--open this files again with another index
SELECT SPCK_LIN
SET ORDER TO SPCKLINS INTO SPCK_LIN ADDITIVE

SELECT WHSLOC
SET ORDER TO WHSLOCST INTO WHSLOC ADDITIVE

SELECT (lcTmpOrdL)
*-- MAIN LOOP
DO WHILE !EOF(lcTmpOrdL)
  WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT

  SELECT (lcTmpOrdL)
  LNCURREC = RECNO()
  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
    SELECT (lcTmpOrdL)
    XPIKTKT = PIKTKT
    XORDER  = ORDER
    XDATE   = PIKTKT.DATE
    XSTORE  = PIKTKT.Store

    *B606405,1 BWA 08/20/2002 Fix the bug of Print the complation date for the 1st P/T.[START]
    SELECT ORDHDR
    =SEEK('O'+XORDER,'ORDHDR')
    *B606405,1 BWA 08/20/2002.[END]

    XDept   = OrdHdr.Dept
    XREP1   = ORDHDR.REP1
    XREP2   = ORDHDR.REP2
    XENTERD = ORDHDR.ENTERED
    XSTART  = ORDHDR.START
    XCOMPLT = ORDHDR.COMPLETE

    *B606405,1 BWA 08/20/2002 Comment this 2 lines due to get the right record befor the variables.[START]
    *SELECT ORDHDR
    *=SEEK('O'+XORDER,'ORDHDR')
    *B606405,1 BWA 08/20/2002.[END]

    XMULTI   = MULTI
    XACCOUNT = ACCOUNT
    STORE 0.00 TO XORDTOT, XVALUE , SVALUE
    lcCustPoVl = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)

    SELECT CUSTOMER
    XBTNAME  = CUSTOMER.BTNAME
    XBTADDR1 = gfGetAdr('CUSTOMER','','','',1,'2')
    XBTADDR2 = gfGetAdr('CUSTOMER','','','',2,'2')
    XBTADDR3 = gfGetAdr('CUSTOMER','','','',3,'2')

    IF LEN(TRIM(XBTADDR2)) = 0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF

    XTERM_DATA = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
    XSHIP_DATA = gfCodDes(IIF(ALLTRIM(ORDHDR.SHIPVIA)='*',CUSTOMER.SHIPVIA,ORDHDR.SHIPVIA),'SHIPVIA')
    XSPCI_DATA = gfCodDes(ORDHDR.SPCINST,'SPCINST')
    XSEAS_DATA = gfCodDes(ORDHDR.SEASON,'SEASON')
    XDIVI_DATA = gfCodDes(ORDHDR.CDIVISION,'CDIVISION')
    =gfRltFld(ORDHDR.CDIVISION,@LADIVLNAME,'CDIVISION')

    HLINE1 = IIF(EMPTY(HLINE1),LCCOMPNAME,HLINE1)
    STORE '' TO XSTNAME, XSTADDR1, XSTADDR2, XSTADDR3, XSTADDR4
    XPIECES = 0
  ENDIF

  *--Initialize the alt address if ther is any.
  SELECT ORDHDR
  SEEK CORDTYPE + XORDER

  IF ALT_SHPTO
    lcShpTName   = ORDHDR.STNAME
    laShipTo[ 1] = OrdHdr.cAddress1
    laShipTo[ 2] = OrdHdr.cAddress2
    laShipTo[ 3] = OrdHdr.cAddress3
    laShipTo[ 4] = OrdHdr.cAddress4
    laShipTo[ 5] = OrdHdr.cAddress5

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo(1)
    XSTADDR2 = laShipTo(2)
    XSTADDR3 = TRIM(laShipTo(3))+' '+TRIM(laShipTo(4))+' '+laShipTo(5)

    IF LEN(TRIM(XSTADDR2)) = 0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
   ELSE
    SELECT Customer
    IF !EMPTY(CUSTOMER.DIST_CTR)
      =SEEK("S" + CUSTOMER.Account + CUSTOMER.DIST_CTR )
    ENDIF

    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    XSTADDR1 = gfGetAdr('CUSTOMER','','','',1)
    XSTADDR2 = gfGetAdr('CUSTOMER','','','',2)
    XSTADDR3 = gfGetAdr('CUSTOMER','','','',3)

    IF LEN(TRIM(XSTADDR2)) = 0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF

  SELECT ORDHDR
  *-- START PRINTING
  DO lpHrdLabls

  IF LLRET
    LOOP
  ENDIF

  *-- LINE LOOP
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0

  SCAN WHILE PIKTKT = XPIKTKT
    DO CASE
      CASE EOF()
        NEWDOC = .T.
        EXIT
      CASE TOTPIK<=0
        LOOP
      CASE ROW>=55
        NEWDOC = .F.
        EXIT
    ENDCASE

    SELECT STYLE
    IF SEEK(&lcTmpOrdL..Style)
      XSTYDESC   = DESC
      LCSTYLOCAT = LOCATION
      LCSCALE    = SCALE
    ELSE
      XSTYDESC   = ''
      LCSTYLOCAT = ''
      LCSCALE    = ''
    ENDIF

    SELECT (lcTmpOrdL)
    @ ROW, 1 SAY SUBSTR(STYLE,1,12)
    @ ROW, 13 SAY SUBSTR(STYLE,14,19)
    @ ROW, 19 SAY LEFT(LCSCALE,2)
    @ ROW, 21 SAY PIK1 PICTURE '@Z 9999'
    @ ROW, 26 SAY PIK2 PICTURE '@Z 9999'
    *@ ROW, 30 SAY PIK3 PICTURE '@Z 9999'
    @ ROW, 31 SAY PIK3 PICTURE '@Z 9999'
    *@ ROW, 35 SAY PIK4 PICTURE '@Z 9999'
    @ ROW, 36 SAY PIK4 PICTURE '@Z 9999'
    *@ ROW, 39 SAY PIK5 PICTURE '@Z 9999'
    @ ROW, 41 SAY PIK5 PICTURE '@Z 9999'
    *@ ROW, 44 SAY PIK6 PICTURE '@Z 9999'
    @ ROW, 46 SAY PIK6 PICTURE '@Z 9999'
    *@ ROW, 48 SAY PIK7 PICTURE '@Z 9999'
    @ ROW, 51 SAY PIK7 PICTURE '@Z 9999'
    *@ ROW, 53 SAY PIK8 PICTURE '@Z 9999'
    @ ROW, 56 SAY PIK8 PICTURE '@Z 9999'
    *@ ROW, 57 SAY TOTPIK PICTURE '99999'
    @ ROW, 61 SAY TOTPIK PICTURE '@Z 9999'

    IF LLRPSTYPRC
      @ ROW, 66 SAY PRICE PICTURE '9999.99'
    ENDIF
    @ ROW, 74 SAY TOTPIK PICTURE '99999'
    DO LPSIDLABLS
    ROW = ROW + 1
    = LFCHKROW('B')
    IF !llRpSkuBck
      @ ROW, 1 SAY XSTYDESC
      IF LLRPSTYLOC
        = LFGETLOC()
        IF !EMPTY(LCSTR)
          @ ROW, 60 SAY 'BINS    :'+SUBSTR(LCSTR,1,10)
          DO LPSIDLABLS
          ROW = ROW + 1
          = LFCHKROW('B')
          IF LEN(LCSTR)>18
            = LFCONTULOC()
          ENDIF
        ELSE
          DO LPSIDLABLS
          ROW = ROW + 1
          = LFCHKROW('B')
        ENDIF
      ENDIF
    ELSE
      IF SEEK('P'+PIKTKT.Account+&lcTmpOrdL..Style,'SPCK_LIN')
        @ ROW, 1 SAY 'PACK ID#:'+SPCK_LIN.PACK_ID
        @ ROW, 25 SAY XSTYDESC
        DO LPSIDLABLS
        ROW = ROW + 1
        = LFCHKROW('B')
      ELSE
        DO LPPRTSKU
        IF !llRpSkuSiz
          @ ROW, 0 SAY SUBSTR(LCSTRTOPRN,1,IIF(LEN(LCSTRTOPRN)>=40,40,LEN(LCSTRTOPRN)))
          @ ROW, IIF(LEN(LCSTRTOPRN)>=40,41,LEN(LCSTRTOPRN)+2) SAY XSTYDESC
          DO LPSIDLABLS
          ROW = ROW + 1
          = LFCHKROW('B')
          IF LLRPSTYLOC
            = LFGETLOC()
            IF !EMPTY(LCSTR)
              @ ROW, 0 SAY 'BINS    :'+SUBSTR(LCSTR,1,85)
              DO LPSIDLABLS
              ROW = ROW + 1
              = LFCHKROW('B')
              IF LEN(LCSTR)>85
                = LFCONTULOC()
              ENDIF
            ELSE
              DO LPSIDLABLS
              ROW = ROW + 1
              = LFCHKROW('B')
            ENDIF
          ENDIF
          DO LPSIDLABLS
          ROW = ROW + 1
          = LFCHKROW('B')
        ELSE
          @ ROW, 0 SAY XSTYDESC
          IF LLRPSTYLOC
            = LFGETLOC()
            IF !EMPTY(LCSTR)
              @ ROW, 60 SAY 'BINS    :'+SUBSTR(LCSTR,1,10)
              DO LPSIDLABLS
              ROW = ROW + 1
              = LFCHKROW('B')
              IF LEN(LCSTR)>18
                = LFCONTULOC()
              ENDIF
            ELSE
              DO LPSIDLABLS
              ROW = ROW + 1
              = LFCHKROW('B')
            ENDIF
          ELSE
            DO LPSIDLABLS
            ROW = ROW + 1
            = LFCHKROW('B')
          ENDIF
          = LFPRNSTRN()
          LCSKUSIZE = LCSTRTOPRN + ' ' + LCSKUSIZE
          @ ROW, 0 SAY SUBSTR(LCSKUSIZE,1,86)
          IF !EMPTY(LCSKUSIZE)
            DO LPSIDLABLS
            ROW = ROW + 1
            = LFCHKROW('B')
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    DO LPSIDLABLS
    ROW = ROW + 1
    = LFCHKROW('B')
    SELECT (lcTmpOrdL)
    XTOTQTY = XTOTQTY + TOTQTY
    XVALUE  = XVALUE  + TOTPIK*PRICE
    XPIECES = XPIECES + TOTPIK
    SELECT (lcTmpOrdL)
  ENDSCAN
  *--End print line loop.

  *-- To print the notepad.
  IF NEWDOC
    SELECT (lcTmpOrdL)
    IF LLRPORDNOT
      SELECT NOTEPAD
      IF SEEK('B' + XORDER)
        LNOLDMEMW = SET('MEMOWIDTH')
        SET MEMOWIDTH TO 75
        LNMEMLINS = MEMLINES(NOTEPAD.MNOTES)
        @ ROW, 0 SAY '* -- N O T E S -- *'
        DO LPSIDLABLS
        ROW = ROW + 1
        = LFCHKROW('B')
        DO WHILE LNNOTLINE<=LNMEMLINS
          IF ROW>=55
            DO LPOLDFOTER
            DO LPHRDLABLS
          ENDIF
          @ ROW, 0 SAY MLINE(MNOTES,LNNOTLINE)
          DO LPSIDLABLS
          ROW = ROW + 1
          LNNOTLINE = LNNOTLINE + 1
        ENDDO
        SET MEMOWIDTH TO LNOLDMEMW
      ENDIF
    ENDIF
    LNNOTLINE = 1
    LLENDPT = .T.
    DO LPFILEMPLN
    DO LPNEWFOTER
  ELSE
    LLENDPT = .F.
    DO LPFILEMPLN
    DO LPOLDFOTER
    LOOP
  ENDIF

  LLSCALE = .T.      && Re-initialize llScale to get the size scales for a new P/T
  SELECT (lcTmpOrdL)
ENDDO
WAIT CLEAR
SET DEVICE TO SCREEN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpFilEmpLn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : Fill the empty pick ticket lines.
*!*************************************************************
*! Called from : ALPKTKS.PRG
*!*************************************************************
*! Calls       : lpSidLabls
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpFilEmpLn()
*!*************************************************************
PROCEDURE lpFilEmpLn

DO WHILE ROW <= 53
  DO LPSIDLABLS
  IF ROW < 53
    ROW = ROW + 1
  ELSE
    EXIT
  ENDIF
ENDDO

*--End of lpFilEmpLn.
*!*************************************************************
*! Name      : lpSidLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : Print the Side labels.
*!*************************************************************
*! Called from : ALPKTKS.PRG
*!*************************************************************
*! Calls       : lpSidLabls
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpSidLabls()
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 34 .OR. ROW = 50
    @ ROW, 93 SAY IIF(llPrntComp , HLINE1 , '')
  CASE ROW = 35 .OR. ROW = 51
    @ ROW, 93 SAY IIF(llPrntComp , HLINE2 , '')
  CASE ROW = 36 .OR. ROW = 52
    @ ROW, 93 SAY IIF(llPrntComp , HLINE3 , '')
  CASE ROW = 37 .OR. ROW = 53
    IF LLENDPT .AND. ROW = 53 .AND. NEWDOC
      @ 53, 0 SAY LCRPMSG1
    ENDIF
    @ ROW, 93 SAY IIF(llPrntComp , HLINE4 , '')
  CASE ROW = 23 .OR. ROW = 40
    @ ROW, 93 SAY XSTNAME
  CASE ROW = 24 .OR. ROW = 41
    @ ROW, 93 SAY XSTADDR1
  CASE ROW = 25 .OR. ROW = 42
    @ ROW, 93 SAY XSTADDR2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW, 93 SAY XSTADDR3
  CASE ROW = 30 .OR. ROW = 46
    @ ROW, 86 SAY LEFT(lcCustPoVl,13)
    @ ROW, 100 SAY XSTORE
    @ ROW, 111 SAY XDept
    @ ROW, 120 SAY XORDER
  CASE ROW = 31 .OR. ROW = 48
    @ ROW, 101 SAY XPIKTKT
    @ ROW, 108 SAY 'SHIP VIA: '+SUBSTR(XSHIP_DATA,1,10)
ENDCASE

*--End of lpSidLabls.
*!*************************************************************
*! Name      : lpHrdLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : .PRG
*!*************************************************************
*! Calls       : lpSidLabls
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpHrdLabls()
*!*************************************************************
PROCEDURE lpHrdLabls

@ 2, 1    SAY IIF(llPrntComp , HLINE1 , '')
@ 2, 95   SAY IIF(llPrntComp , HLINE1 , '')
@ 3, 1    SAY IIF(llPrntComp , HLINE2 , '')
@ 3, 95   SAY IIF(llPrntComp , HLINE2 , '')
@ 4, 1    SAY IIF(llPrntComp , HLINE3 , '')
@ 4, 95   SAY IIF(llPrntComp , HLINE3 , '')
@ 5, 1    SAY IIF(llPrntComp , HLINE4 , '')
@ 5, 61   SAY LEFT(XDIVI_DATA,7)
@ 5, 74   SAY XPIKTKT
@ 5, 95   SAY IIF(llPrntComp , HLINE4 , '')
@ 7, 95   SAY XSTNAME
*@ 8, 61   SAY ORDHDR.APPROVAL
@ 8, 60   SAY ORDHDR.APPROVAL
@ 8, 74   SAY XDATE
@ 8, 95   SAY XSTADDR1
@ 9, 95   SAY XSTADDR2
@ 10, 5   SAY XBTNAME
@ 10, 48  SAY XSTNAME
@ 10, 95  SAY XSTADDR3
@ 11, 5   SAY XBTADDR1
@ 11, 48  SAY XSTADDR1
@ 12, 5   SAY XBTADDR2
@ 12, 48  SAY XSTADDR2
@ 13, 5   SAY XBTADDR3
@ 13, 48  SAY XSTADDR3
@ 13, 86  SAY LEFT(lcCustPoVl,13)
@ 13, 100 SAY XSTORE
@ 13, 111 SAY XDept
@ 13, 120 SAY XORDER
@ 15, 101 SAY XPIKTKT
@ 15, 108 SAY 'SHIP VIA: '+SUBSTR(XSHIP_DATA,1,10)
@ 17, 93  SAY IIF(llPrntComp , HLINE1 , '')
@ 18, 1   SAY XACCOUNT
@ 18, 7   SAY XORDER
@ 18, 13  SAY XREP1
@ 18, 17  SAY XREP2
*@ 18, 21  SAY XENTERD
@ 18, 20  SAY XENTERD
*@ 18, 30  SAY XSTART
@ 18, 28  SAY XSTART
*@ 18, 38  SAY XCOMPLT
@ 18, 35  SAY XCOMPLT
@ 18, 46  SAY SUBSTR(XTERM_DATA,1,14)
@ 18, 61  SAY XSTORE
@ 18, 69  SAY lcCustPoVl PICTURE '##########'
@ 18, 80  SAY XDept
@ 18, 93  SAY IIF(llPrntComp , HLINE2 , '')
@ 19, 93  SAY IIF(llPrntComp , HLINE3 , '')

ROW = 20
IF LLSCALE
  STORE SPACE(0) TO LASCALESZ
  LNCOUNT   = 1
  LCSCALCNT = 0
  LCSELFILE = SELECT(0)
  
  SELECT ORDLINE
  SEEK ORDHDR.CORDTYPE + XORDER
  LCKEY = CORDTYPE+ORDER+STR(LINENO,6)

  SCAN REST WHILE XORDER = ORDER .AND. LCSCALCNT<=5 FOR IIF(EMPTY(XSTORE) , .T. , &lcTmpOrdL..PIKTKT = ORDLINE.PIKTKT)
    IF ASCAN(LASCALESZ,SUBSTR(ORDLINE.SCALE,1,2))=0 .AND. SEEK('S'+ORDLINE.SCALE,'SCALE')
      LCSCALCNT = LCSCALCNT + 1
      DIMENSION LASCALESZ[ LCSCALCNT]
      *LCSCALSIZ = SUBSTR(ORDLINE.SCALE,1,2) + PADL(ALLTRIM(SCALE.SZ1),4,' ') + ;
                                        ' ' + PADL(ALLTRIM(SCALE.SZ2),4,' ') + ;
                                              PADL(ALLTRIM(SCALE.SZ3),4,' ') + ;
                                        ' ' + PADL(ALLTRIM(SCALE.SZ4),4,' ') + ;
                                              PADL(ALLTRIM(SCALE.SZ5),4,' ') + ;
                                        ' ' + PADL(ALLTRIM(SCALE.SZ6),4,' ') + ;
                                              PADL(ALLTRIM(SCALE.SZ7),4,' ') + ;
                                        ' ' + PADL(ALLTRIM(SCALE.SZ8),4,' ')
      LCSCALSIZ = SUBSTR(ORDLINE.SCALE,1,2) + PADL(ALLTRIM(SCALE.SZ1),5,' ') + ;
                                        ' ' + PADL(ALLTRIM(SCALE.SZ2),5,' ') + ;
                                              PADL(ALLTRIM(SCALE.SZ3),5,' ') + ;
                                        ' ' + PADL(ALLTRIM(SCALE.SZ4),5,' ') + ;
                                              PADL(ALLTRIM(SCALE.SZ5),5,' ') + ;
                                        ' ' + PADL(ALLTRIM(SCALE.SZ6),5,' ') + ;
                                              PADL(ALLTRIM(SCALE.SZ7),5,' ') + ;
                                        ' ' + PADL(ALLTRIM(SCALE.SZ8),5,' ')

      LASCALESZ[ LCSCALCNT] = LCSCALSIZ
    ENDIF
  ENDSCAN
  SEEK (LCKEY)
  FOR LNCOUNT = 1 TO ALEN(LASCALESZ,1)
    *@ ROW, 19 SAY LASCALESZ(LNCOUNT)
    @ ROW, 17 SAY LASCALESZ(LNCOUNT)
    IF LNCOUNT = 1
      @ ROW, 93 SAY IIF(llPrntComp , HLINE4 , '')
    ENDIF
    DO LPSIDLABLS
    ROW = ROW + 1
  ENDFOR
  LLSCALE = .F.
  SELECT (LCSELFILE)
ELSE
  FOR LNCOUNT = 1 TO ALEN(LASCALESZ,1)
    @ ROW, 19 SAY LASCALESZ(LNCOUNT)
    IF LNCOUNT = 1
      @ ROW, 93 SAY IIF(llPrntComp , HLINE4 , '')
    ENDIF
    DO LPSIDLABLS
    ROW = ROW + 1
  ENDFOR
  @ ROW, 93 SAY IIF(llPrntComp , HLINE4 , '')
ENDIF
ROW = IIF(ROW < 23 , 23 , ROW)

*--End of lpHrdLabls.
*!*************************************************************
*! Name      : lpOldFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : Print the form footer at the middle.
*!*************************************************************
*! Called from : .PRG
*!*************************************************************
*! Calls       : lpSidLabls
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpOldFoter()
*!*************************************************************
PROCEDURE lpOldFoter

@ 55, 93  SAY XSTNAME
@ 56, 93  SAY XSTADDR1
@ 57, 12  SAY '*** CONTINUED NEXT PAGE ***'
@ 57, 93  SAY XSTADDR2
@ 58, 93  SAY XSTADDR3
@ 63, 86  SAY LEFT(lcCustPoVl,13)
@ 63, 100 SAY XSTORE
@ 63, 111 SAY XDept
@ 63, 120 SAY XORDER
@ 64, 101 SAY XPIKTKT
@ 64, 108 SAY 'SHIP VIA: '+SUBSTR(XSHIP_DATA,1,10)
ROW = ROW + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name      : lpNewFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : Print the form footer at the end.
*!*************************************************************
*! Called from : .PRG
*!*************************************************************
*! Calls       : lpSidLabls
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpNewFoter()
*!*************************************************************
PROCEDURE lpNewFoter

=LFCHKROW('A')
@ 54, 0  SAY LCRPMSG2
@ 55, 0  SAY LCRPMSG3
@ 56, 93 SAY XSTNAME
@ 57, 93 SAY XSTADDR1
= SEEK('O' + XORDER ,'ORDHDR')
LNCOL = 11
IF SUBSTR(ORDHDR.NOTE1,1,1) <> '*'
  @ 58, LNCOL SAY ORDHDR.NOTE1
  LNCOL = 42
ENDIF
@ 58, LNCOL SAY IIF(SUBSTR(ORDHDR.NOTE2,1,1) <> '*' , SUBSTR(ORDHDR.NOTE2,1,25),'')
@ 58, 76  SAY XPIECES PICTURE '999999'
@ 58, 93  SAY XSTADDR2
@ 59, 93  SAY XSTADDR3
@ 62, 60  SAY SUBSTR(XSHIP_DATA,1,11)
@ 62, 76  SAY IIF(LLRPSTYPRC , XVALUE , '') PICTURE '999999.99'
@ 63, 86  SAY LEFT(lcCustPoVl,13)
@ 63, 100 SAY XSTORE
@ 63, 111 SAY XDept
@ 63, 120 SAY XORDER
@ 64, 101 SAY XPIKTKT
@ 64, 108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
XVALUE = 0.00
ROW    = ROW + 1

*--End of lpNewFoter.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : .PRG
*!*************************************************************
*! Calls       : lpSidLabls
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpPrtSku()
*!*************************************************************
PROCEDURE lpPrtSku

IF !SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Lin')
  LCSTRTOPRN = ' '
  RETURN
ENDIF

SELECT SPCK_LIN
IF EMPTY(SKU)
  = SEEK('S'+STYLE.SCALE,'Scale')
  = SEEK('M'+PIKTKT.Account,'Customer')

  *B605945,1 BWA 10/14/2002 Fix the bug of printing the right SKU.[START]
  *lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  *IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
  *  lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
  *  lnDime2 = SkuTmpl.Len4
  *ELSE
  *  lnDime1 = 8  &&Default
  *  lnDime2 = 8  &&Default
  *ENDIF

  PRIVATE lcAlasSpkH , lcOrdrSpkH
  lcAlasSpkH = SELECT(0)
  SELECT SPCK_HDR
  lcOrdrSpkH = ORDER('Spck_Hdr')
  SET ORDER TO TAG Sku_style IN SPCK_HDR
  IF SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Hdr')
    lnDime1 = LEN(ALLTRIM(Spck_Hdr.pack_id))
  	lnLenDm1 = lnDime1
  ELSE
    lcStrToPrn = " "
    RETURN
  ENDIF
  SET ORDER TO TAG &lcOrdrSpkH
  SELECT(lcAlasSpkH)
  *B605945,1 BWA 10/14/2002.[END]

  = SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Lin')
  LCSTRTOPRN = 'SKU#: '+SUBSTR(PACK_ID,1,LNDIME1)
  LCSTRTOPRN = IIF(LEN(LCSTRTOPRN)>65,SUBSTR(LCSTRTOPRN,1,65),ALLTRIM(LCSTRTOPRN))
  LNLENGTH   = LEN(LCSTRTOPRN) + 2
  LNLOCCOL   = LEN(LCSTRTOPRN) + 5
ELSE
  @ ROW, 1 SAY SUBSTR(SKU,1,8)
  LNLOCCOL = 35
  @ ROW, 17 SAY 'CUSTOMER SKU #'
ENDIF
RETURN

*--End of lpPrtSku.
*!*************************************************************
*! Name      : lfGetLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : Function to get the location.
*!*************************************************************
*! Called from : .PRG
*!*************************************************************
*! Calls       : lpSidLabls
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfGetLoc()
*!*************************************************************
PROCEDURE lfGetLoc
PRIVATE LCALIAS

LCALIAS = ALIAS()
LCSTR   = ' '
SELECT WHSLOC
IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE Style = &lcTmpOrdL..Style
    LCSTR = LCSTR + ' ' + ALLTRIM(CLOCATION)
  ENDSCAN
  LCSTR = ALLTRIM(LCSTR)
  LNLEN = LEN(LCSTR)
ENDIF
SELECT (LCALIAS)

*--End of lfGetLoc.
*!*************************************************************
*! Name      : lfContuLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : Print the location.
*!*************************************************************
*! Called from : .PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfContuLoc()
*!*************************************************************
FUNCTION lfContuLoc

FOR I = 12 TO LNLEN
  @ ROW, 0 SAY SUBSTR(LCSTR,I,86)
  I = I + 86
  DO LPSIDLABLS
  ROW = ROW + 1
  = LFCHKROW('B')
ENDFOR

*--End of lfContuLoc.
*!*************************************************************
*! Name      : lfChkRow
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : To check the row position.
*!*************************************************************
*! Called from : .PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfChkRow()
*!*************************************************************
FUNCTION lfChkRow
PARAMETER LCTYPE

IF ROW>=IIF(LCTYPE = 'A' , 54 , 55)
  DO LPOLDFOTER
  DO LPHRDLABLS
ENDIF

*--End of lfChkRow.
*!*************************************************************
*! Name      : lfPrnStrn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/20/2002
*! Purpose   : To print the acount/style/color skus.
*!*************************************************************
*! Called from : ALPKTKS.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfPrnStrn()
*!*************************************************************
PROCEDURE lfPrnStrn
PRIVATE LNALIAS

LNALIAS = SELECT(0)
LCSKUSIZE = ''

*-- To check the existence of Account/Style/Color SKUs 
IF SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Lin')
  LCKEY = 'S'+PIKTKT.Account+&lcTmpOrdL..Style
  = SEEK('S'+STYLE.SCALE,'Scale')

  *-- Declaring this array to hold the Skus in order to sort them
  DIMENSION LASKUS[ SCALE.CNT, 2]
  LASKUS = ''

  *-- Saving the customer pointer
  LNCUSTREC = RECNO('Customer')

  *-- Searching for the main account to get check the customer skutmpl
  = SEEK('M'+PIKTKT.ACCOUNT,'Customer')
  LCSKUTMPL = IIF( .NOT. EMPTY(CUSTOMER.SKUTMPL),CUSTOMER.SKUTMPL,'DEF')
  IF BETWEEN(LNCUSTREC,1,RECCOUNT('Customer'))
    GOTO IN CUSTOMER LNCUSTREC
  ENDIF

  IF SEEK('S'+LCSKUTMPL,'SkuTmpl')
    LNDIME1 = SKUTMPL.LEN1+SKUTMPL.LEN2+SKUTMPL.LEN3
    LNDIME2 = SKUTMPL.LEN4
  ELSE
    LNDIME1 = 8
    LNDIME2 = 8
  ENDIF

  SELECT SPCK_LIN
  SCAN REST WHILE 'S'+ACCOUNT+STYLE = LCKEY
    FOR LNCOUNTER = 1 TO SCALE.CNT
      LCCOUNTER = STR(LNCOUNTER,1)
      IF Qty&lcCounter > 0
        LASKUS[ LNCOUNTER, 1] = STR(LNCOUNTER,1)
        LASKUS[ LNCOUNTER, 2] = SUBSTR(PACK_ID,LNDIME1+1,5)+' '
        EXIT
      ENDIF
    ENDFOR
  ENDSCAN

  = ASORT(LASKUS,1)
  FOR LNARRAYCNT = 1 TO ALEN(LASKUS,1)
    LCARRAYCNT = ALLTRIM(STR(LNARRAYCNT))
    IF &lcTmpOrdL..Qty&lcArrayCnt > 0


      *B605945,1 BWA 10/14/2002 Fix the bug of printing the right SKU.[START]
      *LCSKUSIZE = LCSKUSIZE+'S'+LCARRAYCNT+':'+ALLTRIM(LASKUS(LNARRAYCNT,2))+' '
      PRIVATE lcAlasSpkh , lcRecNoH
      lcAlasSpkh = SELECT(0)
      SELECT SPCK_LIN
      =SEEK(lckey)
      lcRecNoH = RECNO()
      SCAN REST WHILE TYPE + ACCOUNT + STYLE + PACK_ID = lcKey
        IF SPCK_LIN.QTY&lcArrayCnt = 1
          lcSkuSize = lcSkuSize + 'S' + lcArrayCnt + ':' + SUBSTR(ALLTRIM(Pack_Id),lnLenDm1+1) + ' '
          GOTO lcRecNoH IN SPCK_LIN
          EXIT
        ENDIF
      ENDSCAN
      *B605945,1 BWA 10/14/2002.[END]

    ENDIF
  ENDFOR

  LCSKUSIZE = ALLTRIM(LCSKUSIZE)
ENDIF
SELECT (LNALIAS)
RETURN

*--End of lfPrnStrn.