*:***************************************************************************
*: Program file  : ALPKTKPO.PRG
*: Program desc. : Customized Picking Ticket Form for POI04 (Making from FORM_S)
*: Date          : 03/16/2004
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : NADER NABIL ABD ELMONEM (NNA)
*:***************************************************************************
*: Calls : 
*:    Procedures : lpPrtSku,lpNewFoter,lpOldFoter,lpHrdLabls,lpSidLabls,lpFilEmpLn
*:    Functions  : lfContuLoc(),lfGetLoc(),lfPrnStrn()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : CP # 037893
*:***************************************************************************
*: Example : DO ALPKTKPO
*:***************************************************************************
*: Modification:
*****************************************************************************

           *--Section of Initializing the variables --*
STORE SPACE(1) TO LCSTRTOPRN, LCSKUSIZE, LCSCLSTRN, LCSTR, HLINE1
STORE SPACE(0) TO LCOLDSCAL,LCSCALSIZ
PRIVATE LCSELFILE
LLNOREC  = .F.                && Variable used to make the endreport function in the main program.
LNLENGTH = 7
LNLEN    = 0
llNewPag = .F.				  && Variable used to know if we are in a new page (after the first page)

*--bas
llPrnRcord = .T.              && Variable to check if the record is printed or not.
*--bas


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
*LLRET      = .F.

DIMENSION LASCALESZ[5]
STORE SPACE(0) TO LASCALESZ

*--Variable hold the length of the sku in the spck_hdr.
STORE 0 TO lnLenDm1

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

    SELECT ORDHDR
    =SEEK('O'+XORDER,'ORDHDR')

    XDept   = OrdHdr.Dept
    XREP1   = ORDHDR.REP1
    XREP2   = ORDHDR.REP2
    XENTERD = ORDHDR.ENTERED
    XSTART  = ORDHDR.START
    XCOMPLT = ORDHDR.COMPLETE


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
	llnewpag=.T.
*  IF LLRET
*    LOOP
*  ENDIF

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
	  XSTYLE     = STYLE
      XSTYDESC   = DESC
      LCSTYLOCAT = LOCATION
      LCSCALE    = SCALE
    ELSE
	  XSTYLE     = ''
      XSTYDESC   = ''
      LCSTYLOCAT = ''
      LCSCALE    = ''
    ENDIF

    SELECT (lcTmpOrdL)
	LCSELFILE = ALIAS()
 	LCOLDSCAL = LCSCALSIZ

	=lfPrnScale()
	=lfPrnQty()
  
    IF llNewPag = .T.			&& If it a new Page after the page No.1 but inside the same pik.
      llNewPag = .F. 	  
    ENDIF
  
    =LFCHKROW('B')				&& we are within the same pick Ticket
 	
    DO LPSIDLABLS
    ROW = ROW + 1
    = LFCHKROW('B')

    *--bas
    IF llNewPag AND llPrnRcord
      llNewPag = .F.
      =lfPrnQty()
    ENDIF

	IF !llPrnRcord
      llPrnRcord = .T.
	ENDIF
    *--bas

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
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : Fill the empty pick ticket lines.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : Print the Side labels.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
@ 7, 60   SAY ORDHDR.APPROVAL
@ 7, 74   SAY XDATE
@ 7, 95   SAY XSTNAME
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
@ 18, 20  SAY XENTERD
@ 18, 28  SAY XSTART
@ 18, 35  SAY XCOMPLT
@ 18, 46  SAY SUBSTR(XTERM_DATA,1,14)
@ 18, 61  SAY XSTORE
@ 18, 69  SAY lcCustPoVl PICTURE '##########'
@ 18, 80  SAY XDept
@ 18, 93  SAY IIF(llPrntComp , HLINE2 , '')
@ 19, 93  SAY IIF(llPrntComp , HLINE3 , '')
@ 20, 93 SAY IIF(llPrntComp , HLINE4 , '')
lcoldAlis = SELECT(0)
ROW = 22
SELECT ORDLINE
DO LPSIDLABLS
ROW = ROW + 1
SELECT (lcoldAlis)
*--End of lpHrdLabls.
*!*************************************************************
*! Name      : lpOldFoter
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : Print the form footer at the middle.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
@ 62, 86  SAY LEFT(lcCustPoVl,13)
@ 62, 100 SAY XSTORE
@ 62, 111 SAY XDept
@ 62, 120 SAY XORDER
@ 64, 101 SAY XPIKTKT
@ 64, 108 SAY 'SHIP VIA: '+SUBSTR(XSHIP_DATA,1,10)
ROW = ROW + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name      : lpNewFoter
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : Print the form footer at the end.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
=LFCHKROW('A')				&& Here we are with a new pick ticket
IF ROW=53
  ROW = ROW + 1	
  @ ROW, 0  SAY LCRPMSG2
ELSE
  @ 54, 0  SAY LCRPMSG2
ENDIF  
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
@ 61, 60  SAY SUBSTR(XSHIP_DATA,1,11)
@ 61, 76  SAY IIF(LLRPSTYPRC , XVALUE , '') PICTURE '999999.99'
@ 62, 86  SAY LEFT(lcCustPoVl,13)
@ 62, 100 SAY XSTORE
@ 62, 111 SAY XDept
@ 62, 120 SAY XORDER
@ 63, 101 SAY XPIKTKT
@ 63, 108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
XVALUE = 0.00
ROW    = ROW + 1

*--End of lpNewFoter.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : Function to get the location.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : Print the location.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : To check the row position.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
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
IF ROW >= IIF(LCTYPE = 'A' , 54 , 55) 
  DO LPOLDFOTER
  DO LPHRDLABLS
  llNewPag = .T.			&& here we are in a new page and the same pik.
ENDIF

*--End of lfChkRow.
*!*************************************************************
*! Name      : lfPrnStrn
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
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

    ENDIF
  ENDFOR

  LCSKUSIZE = ALLTRIM(LCSKUSIZE)
ENDIF
SELECT (LNALIAS)
RETURN

*--End of lfPrnStrn.

*!*************************************************************
*! Name      : lfPrnScale
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : To print the acount/style/color skus.
*!*************************************************************
*! Called from : ALPKTKPO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfPrnScale()
*!*************************************************************
FUNCTION lfPrnScale
IF LLSCALE
  IF PIKTKT.STATUS='C' .OR. PIKTKT.STATUS='X'
    SELECT PIKLINE      
    SEEK PIKTKT.PIKTKT
    SCAN REST WHILE &lcTmpOrdL..PIKTKT = PIKLINE.PIKTKT .AND. XORDER = ORDER 
    IF  XSTYLE = Style
      IF ASCAN(LASCALESZ,SUBSTR(PIKLINE.SCALE,1,2))=0 .AND. SEEK('S'+PIKLINE.SCALE,'SCALE')
        LCSCALSIZ = SUBSTR(PIKLINE.SCALE,1,2) + PADL(ALLTRIM(SCALE.SZ1),3,' ') + ;
                                          ' ' + PADL(ALLTRIM(SCALE.SZ2),5,' ') + ;
                                                PADL(ALLTRIM(SCALE.SZ3),5,' ') + ;
                                          ' ' + PADL(ALLTRIM(SCALE.SZ4),4,' ') + ;
                                                PADL(ALLTRIM(SCALE.SZ5),5,' ') + ;
                                          ' ' + PADL(ALLTRIM(SCALE.SZ6),4,' ') + ;
                                                PADL(ALLTRIM(SCALE.SZ7),5,' ') + ;
                                          ' ' + PADL(ALLTRIM(SCALE.SZ8),4,' ')
      ENDIF
    ENDIF
    ENDSCAN
  ELSE
    SELECT ORDLINE
    SEEK ORDHDR.CORDTYPE + XORDER
    SCAN REST WHILE XORDER = ORDER FOR IIF(EMPTY(XSTORE) , .T. , &lcTmpOrdL..PIKTKT = ORDLINE.PIKTKT)      
    IF  XSTYLE = Style
      IF ASCAN(LASCALESZ,SUBSTR(ORDLINE.SCALE,1,2))=0 .AND. SEEK('S'+ORDLINE.SCALE,'SCALE')
        LCSCALSIZ = SUBSTR(ORDLINE.SCALE,1,2) + PADL(ALLTRIM(SCALE.SZ1),3,' ') + ;
                                          ' ' + PADL(ALLTRIM(SCALE.SZ2),5,' ') + ;
                                                PADL(ALLTRIM(SCALE.SZ3),5,' ') + ;
                                          ' ' + PADL(ALLTRIM(SCALE.SZ4),4,' ') + ;
                                                PADL(ALLTRIM(SCALE.SZ5),5,' ') + ;
                                          ' ' + PADL(ALLTRIM(SCALE.SZ6),4,' ') + ;
                                                PADL(ALLTRIM(SCALE.SZ7),5,' ') + ;
                                          ' ' + PADL(ALLTRIM(SCALE.SZ8),4,' ')
      ENDIF
    ENDIF
    ENDSCAN
  ENDIF
ENDIF  

RETURN LCSCALSIZ
*-- END OF FUNCTION

*!*************************************************************
*! Name      : lfPrnQty
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : To print the style Qty. in the same page or in a new page
*!*************************************************************
*! Called from : ALPKTKPO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfPrnQty()
*!*************************************************************
FUNCTION lfPrnQty
	
*-- Print scale in three cases
*--- 1 - if scale changed
*--- 2 - if we are in the same pick ticket but with a new page
*--- 3 - if the pick ticket No. Changed (then we are in a new page)
IF ROW <= 52			&& To make a relation between the scale / style / qtys lines to Print all of them in the same page
  IF LCOLDSCAL<>LCSCALSIZ .OR. llNewPag = .T. .OR. XPIKTKT <> &lcTmpOrdL..PIKTKT
    @ ROW, 10 SAY 'Size : '			
    @ ROW, 19 SAY RIGHT(LCSCALSIZ,LEN(LCSCALSIZ)-2)			&& Print Sizes Scale
    DO LPSIDLABLS
    ROW = ROW + 1
    = LFCHKROW('B')
  ENDIF
  SELECT (LCSELFILE)
  @ ROW, 01 SAY SUBSTR(STYLE,1,12)			&& STYLE MAJOR
  @ ROW, 13 SAY SUBSTR(STYLE,14,19)			&& STYLE COLOR
  @ ROW, 21 SAY ': ' + XSTYDESC				&& STYLE DESCRIPTION
  = lfPrnSk_Pk()
   DO LPSIDLABLS
   ROW = ROW + 1
   = LFCHKROW('B')
  SELECT (LCSELFILE)
  @ ROW, 19 SAY PIK1 PICTURE '@Z 9999'
  @ ROW, 24 SAY PIK2 PICTURE '@Z 9999'
  @ ROW, 29 SAY PIK3 PICTURE '@Z 9999'
  @ ROW, 34 SAY PIK4 PICTURE '@Z 9999'
  @ ROW, 39 SAY PIK5 PICTURE '@Z 9999'
  @ ROW, 44 SAY PIK6 PICTURE '@Z 9999'
  @ ROW, 49 SAY PIK7 PICTURE '@Z 9999'
  @ ROW, 54 SAY TOTPIK PICTURE '@Z 99999'

  IF LLRPSTYPRC
    *-- UNIT PRICE 	
    @ ROW, 61 SAY PRICE PICTURE '9999.99'
  ENDIF
  *-- QTY SHIPPED
  @ ROW, 70 SAY TOTPIK PICTURE '99999'
  
  *--bas
  llPrnRcord = .F.
  *--bas

ENDIF
DO LPSIDLABLS
ROW = ROW + 1

*-- END OF FUNCTION lfPrnQty()
*!*************************************************************
*! Name      : lfPrnSk_Pk
*! Developer : NADER NABIL ABD ELMONEM (NNA)
*! Date      : 03/16/2004
*! Purpose   : To print the style SKU OR Customer PACK in the same page or in a new page
*!*************************************************************
*! Called from : ALPKTKPO.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfPrnSk_Pk()
*!*************************************************************
FUNCTION lfPrnSk_Pk

    *-- If (Print SKU/Pack = NO)
    IF !llRpSkuBck							                        
      IF LLRPSTYLOC
        = LFGETLOC()
        IF !EMPTY(LCSTR)
          @ ROW, 50 SAY 'BINS    :'+SUBSTR(LCSTR,1,10)
          IF LEN(LCSTR)>18
            = LFCONTULOC()
          ENDIF
        ENDIF
      ENDIF

    *-- If (Print SKU/Pack = YES)
    ELSE						
      IF SEEK('P'+PIKTKT.Account+&lcTmpOrdL..Style,'SPCK_LIN') 
        @ ROW, 50 SAY 'PACK ID#:'+SPCK_LIN.PACK_ID
      ELSE
        DO LPPRTSKU
        *-- If (Print SKU by size = NO)
        IF !llRpSkuSiz
          @ ROW, 50 SAY SUBSTR(LCSTRTOPRN,1,IIF(LEN(LCSTRTOPRN)>=40,40,LEN(LCSTRTOPRN)))
          *-- If (Print SKU Bins = YES)
          IF LLRPSTYLOC
            = LFGETLOC()
            IF !EMPTY(LCSTR)
              DO LPSIDLABLS
              ROW = ROW + 1
              = LFCHKROW('B')		&& we are within the same pick Ticket
              @ ROW, 0 SAY 'BINS    :'+SUBSTR(LCSTR,1,85)
              IF LEN(LCSTR)>85
                = LFCONTULOC()
              ENDIF
            ENDIF
          ENDIF
        *-- If (Print SKU by size = YES)
        ELSE
          IF LLRPSTYLOC
            = LFGETLOC()
            IF !EMPTY(LCSTR)
              @ ROW, 50 SAY 'BINS    :'+SUBSTR(LCSTR,1,10)
              IF LEN(LCSTR)>18
                = LFCONTULOC()
              ENDIF
            ENDIF
          ENDIF
          = LFPRNSTRN()
          LCSKUSIZE = LCSTRTOPRN + ' ' + LCSKUSIZE
          IF !EMPTY(LCSKUSIZE)
            DO LPSIDLABLS
            ROW = ROW + 1
            = LFCHKROW('B')
          ENDIF
          @ ROW, 0 SAY SUBSTR(LCSKUSIZE,1,86)
        ENDIF
      ENDIF
    ENDIF
