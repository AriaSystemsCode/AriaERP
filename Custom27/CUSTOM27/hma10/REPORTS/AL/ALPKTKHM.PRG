*:***************************************************************************
*: Program file : ALPKTKHM.PRG
*: DESC : PRINT PICK TICKET (For HMA)
*: Module: Aria Apparel Series.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*: C102377 Refer to.
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:         PROCEDURE  : 
*:************************************************************************
*: Modifications :
*: B605658,1 BWA 03/24/2002 Move all the piktkt one line up and fix.[START]
***************************************************************************

*B605658,1 BWA 03/24/2002 Move all the piktkt one line up and fix.[START]
*-- Set the memory variables.

STORE .F. To llNoRec
llRpStyLoc = IIF(gfGetMemVar('M_WareHouse') = 'Y' , .T. , .F.)

*--open this files again with another index
SELECT SPCK_LIN
SET ORDER TO SPCKLINS INTO SPCK_LIN ADDITIVE

PRIVATE lcStrToPrn
lcStrToPrn = ""
SELECT (lcTmpOrdL)
GO TOP
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

DECLARE laSoldTo[5,1] , laShipTo[5,1]
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address

PRIVATE HLine1,HLine2,HLine3,HLine4
STORE '' TO HLine1,HLine2,HLine3,HLine4

DECLARE laDisRltFld[1,2]
laDisRltFld[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDisRltFld[1,2] = 'HLine1'


HLINE2 = TRIM(laCompAdd[1])
HLINE3 = TRIM(laCompAdd[2])
HLINE4 = TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5]

IF LEN(HLINE3) = 0
  HLINE3=HLINE4
ENDIF

SELECT (lcTmpOrdL)

*-- MAIN LOOP
llEndPT = .F.
NEWDOC  = .T.
lnNotLine = 1
lnTotQty  = 0
XVALUE    = 0

SET DEVICE TO PRINT

SELECT (lcTmpOrdL)
DO WHILE !EOF(lcTmpOrdL)
  WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT
  SELECT (lcTmpOrdL)
  IF NEWDOC
     SELECT (lcTmpOrdL)
     XPIKTKT  = PIKTKT
     XORDER   = ORDER
     XDATE    = PIKTKT.DATE
     XSTORE   = PIKTKT.Store
     lnTotQty = 0

     SELECT ORDHDR
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     STORE 0.00 TO XORDTOT, XVALUE , SVALUE

     SELECT ORDLINE
     SET ORDER TO ordlinst
     = SEEK("O" + XORDER+XSTORE)
     IF ORDHDR.MultiPO
       lcCustPO = OrdLine.CustPO
     ENDIF

     IF PIKTKT <> XPIKTKT
        LOCATE REST FOR PIKTKT = XPIKTKT ;
                    WHILE ORDER+STORE = XORDER+XSTORE
     ENDIF
     GO TOP

     =lfGetAddr()
     XBTNAME  = lcSolTName
     XBTADDR1 = laSoldTo[1]
     XBTADDR2 = laSoldTo[2]
     XBTADDR3 = LEFT(ALLTRIM(laSoldTo[3]) + ' ' +ALLTRIM(laSoldTo[4]) + ' ' + laSoldTo[5],37)
     IF LEN(TRIM(XBTADDR2)) =0
       XBTADDR2 = XBTADDR3
       XBTADDR3 = ''
     ENDIF

     *-- GET DESCRIPTIONS FOR CODED FIELDS

     *------------------------------------
     * GET DESCRIPTIONS FOR CODED FIELDS
     *------------------------------------
     XSEAS_DATA = SUBSTR(gfCodDes(ORDHDR.Season , 'SEASON'),1,7)
     XTERM_DATA = gfCodDes(ORDHDR.ctermcode,'cTermCode')
     XSPCI_DATA = gfCodDes(ORDHDR.SPCINST , 'SPCINST   ')
     XDIVI_DATA = SUBSTR(gfCodDes(ORDHDR.CDivision , 'CDIVISION '),1,7)
     IF !EMPTY(XDIVI_DATA)
       =gfRltFld(OrdHdr.cDivision,@laDisRltFld,'CDIVISION')
       HLINE1 = IIF(EMPTY(HLINE1) , lcCompName , HLINE1)
     ENDIF

     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
     XPIECES = 0
     *--------------------------------------------
     * SHIP-TO ADDRESS FOR THIS STORE
     *--------------------------------------------
     XSTNAME  = IIF(EMPTY(CUSTOMER.DBA) , lcShpTName , ALLTRIM(CUSTOMER.DBA))
     XSTADDR1 = laShipTo[1]
     XSTADDR2 = laShipTo[2]
     XSTADDR3 = LEFT(ALLTRIM(laShipTo[3]) + ' ' +ALLTRIM(laShipTo[4]) + ' ' + ALLTRIM(laShipTo[5]),37)

     IF LEN(TRIM(XSTADDR2)) = 0
       XSTADDR2 = XSTADDR3
       XSTADDR3 = ''
     ENDIF

     *-- GET THE SIZE SCALES
     *-- Moved and enhanced to get the printed scales
     *-- once outside the loop not every time in the loop because it will 
     *-- be the same

     STORE SPACE(1) TO XSCALE2 , lcScale3 , lcScale4 , lcScale5

     SELECT (lcTmpOrdL)
     lnRecNo = RECNO()
     XSCALE1 = SCALE
     LOCATE REST FOR SCALE <> XSCALE1 AND !EMPTY(&lcTmpOrdL..SCALE)
     IF !EOF()
       XSCALE2 = SCALE
     ENDIF
     GO TOP

     LOCATE REST FOR SCALE <> XSCALE1 .AND. SCALE <> XSCALE2 AND !EMPTY(&lcTmpOrdL..SCALE)
     IF !EOF()
       lcScale3 = SCALE
     ENDIF
     GO TOP

     LOCATE REST FOR SCALE <> XSCALE1 .AND. SCALE <> XSCALE2 .AND. SCALE <> lcScale3 AND !EMPTY(&lcTmpOrdL..SCALE)
     IF !EOF()
       lcScale4 = SCALE
     ENDIF
     GO TOP
     LOCATE REST FOR SCALE <> XSCALE1  AND SCALE <> XSCALE2 AND ;
                     SCALE <> lcScale3 AND SCALE <> lcScale4 AND !EMPTY(&lcTmpOrdL..SCALE)
     IF !EOF()
       lcScale5 = SCALE
     ENDIF
     GO TOP
     GOTO lnRecNo
  ENDIF

  SELECT ORDHDR

  *-- START PRINTING
  DO lpHrdLabls

  @ 22,93 SAY XSTNAME

  *-- LINE LOOP
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0
  ROW     = 23
  SCAN REST WHILE PikTkt + Order + cGrupDetal + STR(LineNo , 6) = ;
                  XPIKTKT + XORDER FOR  LINENO <> 0

      *-- Modified to get the Style/Color location.
      SELECT Style
      IF SEEK(&lcTmpOrdL..Style)
        XSTYDESC   = DESC
        lcStyLocat = cdefware
      ELSE
        XSTYDESC   = ''
        lcStyLocat = ''
      ENDIF

      *-- To get the color description.

      lcClrDesc = PADR(ALLTRIM(gfCodDes(SUBSTR(Style.Style,14,6),"COLOR")),30)
      SELECT (lcTmpOrdL)
      @ ROW,01 SAY STYLE
      @ ROW,22 SAY SCALE
      @ ROW,24 SAY PIK1   PICTURE '@Z 9999'
      @ ROW,28 SAY PIK2   PICTURE '@Z 9999'
      @ ROW,32 SAY PIK3   PICTURE '@Z 9999'
      @ ROW,36 SAY PIK4   PICTURE '@Z 9999'
      @ ROW,40 SAY PIK5   PICTURE '@Z 9999'
      @ ROW,44 SAY PIK6   PICTURE '@Z 9999'
      @ ROW,48 SAY PIK7   PICTURE '@Z 9999'
      @ ROW,52 SAY PIK8   PICTURE '@Z 9999'
      @ ROW,56 SAY TOTPIK PICTURE '99999'

      lnTotQty = lnTotQty + TOTPIK * PRICE

      IF llRpStyPrc
        @ ROW,63 SAY PRICE PICTURE '9999.99'
        @ ROW,72 SAY TOTPIK*PRICE PICTURE '9999999.999'
      ENDIF

      DO lpSidLabls

      ROW = ROW + 1

      @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
      *-- To print the color description, And the
      *-- Style/Color location.
      IF llRpPrtClr .OR. llRpStyLoc
        @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.:' + lcClrDesc, '')
        IF llRpPrtClr
          @ ROW,59 SAY IIF(llRpStyLoc .AND. !EMPTY(lcStyLocat),'LOCATION:'+lcStyLocat, '')
        ELSE
          @ ROW,34 SAY IIF(llRpStyLoc.AND.!EMPTY(lcStyLocat),'LOCATION:'+lcStyLocat, '')      
        ENDIF
      ENDIF

      *--Print the bar code on the side label.
      IF ROW = 38
        *DO lpBarCode WITH lcCompNo,lcPicktktno,lcZipcode,1
      ELSE
        DO lpSidLabls
      ENDIF

      Row = Row + 1

      *--  SKU PRINTING FOR ACCOUNTS THAT HAVE SKU'S ON FILE.
      DO lpPrtSku
      @ ROW,00 SAY lcStrToPrn + '  CUSTOMER SKU #'
      DO lpSidLabls
      ROW = ROW + 1
      SELECT (lcTmpOrdL)

      * SUM ORDER TOTALS
      XTOTQTY = XTOTQTY+ TOTQTY
      XVALUE  = XVALUE + TOTPIK * PRICE
      XPIECES = XPIECES + TOTPIK
      SELECT (lcTmpOrdL)
      DO CASE
        CASE EOF()
          NEWDOC = .T.
          EXIT
        CASE TOTPIK <= 0
          SKIP
          LOOP
        CASE ROW >= 46
          NEWDOC = .F.
          EXIT
      ENDCASE
    ENDSCAN
  *-- END PRINT LINE LOOP

  SELECT (lcTmpOrdL)
  SKIP -1

  IF NewDoc
    *-- To print the notepad.
    IF llRpOrdNot
      SELECT NotePad
      IF SEEK('B' + OrdHdr.Order)
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)
        @ Row,00 SAY '* -- N O T E S -- *'
        DO lpSidLabls
        Row = Row + 1
        DO WHILE lnNotLine <= lnMemLins
          IF Row >= 52
            DO lpOldFoter
            DO lpHrdLabls
            Row = 22
          ENDIF
          @ ROW,00 SAY MLINE(MNOTES,lnNotLine)
          DO lpSidLabls
          ROW = ROW + 1
          lnNotLine = lnNotLine + 1
        ENDDO
        SET MEMOWIDTH TO lnOldMemW
      ENDIF
    ENDIF
    lnNotLine = 1
    llEndPT = .T.
    DO lpFilEmpLn
    DO lpNewFoter
  ELSE
    llEndPT = .F.
    DO lpFilEmpLn
    DO lpOldFoter
    LOOP
  ENDIF

  SELECT (lcTmpOrdL)
  IF !EOF()
    SKIP
  ENDIF
ENDDO
WAIT CLEAR
SET DEVICE TO SCREEN
                       *-- End of the Program --*
*!*************************************************************
*! Name : lpFilEmpLn.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Fill the empty pick tiKit lines.
*!*************************************************************
*! Called from : 
*!*************************************************************
PROCEDURE lpFilEmpLn
DO WHILE Row <= 52
  DO lpSidLabls 
  Row = Row + 1
ENDDO

*--End of lpFilEmpLn.
*!*************************************************************
*! Name : lpSidLabls.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the Side labels.
*!*************************************************************
*! Called from : 
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 33 .OR. ROW = 49
    @ ROW,93 SAY IIF(llPrntComp,HLINE1,'')
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,93 SAY IIF(llPrntComp,HLINE2,'')
  CASE ROW = 35 .OR. ROW = 51
     @ ROW,93 SAY IIF(llPrntComp,HLINE3,'')
  CASE ROW = 36 .OR. ROW = 52
    IF llEndPT .AND. Row = 52 .AND. NewDoc
      @ 52,00 SAY lcRpMsg1
    ENDIF
    @ ROW,93 SAY IIF(llPrntComp,HLINE4,'')
  CASE ROW = 22 .OR. ROW = 39
    @ ROW,93 SAY XSTNAME
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,93 SAY XSTADDR1
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,93 SAY XSTADDR2
  CASE ROW = 25 .OR. ROW = 42
    @ ROW,93 SAY XSTADDR3
  CASE ROW = 29 .OR. ROW = 45
    @ ROW,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)
    @ ROW,100 SAY XSTORE
    @ ROW,109 SAY ORDHDR.DEPT
    @ ROW,115 SAY XORDER
  CASE ROW = 30 .OR. ROW = 47
    @ ROW,101 SAY XPIKTKT
ENDCASE

*--End of lpSidLabls.
*!*************************************************************
*! Name : lpHrdLabls.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the Header labels.
*!*************************************************************
*! Called from : 
*!         Procedures : Alo820E.
*!*************************************************************
PROCEDURE lpHrdLabls

*-- Line 0
@ 01,001 SAY IIF(llPrntComp,HLINE1,'')
@ 01,095 SAY IIF(llPrntComp,HLINE1,'')

*-- Line 1
@ 02,001 SAY IIF(llPrntComp,HLINE2,'')
@ 02,095 SAY IIF(llPrntComp,HLINE2,'')

*-- Line 2
@ 03,001 SAY IIF(llPrntComp,HLINE3,'')
@ 03,095 SAY IIF(llPrntComp,HLINE3,'')

*-- Line 3
@ 04,001 SAY IIF(llPrntComp,HLINE4,'')
@ 04,060 SAY XDIVI_DATA
@ 04,095 SAY IIF(llPrntComp,HLINE4,'')

@ 06,060 SAY ORDHDR.APPROVAL
@ 06,095 SAY XSTNAME

*-- Line 5
@ 07,095 SAY XSTADDR1

*-- Line 6
@ 08,095 SAY XSTADDR2

*-- Line 7
@ 09,010 SAY XBTNAME
@ 09,048 SAY XSTNAME
@ 09,095 SAY XSTADDR3

*-- Line 8
@ 10,010 SAY XBTADDR1
@ 10,048 SAY XSTADDR1

*-- Line 9
@ 11,010 SAY XBTADDR2
@ 11,048 SAY XSTADDR2

*-- Line 10
@ 12,010 SAY XBTADDR3
@ 12,048 SAY XSTADDR3
@ 12,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)
@ 12,100 SAY XSTORE
@ 12,109 SAY ORDHDR.DEPT
@ 12,115 SAY XORDER

*-- Line 11
@ 14,101 SAY XPIKTKT

*-- Line 13
@ 15,093 SAY IIF(llPrntComp,HLINE1,'')

*-- Line 15
@ 16,001 SAY XACCOUNT
@ 16,009 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  PICTURE '########'
@ 16,020 SAY XSTORE       PICTURE '#########'
@ 16,030 SAY ORDHDR.DEPT PICTURE '#####'
@ 16,037 SAY XORDER
@ 16,044 SAY SUBSTR(XTERM_DATA,1,14)
@ 16,060 SAY ORDHDR.REP1
@ 16,064 SAY ORDHDR.REP2
@ 16,068 SAY SUBSTR(gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'),1,14)
@ 16,093 SAY IIF(llPrntComp,HLINE2,'')
@ 17,093 SAY IIF(llPrntComp,HLINE3,'')

*-- Line 17 PRINT 1 ST SIZE SCALE
@ 18,22 SAY XSCALE1
XSCALE = GETSCALE( XSCALE1,SPACE(1))

@ 18,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
@ 18,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
@ 18,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
@ 18,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
@ 18,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
@ 18,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
@ 18,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
@ 18,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')

*-- Line 16
@ 18,93 SAY IIF(llPrntComp,HLINE4,'')

*-- Line 18 PRINT 2ND. SIZE SCALE
IF !EMPTY(XSCALE2)
  @ 19,22 SAY XSCALE2
  XSCALE = GETSCALE(XSCALE2,SPACE(1))
  @ 19,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  @ 19,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  @ 19,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  @ 19,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  @ 19,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  @ 19,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  @ 19,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  @ 19,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
ENDIF

IF !EMPTY(lcScale3)
  @ 20,22 SAY lcScale3
  XSCALE = GETSCALE(lcScale3,SPACE(1))
  @ 20,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  @ 20,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  @ 20,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  @ 20,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  @ 20,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  @ 20,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  @ 20,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  @ 20,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
ENDIF

IF !EMPTY(lcScale4)
  @ 21,22 SAY lcScale4
  XSCALE = GETSCALE(lcScale4,SPACE(1))
  @ 21,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  @ 21,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  @ 21,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  @ 21,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  @ 21,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  @ 21,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  @ 21,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  @ 21,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
ENDIF

IF !EMPTY(lcScale5)
  @ 22,22 SAY lcScale5
  XSCALE = GETSCALE(lcScale5,SPACE(1))
  @ 22,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  @ 22,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  @ 22,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  @ 22,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  @ 22,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  @ 22,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  @ 22,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  @ 22,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
ENDIF

*--End of lpHrdLabls.
*!*************************************************************
*! Name : lpOldFoter.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the form footer at in the middle of a spacific
*!            pick ticket.
*!*************************************************************
*! Called from : 
*!*************************************************************
PROCEDURE lpOldFoter

@ 54,93 SAY XSTNAME
@ 55,93 SAY XSTADDR1
@ 56,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 56,93 SAY XSTADDR2
@ 57,93 SAY XSTADDR3
ROW = ROW + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name : lpNewFoter.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the form footer at the end of a spacific
*!            pick ticket.
*!*************************************************************
*! Called from : 
*!*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the
*-- user does not want to print the style prices.
@ 53,00 SAY lcRpMsg2
@ 54,00 SAY lcRpMsg3
@ 55,93 SAY XSTNAME

*-- note to prevent printing any one of them if it has an '*'
*-- in front of it.
lnCol = 11
@ 56,lnCol SAY OrdHdr.Note1
lnCol = 42
@ 56,lnCol SAY OrdHdr.Note2
@ 56,093 SAY XSTADDR1
@ 57,093 SAY XSTADDR2
@ 58,093 SAY XSTADDR3
@ 59,001 SAY XPIKTKT
@ 59,009 SAY XDATE
@ 59,072 SAY lnTotQty PICTURE '999999.999'
@ 62,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)
@ 62,100 SAY XSTORE
@ 62,109 SAY ORDHDR.DEPT
@ 62,115 SAY XORDER
@ 63,101 SAY XPIKTKT
ROW = ROW + 1

*--End of lpNewFoter.
*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 11/07/2001
*! Purpose   : get the address of the customer
*!**************************************************************************
*! Example   : = lfGetAddr()
*!**************************************************************************
FUNCTION lfGetAddr
PARAMETER llDummy

lcCurrAlis = ALIAS()
laSoldTo = ''
laShipTo = ''

=SEEK(IIF(EMPTY(PIKTKT.Store) , 'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store),'Customer')

lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)), 'SHIPVIA')

lcSolTName = CUSTOMER.BTName

= SEEK(IIF(EMPTY(PIKTKT.Store) ,'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store) ,'Customer')

IF Customer.Type = 'S' AND Customer.billto = 'M'
  GO TOP IN 'Customer'
  = SEEK('M'+ PIKTKT.Account ,'Customer')
  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , '2')
ELSE
  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , '2')
ENDIF

IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE
  = SEEK(IIF(EMPTY(PIKTKT.Store) ,'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store) ,'Customer')
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    =SEEK('S' + PIKTKT.Account + lcDistCntr)
  ELSE
    =SEEK(IIF(EMPTY(PIKTKT.Store) , 'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store))
  ENDIF

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
ENDIF
SELECT (lcCurrAlis)
RETURN ''

*-- End of lfGetAddr.
*!***************************************************************
*! Name : lpPrtSku.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!***************************************************************
PROCEDURE lpPrtSku
IF !SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = " "
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M' + ORDHDR.Account , 'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
    nDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  = SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = SUBSTR(Pack_Id,1,lnDime1)
ELSE
  lcStrToPrn = SUBSTR(Sku,1,8)
ENDIF
RETURN

*--End of lpPrtSku.



*B605658,1 BWA 03/24/2002 Comment all the old code.[START]
*-- Set the memory variables.

*PRIVATE lcStrToPrn
*lcStrToPrn = ""
*SELECT &lcTmpOrdL
*GO TOP
*IF EOF()
*  llNoRec = .T.
*  =gfModalGen('TRM00052B00000','DIALOG' )
*  SET DEVICE TO SCREEN
*  RETURN
*ENDIF

*DECLARE laDisRltFld[1,2]
*laDisRltFld[1,1] = 'DIVLNAME'      && Array to get the Division long name
*laDisRltFld[1,2] = 'lcDivLName'

*DECLARE laSoldTo[5,1] , laShipTo[5,1]
*laSoldTo = ''           && Array to hold the Sold To address
*laShipTo = ''           && Array to hold the Ship To address

*PRIVATE HLine1,HLine2,HLine3,HLine4
*STORE '' TO HLine1,HLine2,HLine3,HLine4

*HLINE1 = lcCompName
*HLINE2 = TRIM(laCompAdd[1])
*HLINE3 = TRIM(laCompAdd[2])
*HLINE4 = TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5]

*IF LEN(HLINE3) = 0
*  HLINE3=HLINE4
*ENDIF

*SELECT &lcTmpOrdL

*-- MAIN LOOP
*llEndPT = .F.
*NEWDOC  = .T.
*lnNotLine = 1
*lnTotQty  = 0
*XVALUE    = 0
*WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
*SET DEVICE TO PRINT
*SELECT &lcTmpOrdL
*DO WHILE !EOF() .AND. INKEY() <>32
*  SELECT &lcTmpOrdL
*  IF NEWDOC
*     SELECT &lcTmpOrdL
*     XPIKTKT  = PIKTKT
*     XORDER   = ORDER
*     XDATE    = PikDATE
*     XSTORE   = STORE
*     lnTotQty = 0
*     SELECT ORDHDR
*     XMULTI   = MULTI
*     XACCOUNT = ACCOUNT
*     STORE 0.00 TO XORDTOT, XVALUE , SVALUE
*     *-- We added the STORE# to use the new created index (ORDLINST) which
*     *-- consists of ORDER+STORE+STYLE+COLOR+STR(LINENO,6) in order to make
*     *-- the search faster because one PIKTKT can't exceed one STORE. This
*     *-- will work both form single or multi store orders. In single store
*     *-- order, either the store will be empty or have something and all
*     *-- the lines will have the same store. So this change will not improve
*     *-- anything on the single store orders but it will on big multistore
*     *-- orders. And in most cases the single store orders are not that big
*     *-- anyway.
*     SELECT ORDLINE
*     SET ORDER TO ordlinst
*     = SEEK("O" + XORDER+XSTORE)
*     IF ORDHDR.MultiPO 
*       lcCustPO=OrdLine.CustPO
*     ENDIF
*     IF PIKTKT <> XPIKTKT
*        LOCATE REST FOR   PIKTKT=XPIKTKT ;
*                    WHILE ORDER+STORE=XORDER+XSTORE
*     ENDIF
*     *IF ORDER+STORE <> XORDER+XSTORE 
*        *SELECT &lcTmpOrdL
*        *SKIP
*        *LOOP
*     *ENDIF
*     SET DEVICE TO PRINT
*     GO TOP
*     =lfGetAddr()
*     XBTNAME  = lcSolTName
*     XBTADDR1 = laSoldTo[1]
*     XBTADDR2 = laSoldTo[2]
*     XBTADDR3 = ALLTRIM(laSoldTo[3]) + ' ' +ALLTRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
*     IF LEN(TRIM(XBTADDR2)) =0
*       XBTADDR2 = XBTADDR3
*       XBTADDR3 = ''
*     ENDIF
     *-- GET DESCRIPTIONS FOR CODED FIELDS

     *------------------------------------
     * GET DESCRIPTIONS FOR CODED FIELDS
     *------------------------------------
*     XSEAS_DATA = SUBSTR(gfCodDes(ORDHDR.Season , 'SEASON'),1,7)
*     XTERM_DATA = gfCodDes(ORDHDR.ctermcode,'cTermCode')
*     XSHIP_DATA = IIF(ORDHDR.ShipVia ='*',gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'),gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'))
*     XSPCI_DATA = gfCodDes(ORDHDR.SPCINST , 'SPCINST   ')
*     XDIVI_DATA = SUBSTR(gfCodDes(ORDHDR.CDivision , 'CDIVISION '),1,7)
*     IF !EMPTY(XDIVI_DATA)
*       =gfRltFld(OrdHdr.cDivision,@laDisRltFld,'CDIVISION')
*       HLINE1 = laDisRltFld[1,2]
*     ENDIF

*     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
*     XPIECES = 0
     *--------------------------------------------
     * SHIP-TO ADDRESS FOR THIS STORE
     *--------------------------------------------
*     XSTNAME  = lcShpTName
*     XSTADDR1 = laShipTo[1]
*     XSTADDR2 = laShipTo[2]
*     XSTADDR3 = ALLTRIM(laShipTo[3]) + ' ' +ALLTRIM(laShipTo[4]) + ' ' + laShipTo[5]
     
*     IF LEN(TRIM(XSTADDR2)) =0
*       XSTADDR2 = XSTADDR3
*       XSTADDR3 = ''
*     ENDIF

     *-- GET THE SIZE SCALES
     *-- Moved and enhanced to get the printed scales
     *-- once outside the loop not every time in the loop because it will 
     *-- be the same

*     XSCALE2   = SPACE(1)
*     lcScale3  = SPACE(1)
*     lcScale4  = SPACE(1)
*     lcScale5  = SPACE(1)

*     SELECT &lcTmpOrdL
*     lnRecNo = RECNO()
*     XSCALE1 = SCALE
*     LOCATE REST FOR SCALE <> XSCALE1
*     IF !EOF()
*       XSCALE2 = SCALE
*     ENDIF
*     GO TOP 

*     LOCATE REST FOR SCALE <> XSCALE1 .AND. SCALE <> XSCALE2
*     IF !EOF()
*       lcScale3 = SCALE
*     ENDIF
*     GO TOP 

*     LOCATE REST FOR SCALE <> XSCALE1 .AND. SCALE <> XSCALE2 .AND. SCALE <> lcScale3
*     IF !EOF()
*       lcScale4 = SCALE
*     ENDIF
*     GO TOP 
*     LOCATE REST FOR SCALE <> XSCALE1  AND SCALE <> XSCALE2 AND ;
*                     SCALE <> lcScale3 AND SCALE <> lcScale4
*     IF !EOF()
*       lcScale5 = SCALE
*     ENDIF
*     GO TOP 
*     GOTO lnRecNo
*  ENDIF
  
*  SELECT ORDHDR
  
  *-- START PRINTING
*  DO lpHrdLabls
*  @ 23,93 SAY XSTNAME
  *-- LINE LOOP
*  SELECT &lcTmpOrdL
*  NEWDOC  = .T.
*  XTOTQTY = 0
*  ROW     = 24
*  SCAN REST WHILE PikTkt + Order + cGrupDetal + STR(LineNo , 6) = ;
*                  XPIKTKT + XORDER FOR  LINENO <> 0

*    *DO WHILE .T.
*      *DO CASE
*        *CASE EOF()
*          *NEWDOC = .T.
*          *EXIT
*        *CASE TOTPIK <= 0
*          *SKIP
*          *LOOP
*        *CASE ROW >= 47
*          *NEWDOC = .F.
*          *EXIT
*      *ENDCASE

      *-- Modified to get the Style/Color location.     
*      SELECT Style
*      IF SEEK(&lcTmpOrdL..Style)
*        XSTYDESC   = DESC
*        lcStyLocat = Location
*      ELSE
*        XSTYDESC   = ''
*        lcStyLocat = ''
*      ENDIF
    
      *-- To get the color description.

*      lcClrDesc = PADR(ALLTRIM(gfCodDes(SUBSTR(Style.Style,14,6),"COLOR")),30) 
*      SELECT &lcTmpOrdL
*      @ ROW,01 SAY STYLE
*      @ ROW,22 SAY SCALE
*      @ ROW,24 SAY PIK1   PICTURE '@Z 9999'
*      @ ROW,28 SAY PIK2   PICTURE '@Z 9999'
*      @ ROW,32 SAY PIK3   PICTURE '@Z 9999'
*      @ ROW,36 SAY PIK4   PICTURE '@Z 9999'
*      @ ROW,40 SAY PIK5   PICTURE '@Z 9999'
*      @ ROW,44 SAY PIK6   PICTURE '@Z 9999'
*      @ ROW,48 SAY PIK7   PICTURE '@Z 9999'
*      @ ROW,52 SAY PIK8   PICTURE '@Z 9999'
*      @ ROW,56 SAY TOTPIK PICTURE '99999'
*      lnTotQty = lnTotQty + TOTPIK*PRICE
*      IF llRpStyPrc
*        @ ROW,63 SAY PRICE PICTURE '9999.99'
*        @ ROW,72 SAY TOTPIK*PRICE PICTURE '9999999.999'
*      ENDIF
*      DO lpSidLabls
*      ROW = ROW + 1
*      @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
      *-- To print the color description, And the 
      *-- Style/Color location.
*      IF llRpPrtClr .OR. llRpStyLoc
*        @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.:' + lcClrDesc, '')
*        IF llRpPrtClr
*          @ ROW,59 SAY IIF(llRpStyLoc .AND. !EMPTY(lcStyLocat),'LOCATION:'+lcStyLocat, '')
*        ELSE
*          @ ROW,34 SAY IIF(llRpStyLoc.AND.!EMPTY(lcStyLocat),'LOCATION:'+lcStyLocat, '')      
*        ENDIF
*      ENDIF
      *--Print the bar code on the side label.
*      IF ROW = 39
*        *DO lpBarCode WITH lcCompNo,lcPicktktno,lcZipcode,1
*      ELSE
*        DO lpSidLabls
*      ENDIF
*      Row = Row + 1
*      *--  SKU PRINTING FOR ACCOUNTS THAT HAVE SKU'S ON FILE.
*      DO lpPrtSku
*      @ ROW,00 SAY lcStrToPrn + '  CUSTOMER SKU #'
*      DO lpSidLabls
*      ROW = ROW + 1
*      SELECT &lcTmpOrdL
*      * SUM ORDER TOTALS
*      XTOTQTY = XTOTQTY+ TOTQTY
*      XVALUE  = XVALUE + TOTPIK * PRICE
*      XPIECES = XPIECES + TOTPIK
*      SELECT &lcTmpOrdL
*      DO CASE
*        CASE EOF()
*          NEWDOC = .T.
*          EXIT
*        CASE TOTPIK <= 0
*          SKIP                              
*          LOOP
*        CASE ROW >= 47
*          NEWDOC = .F.
*          EXIT
*      ENDCASE
*    ENDSCAN
*    *SELECT &lcTmpOrdL
*    *SKIP                        
*  *ENDDO                         
*  *-- END PRINT LINE LOOP
*  SELECT &lcTmpOrdL
*  SKIP -1
*  IF NewDoc
*    *-- To print the notepad.
*    IF llRpOrdNot
*      SELECT NotePad
*      IF SEEK('B' + OrdHdr.Order)
*        lnOldMemW = SET("MEMOWIDTH")
*        SET MEMOWIDTH TO 75
*        lnMemLins = MEMLINES(NOTEPAD.MNOTES)
*        @ Row,00 SAY '* -- N O T E S -- *' 
*        DO lpSidLabls
*        Row = Row + 1
*        DO WHILE lnNotLine <= lnMemLins
*          IF Row >= 53
*            DO lpOldFoter
*            DO lpHrdLabls
*            Row = 23
*          ENDIF
*          @ ROW,00 SAY MLINE(MNOTES,lnNotLine)
*          DO lpSidLabls
*          ROW = ROW + 1
*          lnNotLine = lnNotLine + 1
*        ENDDO
*        SET MEMOWIDTH TO lnOldMemW
*      ENDIF
*    ENDIF  
*    lnNotLine = 1
*    llEndPT = .T.
*    DO lpFilEmpLn        
*    DO lpNewFoter        
*  ELSE
*    llEndPT = .F.
*    DO lpFilEmpLn   
*    DO lpOldFoter   
*    LOOP
*  ENDIF
***  SELECT &lcTmpOrdL
***  SKIP
*  SELECT PIKTKT
*  SEEK &lcTmpOrdL..PIKTKT
*  REPLACE PRTFLAG WITH 'P'

*  SELECT &lcTmpOrdL
*  IF !EOF()
*    SKIP
*  ENDIF
*  **IF RECNO() == RECCOUNT()
*    **SKIP
*  **ENDIF
*ENDDO

*!*************************************************************
*! Name : lpFilEmpLn.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Fill the empty pick tiKit lines.
*!*************************************************************
*! Called from : 
*!*************************************************************
*PROCEDURE lpFilEmpLn
*DO WHILE Row <= 53
*  DO lpSidLabls 
*  Row = Row + 1
*ENDDO

*!*************************************************************
*! Name : lpSidLabls.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the Side labels.
*!*************************************************************
*! Called from : 
*!*************************************************************
*PROCEDURE lpSidLabls
*DO CASE
*  CASE ROW = 34 .OR. ROW = 50
*    @ ROW,93 SAY IIF(llPrntComp,HLINE1,'')
*  CASE ROW = 35 .OR. ROW = 51
*    @ ROW,93 SAY IIF(llPrntComp,HLINE2,'')
*  CASE ROW = 36 .OR. ROW = 52
*     @ ROW,93 SAY IIF(llPrntComp,HLINE3,'')
*  CASE ROW = 37 .OR. ROW = 53
*    IF llEndPT .AND. Row = 53 .AND. NewDoc
*      @ 53,000 SAY lcRpMsg1                    
*    ENDIF  
*    @ ROW,93 SAY IIF(llPrntComp,HLINE4,'')
*  CASE ROW = 23 .OR. ROW = 40
*    @ ROW,93 SAY XSTNAME
*  CASE ROW = 24 .OR. ROW = 41
*    @ ROW,93 SAY XSTADDR1
*  CASE ROW = 25 .OR. ROW = 42
*    @ ROW,93 SAY XSTADDR2
*  CASE ROW = 26 .OR. ROW = 43
*    @ ROW,93 SAY XSTADDR3
*  CASE ROW = 30 .OR. ROW = 46
*    @ ROW,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)
*    @ ROW,100 SAY XSTORE
*    @ ROW,109 SAY ORDHDR.DEPT
*    @ ROW,115 SAY XORDER
*  CASE ROW = 31 .OR. ROW = 48
*    @ ROW,101 SAY XPIKTKT
*ENDCASE

*!*************************************************************
*! Name : lpHrdLabls.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the Header labels.
*!*************************************************************
*! Called from : 
*!         Procedures : Alo820E.
*!*************************************************************
*PROCEDURE lpHrdLabls

*-- Line 0
*@ 02,001 SAY IIF(llPrntComp,HLINE1,'')
*@ 02,095 SAY IIF(llPrntComp,HLINE1,'') 
*-- Line 1
*@ 03,001 SAY IIF(llPrntComp,HLINE2,'')
*@ 03,095 SAY IIF(llPrntComp,HLINE2,'')
 
*-- Line 2
*@ 04,001 SAY IIF(llPrntComp,HLINE3,'')
*@ 04,095 SAY IIF(llPrntComp,HLINE3,'')
  
*-- Line 3
*@ 05,001 SAY IIF(llPrntComp,HLINE4,'')
*@ 05,060 SAY XDIVI_DATA
*@ 05,095 SAY IIF(llPrntComp,HLINE4,'')
  
*@ 07,060 SAY ORDHDR.APPROVAL
*@ 07,095 SAY XSTNAME
  
*-- Line 5
*@ 08,095 SAY XSTADDR1
 
*-- Line 6
*@ 09,095 SAY XSTADDR2
 
*-- Line 7
*@ 10,010 SAY XBTNAME
*@ 10,048 SAY XSTNAME
*@ 10,095 SAY XSTADDR3
 
*-- Line 8
*@ 11,010 SAY XBTADDR1
*@ 11,048 SAY XSTADDR1

*-- Line 9
*@ 12,010 SAY XBTADDR2
*@ 12,048 SAY XSTADDR2
  
*-- Line 10
*@ 13,010 SAY XBTADDR3
*@ 13,048 SAY XSTADDR3
*@ 13,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)
*@ 13,100 SAY XSTORE
*@ 13,109 SAY ORDHDR.DEPT
*@ 13,119 SAY XORDER
  
*-- Line 11
*@ 15,101 SAY XPIKTKT
  
*-- Line 13
*@ 16,093 SAY IIF(llPrntComp,HLINE1,'')

*-- Line 15
*@ 17,001 SAY XACCOUNT
*@ 17,009 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  PICTURE '########'
*@ 17,020 SAY XSTORE       PICTURE '#########'
*@ 17,030 SAY ORDHDR.DEPT PICTURE '#####'
*@ 17,037 SAY XORDER
*@ 17,044 SAY SUBSTR(XTERM_DATA,1,14)
*@ 17,060 SAY ORDHDR.REP1
*@ 17,064 SAY ORDHDR.REP2
*@ 17,068 SAY SUBSTR(XSHIP_DATA,1,14)
*@ 17,093 SAY IIF(llPrntComp,HLINE2,'')

*@ 18,093 SAY IIF(llPrntComp,HLINE3,'')

*-- Line 17 PRINT 1 ST SIZE SCALE
*@ 19,22 SAY XSCALE1
*XSCALE = GETSCALE( XSCALE1,SPACE(1))

*@ 19,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*@ 19,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*@ 19,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*@ 19,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*@ 19,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*@ 19,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
*@ 19,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
*@ 19,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
  
*-- Line 16
*@ 19,93 SAY IIF(llPrntComp,HLINE4,'')

*-- Line 18 PRINT 2ND. SIZE SCALE
*IF !EMPTY(XSCALE2)
*  @ 20,22 SAY XSCALE2
*  XSCALE = GETSCALE(XSCALE2,SPACE(1))
*  @ 20,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*  @ 20,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*  @ 20,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*  @ 20,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*  @ 20,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*  @ 20,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
*  @ 20,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
*  @ 20,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
*ENDIF

*IF !EMPTY(lcScale3)
*  @ 21,22 SAY lcScale3
*  XSCALE = GETSCALE(lcScale3,SPACE(1))
*  @ 21,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*  @ 21,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*  @ 21,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*  @ 21,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*  @ 21,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*  @ 21,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
*  @ 21,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
*  @ 21,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
*ENDIF

*IF !EMPTY(lcScale4)
*  @ 22,22 SAY lcScale4
*  XSCALE = GETSCALE(lcScale4,SPACE(1))
*  @ 22,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*  @ 22,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*  @ 22,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*  @ 22,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*  @ 22,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*  @ 22,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
*  @ 22,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
*  @ 22,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
*ENDIF

*IF !EMPTY(lcScale5)
*  @ 23,22 SAY lcScale5
*  XSCALE = GETSCALE(lcScale5,SPACE(1))
*  @ 23,24 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*  @ 23,28 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*  @ 23,32 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*  @ 23,36 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*  @ 23,40 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*  @ 23,44 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
*  @ 23,48 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
*  @ 23,52 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
*ENDIF

*!*************************************************************
*! Name : lpOldFoter.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the form footer at in the middle of a spacific 
*!            pick ticket. 
*!*************************************************************
*! Called from : 
*!*************************************************************
*PROCEDURE lpOldFoter

*@ 55,93 SAY XSTNAME
*@ 56,93 SAY XSTADDR1
*@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
*@ 57,93 SAY XSTADDR2
*@ 58,93 SAY XSTADDR3
*ROW = ROW + 1

*!*************************************************************
*! Name : lpNewFoter.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the form footer at the end of a spacific 
*!            pick ticket. 
*!*************************************************************
*! Called from : 
*!*************************************************************
*PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
*@ 54,000 SAY lcRpMsg2
*@ 55,000 SAY lcRpMsg3
*@ 56,093 SAY XSTNAME

*-- note to prevent printing any one of them if it has an '*'
*-- in front of it.
*lnCol = 11
*@ 57,lnCol SAY OrdHdr.Note1
*lnCol = 42
*@ 57,lnCol SAY OrdHdr.Note2
*@ 57,093 SAY XSTADDR1
*@ 58,093 SAY XSTADDR2
*@ 59,093 SAY XSTADDR3
*@ 60,001 SAY XPIKTKT
*@ 60,009 SAY XDATE
*@ 60,072 SAY lnTotQty PICTURE '999999.999' 
*@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  
*@ 63,100 SAY XSTORE                  
*@ 63,109 SAY ORDHDR.DEPT            
*@ 63,115 SAY XORDER
*@ 64,101 SAY XPIKTKT
*ROW = ROW + 1

*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 11/07/2001
*! Purpose   : get the address of the customer
*!**************************************************************************
*! Example   : = lfGetAddr()
*!**************************************************************************
*
*FUNCTION lfGetAddr
*PARAMETER llDummy

*lcCurrAlis = ALIAS()
*laSoldTo = ''
*laShipTo = ''

*=SEEK(IIF(EMPTY(PIKTKT.Store) , 'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store),'Customer')

*lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
*                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
*                         CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)), 'SHIPVIA')

*lcSolTName = CUSTOMER.BTName

*= SEEK(IIF(EMPTY(PIKTKT.Store) ,'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store) ,'Customer')

*IF Customer.Type = 'S' AND Customer.billto = 'M'
*  GO TOP IN 'Customer'
*  = SEEK('M'+ PIKTKT.Account ,'Customer')
*  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , '2')
*ELSE
*  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , '2')
*ENDIF

*IF ORDHDR.Alt_ShpTo
*  SELECT ORDHDR
*  lcShpTName = STName
*  laShipTo[1] = cAddress1
*  laShipTo[2] = cAddress2
*  laShipTo[3] = cAddress3
*  laShipTo[4] = cAddress4
*  laShipTo[5] = cAddress5
*ELSE    && Else
*  = SEEK(IIF(EMPTY(PIKTKT.Store) ,'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store) ,'Customer')
*  SELECT CUSTOMER
*  lcDistCntr = CUSTOMER.Dist_Ctr
*  *--If there is a distribution center
*  IF !EMPTY(lcDistCntr)
*    =SEEK('S' + PIKTKT.Account + lcDistCntr)
*  ELSE
*    =SEEK(IIF(EMPTY(PIKTKT.Store) , 'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store))
*  ENDIF
*  
*  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
*  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
*  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
*  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
*  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
*  *laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(Customer.Phone1 , lcPhonPict)
*  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
*ENDIF    && End of IF
*SELECT (lcCurrAlis)
*RETURN ''
*-- End of lfGetAddr

*!***************************************************************
*! Name : lpPrtSku.
*: DATE : 07/11/2001
*: Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : 
*!         Program : alpktkog
*!***************************************************************

*PROCEDURE lpPrtSku
*IF !SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
*  lcStrToPrn = " "
*  RETURN
*ENDIF

*SELECT Spck_Lin
*IF EMPTY(Sku)
*  lnI = 1
*  = SEEK('S'+Style.Scale,'Scale')
*  = SEEK('M' + ORDHDR.Account , 'Customer')
*  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
*  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
*    lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
*    nDime2 = SkuTmpl.Len4
*  ELSE
*    lnDime1 = 8  &&Default
*    lnDime2 = 8  &&Default
*  ENDIF 
*  = SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
*  *lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)
*  lcStrToPrn = SUBSTR(Pack_Id,1,lnDime1)
*ELSE
*  lcStrToPrn = SUBSTR(Sku,1,8)
*  *lnLocCol = 35
*  *@ ROW,17 SAY 'CUSTOMER SKU #'
*ENDIF
*RETURN

*B605658,1 BWA 03/24/2002 Move all the piktkt one line up and fix.[END]