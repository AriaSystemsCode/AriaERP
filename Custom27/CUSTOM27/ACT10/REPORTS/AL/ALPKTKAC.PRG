\*:***************************************************************************
*: Program file  : ALPKTKAC.PRG
*: Program desc. : CUSTOMIZED SALES ORDER ALLOCATION PICKING TICKET Form FOR ACT10
*: Date          : 07/28/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: B606174
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
*: Example : DO ALPKTKAC
*:***************************************************************************
*: Modification:
*****************************************************************************

           *--Section of Initializing the variables --*
STORE SPACE(1) TO lcStrToPrn , lcSkuSize , lcStr
STORE .F. TO llNoRec                && Variable used to make the endreport function in the main program.
STORE .T. TO NEWDOC , llEndPT
lnNotLine = 1
LNLEN     = 0
STORE 0.00 TO XORDTOT, XVALUE , SVALUE

STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4

IF LEN(TRIM(HLine3)) = 0
  HLine3 = HLine4
  HLine4 = SPACE(1)
ENDIF

*--If EOF it will return to the main program
IF EOF(lcTmpOrdL)
  llNoRec = .T.
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

lcSpcOrd = ORDER("SPCK_LIN")
lcWhsOrd = ORDER("WHSLOC") 
SET ORDER TO TAG WHSLOCST IN WHSLOC
SET ORDER TO TAG SPCKLINS IN SPCK_LIN

SELECT (lcTmpOrdL)
SCAN

  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
    XPIKTKT  = PIKTKT
    XORDER   = ORDER
    XDATE    = PIKTKT.DATE
    XSTORE   = PIKTKT.Store

    SELECT ORDHDR
    =SEEK('O'+XORDER,'ORDHDR')
    XMULTI   = MULTI
    XACCOUNT = ORDHDR.ACCOUNT
    STORE 0.00 TO XORDTOT, XVALUE , SVALUE
    lcCustPO = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)

    SELECT CUSTOMER
    XBTNAME  = CUSTOMER.BTNAME
    XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
    XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
    XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')

    IF LEN(TRIM(XBTADDR2)) = 0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF

    *-- GET DESCRIPTIONS FOR CODED FIELDS
    SELECT CODES
    SET ORDER TO CODES IN CODES
    lcTerm = gfCodDes(ORDHDR.CTERMCODE , 'CTERMCODE')
    lcShip = gfCodDes(ORDHDR.SHIPVIA , 'SHIPVIA')
    lcDivi  = gfCodDes(ORDHDR.CDIVISION , 'CDIVISION')

    HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
    STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
    XPIECES = 0

    *-- GET THE SIZE SCALES.
    XSCALE2 = SPACE(1)
    SELECT (lcTmpOrdL)
    lcKey   = EVAL(KEY())   && Variable to hold the record number.
    XSCALE1 = SCALE
    LOCATE REST FOR ( SCALE <> XSCALE1 .AND. ORDER = XORDER  .AND. !EMPTY(SCALE) )
    IF !EOF()
      XSCALE2 = LEFT(SCALE,1)
    ENDIF
    =SEEK(lcKey)  && Back to original record number.

  ENDIF
  *-- END PICK TKT/ORDER SELECTION.
  *-- SHIP-TO ADDRESS FOR THIS STORE
  *- Initialize the alt address if ther is any.
  SELECT ORDHDR

  lcDistCntr = ""
  IF Alt_ShpTo
    lcShpTName  = OrdHdr.STName
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3
    laShipTo[4] = OrdHdr.cAddress4
    laShipTo[5] = OrdHdr.cAddress5

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' +','+ laShipTo[5]

    IF LEN(TRIM(XSTADDR2)) = 0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT CUSTOMER
    lnRecNo = RECNO()
    IF !EMPTY(Dist_Ctr) AND llPrnDc
      lcDistCntr = ALLTRIM(Dist_Ctr)
      =seek('S' + XACCOUNT + lcDistCntr)
    ENDIF

    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )

    IF LEN(TRIM(XSTADDR2)) = 0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  SELECT ORDHDR
  *-- START PRINTING
  DO lpHrdLabls

  *-- LINE LOOP
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0
  ROW     = 23

  DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT  .AND. LINENO <> 0
    WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT
    IF Row >= 53
      DO lpOldFoter
      DO lpHrdLabls
      Row = 23
    ENDIF

    SELECT STYLE
    IF SEEK(&lcTmpOrdL..STYLE)
      XSTYDESC   = SUBSTR(DESC,1,15)
      lcStyLocat = Location
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
    ENDIF

    LCCLRDESC = LEFT(gfCodDes(SUBSTR(STYLE.STYLE,14,6) ,'COLOR'),15)
    SELECT (lcTmpOrdL)
    @ ROW,00 SAY SUBSTR(STYLE, 1, 7 )
    @ ROW, 7 SAY SUBSTR(STYLE,lnClrPosM1,3)
    @ ROW,11 SAY XSTYDESC
    @ ROW,29 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,33 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,37 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,41 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,45 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,49 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,57 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,61 SAY TOTPIK PICTURE '@Z 99999'

    IF LLRPSTYPRC
      @ ROW,66 SAY PRICE PICTURE '9999.99'
    ENDIF
    @ ROW,75 SAY TOTPIK PICTURE '9999'
    DO lpSidLabls
    ROW = ROW + 1

    IF !llRpSkuBck
      @ ROW,00 SAY 'STYLE DESC.:' + XStyDesc
      @ ROW, 33 SAY IIF(llRpPrtClr,'CLR DESC.:' + LCCLRDESC,'')
      IF llRpStyLoc
        =lfGetLoc()
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,15)
          DO lpSidLabls
          Row = Row + 1
          IF LEN(lcStr) > 18
            =lfContuLoc()
          ENDIF
        ELSE
          DO lpSidLabls
          Row = Row + 1
        ENDIF
      ENDIF
    ELSE
      IF SEEK('P'+ORDHDR.Account + &lcTmpOrdL..STYLE , 'SPCK_LIN')
        @ ROW,00 SAY 'PACK ID#:'+SPCK_LIN.Pack_Id
        @ ROW, 25 SAY 'STYLE DESC.:'+XStyDesc
        @ ROW, 59 SAY IIF(llRpPrtClr,'CLR DESC.:'+SUBSTR(LCCLRDESC,1,10),'')
        DO lpSidLabls
        Row = Row + 1
      ELSE
        DO lpPrtSku      && Procedure to print the STYLE/Color Sku no.
        IF !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,40)
          @ ROW,41 SAY 'STYLE DESC.:'+XStyDesc
          @ ROW, 73 SAY IIF(llRpPrtClr,'CLR DESC.:'+SUBSTR(LCCLRDESC,1,3),'')
          DO lpSidLabls
          Row = Row + 1
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,00 SAY 'LOCATION:'+ SUBSTR(lcStr,1,85)
              DO lpSidLabls
              Row = Row + 1
              IF LEN(lcStr) > 85
                =lfContuLoc()
              ENDIF
            ELSE
              DO lpSidLabls
              Row = Row + 1
            ENDIF
          ENDIF
          DO lpSidLabls
          Row = Row + 1
        ELSE
          @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
          @ ROW, 33 SAY IIF(llRpPrtClr,'CLR DESC.:'+LCCLRDESC,'')
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,15)
              DO lpSidLabls
              Row = Row + 1
              IF LEN(lcStr) > 18
                =lfContuLoc()
              ENDIF
            ELSE
              DO lpSidLabls
              Row = Row + 1
            ENDIF
          ENDIF
          =lfPrnStrn()
          lcSkuSize = lcStrToPrn + ' ' + lcSkuSize
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,85)
          DO lpSidLabls
          Row = Row + 1
          =lfPrnStrn()
        ENDIF
      ENDIF
    ENDIF

    *-- To print the color description, And the Style/Color location.
    DO lpSidLabls
    Row = Row + 1

    *--SUM ORDER TOTALS
    SELECT (lcTmpOrdL)
    XTOTQTY = XTOTQTY + TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK

    SKIP
  ENDDO

  IF (XORDER # &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) ) OR (XORDER = &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) AND XPIKTKT # &lcTmpOrdL..PIKTKT )
    SKIP - 1
  ENDIF

  *-- To print the notepad.
  IF NewDoc
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
          IF Row >= 53
            DO lpOldFoter
            DO lpHrdLabls
            Row = 23
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
  IF PIKTKT = "******" .AND. LINENO <> 0 .AND. !EOF()
    SKIP -1
  ENDIF
ENDSCAN
SET ORDER TO &lcSpcOrd IN SPCK_LIN
SET ORDER TO &lcWhsOrd IN WHSLOC

SET DEVICE TO SCREEN
WAIT CLEAR
RETURN

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpFilEmpLn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : Fill the empty pick ticket lines.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
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

DO WHILE Row <= 53
  DO lpSidLabls
  Row = Row + 1
ENDDO

*--End of lpFilEmpLn.
*!*************************************************************
*! Name      : lpSidLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : Print the Side labels.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
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
    @ ROW,93 SAY IIF(llPrntComp , HLINE1 , '')
  CASE ROW = 35 .OR. ROW = 51
    @ ROW,93 SAY IIF(llPrntComp , HLINE2 , '')
  CASE ROW = 36 .OR. ROW = 52
    @ ROW,93 SAY IIF(llPrntComp , HLINE3 , '')
  CASE ROW = 37 .OR. ROW = 53
    IF llEndPT .AND. Row = 53 .AND. NewDoc
      @ 53,000 SAY lcRpMsg1
    ENDIF
    @ ROW,93 SAY IIF(llPrntComp , HLINE4 , '')
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,93 SAY XSTNAME
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,93 SAY XSTADDR1
  CASE ROW = 25 .OR. ROW = 42
    @ ROW,93 SAY XSTADDR2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW,93 SAY XSTADDR3
  CASE ROW = 29 .OR. ROW = 45
    @ ROW,086 SAY LEFT(lcCustPO,13)
    @ ROW,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
    @ ROW,109 SAY ORDHDR.DEPT
    @ ROW,115 SAY XORDER
  CASE ROW = 30 .OR. ROW = 46
    @ ROW,101 SAY XPIKTKT
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(lcShip,1,10)
ENDCASE

*--End of lpSidLabls.
*!*************************************************************
*! Name      : lpHrdLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
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

*-- Line 0
@ 02,001 SAY IIF(llPrntComp , HLINE1 , '')
@ 02,095 SAY IIF(llPrntComp , HLINE1 , '')

*-- Line 1
@ 03,001 SAY IIF(llPrntComp , HLINE2 , '')
@ 03,095 SAY IIF(llPrntComp , HLINE2 , '')

*-- Line 2
@ 04,001 SAY IIF(llPrntComp , HLINE3 , '')
@ 04,095 SAY IIF(llPrntComp , HLINE3 , '')

*-- Line 3
@ 05,001 SAY IIF(llPrntComp , HLINE4 , '')
@ 05,060 SAY SUBSTR(ALLTRIM(lcDivi),1,8)
@ 05,073 SAY XPIKTKT
@ 05,095 SAY IIF(llPrntComp , HLINE4 , '')

*-- Line 4
@ 07,060 SAY ORDHDR.APPROVAL
@ 07,073 SAY XDATE
@ 07,095 SAY XSTNAME

*-- Line 5
@ 08,095 SAY XSTADDR1

*-- Line 6
@ 09,095 SAY XSTADDR2

*-- Line 7
@ 10,005 SAY XBTNAME
@ 10,048 SAY XSTNAME
@ 10,095 SAY XSTADDR3

*-- Line 8
@ 11,005 SAY XBTADDR1
@ 11,048 SAY XSTADDR1

*-- Line 9
@ 12,005 SAY XBTADDR2
@ 12,048 SAY XSTADDR2

*-- Line 10
@ 13,005 SAY XBTADDR3
@ 13,048 SAY XSTADDR3
@ 13,086 SAY LEFT(lcCustPO,13)
@ 13,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 13,109 SAY ORDHDR.DEPT
@ 13,119 SAY XORDER

*-- Line 11
@ 14,101 SAY XPIKTKT
@ 14,108 SAY 'SHIP VIA: ' + SUBSTR(lcShip,1,10)

*-- Line 13
@ 17,093 SAY IIF(llPrntComp , HLINE1 , '')

*-- Line 15
@ 18,001 SAY XACCOUNT
@ 18,012 SAY XORDER
@ 18,023 SAY ORDHDR.REP1
@ 18,027 SAY ORDHDR.REP2
@ 18,031 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)
@ 18,039 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 18,045 SAY ORDHDR.COMPLETE
@ 18,054 SAY SUBSTR(lcTerm,1,11)
@ 18,066 SAY XSTORE         PICTURE '####'
@ 18,070 SAY lcCustPO       PICTURE '########'
@ 18,078 SAY ORDHDR.DEPT
@ 18,093 SAY IIF(llPrntComp , HLINE2 , '')

@ 19,093 SAY IIF(llPrntComp , HLINE3 , '')

*-- Line 17 PRINT 1 ST SIZE SCALE
@ 20,29 SAY XSCALE1
XSCALE = GETSCALE( XSCALE1,SPACE(1))

@ 20,30 SAY SUBSTR(SCALE.SZ1,1,4)
@ 20,34 SAY SUBSTR(SCALE.SZ2,1,4)
@ 20,38 SAY SUBSTR(SCALE.SZ3,1,4)
@ 20,42 SAY SUBSTR(SCALE.SZ4,1,4)
@ 20,46 SAY SUBSTR(SCALE.SZ5,1,4)
@ 20,50 SAY SUBSTR(SCALE.SZ6,1,4)
@ 20,54 SAY SUBSTR(SCALE.SZ7,1,4)
@ 20,58 SAY SUBSTR(SCALE.SZ8,1,4)

*-- Line 16
@ 20,93 SAY IIF(llPrntComp , HLINE4 , '')

*-- Line 18 PRINT 2ND. SIZE SCALE
IF !EMPTY(XSCALE2)
  @ 21,29 SAY XSCALE2
  XSCALE = GETSCALE( XSCALE2,SPACE(1))
  @ 21,30 SAY SUBSTR(SCALE.SZ1,1,4)
  @ 21,34 SAY SUBSTR(SCALE.SZ2,1,4)
  @ 21,38 SAY SUBSTR(SCALE.SZ3,1,4)
  @ 21,42 SAY SUBSTR(SCALE.SZ4,1,4)
  @ 21,46 SAY SUBSTR(SCALE.SZ5,1,4)
  @ 21,50 SAY SUBSTR(SCALE.SZ6,1,4)
  @ 21,54 SAY SUBSTR(SCALE.SZ7,1,4)
  @ 21,58 SAY SUBSTR(SCALE.SZ8,1,4)
ENDIF

*--End of lpHrdLabls.
*!*************************************************************
*! Name      : lpOldFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : Print the form footer at the middle.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
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

@ 55,93 SAY XSTNAME
@ 56,93 SAY XSTADDR1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,93 SAY XSTADDR2
@ 58,93 SAY XSTADDR3
ROW = ROW + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name      : lpNewFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : Print the form footer at the end.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
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

*-- user does not want to print the style prices.
@ 54,000 SAY lcRpMsg2
@ 55,000 SAY lcRpMsg3
@ 56,093 SAY XSTNAME
@ 57,093 SAY XSTADDR1
=SEEK('O'+XORDER,'ORDHDR')

lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', OrdHdr.Note2, '')
@ 58,075 SAY XPIECES PICTURE '999999'
@ 58,093 SAY XSTADDR2
@ 59,093 SAY XSTADDR3
@ 62,060 SAY SUBSTR(lcShip,1,11)
@ 62,076 SAY IIF(LLRPSTYPRC , XVALUE , '') PICTURE '99999.99'
@ 62,086 SAY LEFT(lcCustPO,13)
@ 62,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 62,109 SAY ORDHDR.DEPT
@ 62,115 SAY XORDER
@ 63,101 SAY XPIKTKT
@ 63,108 SAY 'SHIP VIA: ' + SUBSTR(lcShip,1,10)

XVALUE = 0.00
ROW = ROW + 1

*--End of lpNewFoter.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
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

IF !SEEK('S'+ ORDHDR.Account + &lcTmpOrdL..STYLE,'SPCK_LIN')
  STORE SPACE(1) TO lcStrToPrn
  RETURN
ENDIF

SELECT SPCK_LIN
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+STYLE.Scale,'Scale')
  = SEEK('M'+ORDHDR.Account,'CUSTOMER')
  lcSkuTmpl=IIF(!EMPTY(CUSTOMER.SkuTmpl),CUSTOMER.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  = SEEK('S' + ORDHDR.Account + &lcTmpOrdL..STYLE , 'SPCK_LIN')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)

  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLength   = LEN(lcStrToPrn) + 2
  lnLocCol   = LEN(lcStrToPrn) + 5

ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'

ENDIF
RETURN

*--End of lpPrtSku.
*!*************************************************************
*! Name      : lfPrnStrn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : To print the acount/style/color skus.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
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

lcAlias = ALIAS()
lcSkuSize = ' '
lcKey     = ' '
IF SEEK('S' + ORDHDR.Account + &lcTmpOrdL..STYLE , 'SPCK_LIN')
  lcKey='S' + ORDHDR.Account + &lcTmpOrdL..STYLE 
  lnSep = 1
  Q = 1
  W = STR(Q,1)

  X = 1
  Z = STR(X,1)
  SELECT SPCK_LIN
  SCAN REST WHILE 'S' + ORDHDR.Account + &lcTmpOrdL..STYLE = lcKey
    IF &lcTmpOrdL..Qty&Z > 0
      lcSkuSize = lcSkuSize + 'S' + W + ':'+SUBSTR(Pack_Id,1,5) + ' '
    ENDIF
    lnSep = lnSep + 6
    X = X + 1
    Z = STR(X,1)
    Q = Q + 1
    W = STR(Q,1)
    IF Z = '9'
      EXIT
    ENDIF
  ENDSCAN
  lcSkuSize = ALLTRIM(lcSkuSize)
ENDIF
SELECT (lcAlias)

RETURN

*--End of lfPrnStrn.
*!*************************************************************
*! Name      : lfGetLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : Function to get the location.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfGetLoc()
*!*************************************************************
PROCEDURE lfGetLoc

lcAlias = ALIAS()
SELECT WHSLOC
lcStr = SPACE(0)
IF SEEK(&lcTmpOrdL..STYLE)
  SCAN REST WHILE STYLE = &lcTmpOrdL..STYLE
    lcStr = lcStr + " " + ALLTRIM(cLocation)
  ENDSCAN
  lcStr = ALLTRIM(lcStr)
  lnLen = LEN(lcStr)
ENDIF
SELECT (lcAlias)

*--End of lfGetLoc.
*!*************************************************************
*! Name      : lfContuLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/28/2002
*! Purpose   : Print the location.
*!*************************************************************
*! Called from : ALPKTKAC.PRG
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

FOR I = 12 TO lnLen
   @ ROW ,00 SAY SUBSTR(lcStr,I,85)
   I = I + 85
   DO lpSidLabls
   Row = Row + 1
ENDFOR

*--End of lfContuLoc.