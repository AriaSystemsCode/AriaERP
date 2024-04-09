*:***************************************************************************
*: Program file  : ALPKTKKR.PRG
*: Program desc. : CUSTOMIZED SALES ORDER ALLOCATION PICKING TICKET FOR KAR10.
*: Date          : 07/15/2002
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102668
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : lpPrtSku   , lpNewFoter , lpOldFoter , lpHrdLabls , lpSidLabls
*:               : lpFilEmpLn
*:    Functions  : lfContuLoc() , lfGetLoc() , lfPrnStrn()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKKR
*:***************************************************************************
*: Modifications :
***************************************************************************

*--initialize the variables.
STORE SPACE(1) TO lcStrToPrn , lcSkuSize , lcSclStrn , lcStr , HLINE1
ROW       = 0
lcCustPO  = ''
llEndPT   = .F.
NEWDOC    = .T.
llPrntClr = llRpPrtClr
lnLength  = 07
lnLen     = 0
lnNotLine = 1                    && For the notepad
llNoRec   = .F.                  && Variable used to make the endreport function in the main program.

*--If EOF it will return to the main program
IF EOF(lcTmpOrdL)
  llNoRec = .T.
  *-- Message <No records to display.>
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
SET DEVICE TO PRINT

*--Change the tag in the SPCK_LIN file.
SELECT SPCK_LIN
SET ORDER TO SPCKLINS INTO SPCK_LIN ADDITIVE

*--Change the tag in the WHSLOC file.
SELECT WHSLOC
SET ORDER TO WHSLOCST INTO WHSLOC ADDITIVE

SELECT (lcTmpOrdL)
*-- MAIN LOOP
SCAN
  WAIT WINDOW 'Printing PT# '+&lcTmpOrdL..PikTkt NOWAIT

  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
    SELECT (lcTmpOrdL)
    lcPikTkt  = PikTkt
    lcOrder   = ORDER
    ldDate    = PIKTKT.DATE
    lcStore   = PIKTKT.Store
    lcDept    = ORDHDR.DEPT

    HLINE2 = IIF(llPrntComp,gfGetAdr('WAREHOUS' , '' , '' , '' , 1),'')
    HLINE3 = IIF(llPrntComp,gfGetAdr('WAREHOUS' , '' , '' , '' , 2),'')
    HLINE4 = IIF(llPrntComp,gfGetAdr('WAREHOUS' , '' , '' , '' , 3),'')
    IF LEN(HLINE3) = 0
      STORE HLINE4 TO HLINE3
      STORE ' '    TO HLINE4
    ENDIF

    SELECT ORDHDR
    STORE 0 TO lnValue , SVALUE
    XACCOUNT  = ORDHDR.Account
    lcCustPo  = LEFT(IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO),10)

    XBTNAME  = CUSTOMER.BTNAME
    XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
    XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
    XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')

    IF LEN(TRIM(XBTADDR2)) = 0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF

    *-- Get descriptions for Coded fields.
    SELECT CODES
    SET ORDER TO CODES IN CODES 
    XTERM_DATA = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
    XSHIP_DATA = gfCodDes(IIF(ALLTRIM(ORDHDR.SHIPVIA) = "*" ,CUSTOMER.SHIPVIA,ORDHDR.SHIPVIA),'SHIPVIA')
    XDIVI_DATA = LEFT(gfCodDes(ORDHDR.CDIVISION,'CDIVISION'),9)

    HLINE1 = LEFT(WAREHOUS.CDESC,30)
    STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
    XPIECES = 0

     *-- GET THE SIZE SCALES
     *-- Moved and enhanced to get the printed scales
     *-- once outside the loop not every time in the loop because it will
     *-- be the same REM1
     XSCALE2   = SPACE(1)
     SELECT (lcTmpOrdL)
     lcKey = EVAL(KEY())   && Variable to hold the record number.
     XSCALE1 = SUBSTR(SCALE,1,1)
     LOCATE REST FOR ( SCALE <> XSCALE1 .AND. ORDER = LCORDER  .AND. !EMPTY(SCALE) )
     IF !EOF()
       XSCALE2 = SUBSTR(SCALE,1,1)
     ENDIF
     =SEEK(lcKey)  && Back to original record number.
  ENDIF

  *-- SHIP-TO ADDRESS FOR THIS STORE
  *- Initialize the alt address if ther is any.
  SELECT OrdHdr

  IF Alt_ShpTo
    XSTNAME  = OrdHdr.STName
    XSTADDR1 = OrdHdr.cAddress1
    XSTADDR2 = OrdHdr.cAddress2
    XSTADDR3 = LEFT(TRIM(OrdHdr.cAddress3) + ' ' +TRIM(OrdHdr.cAddress4) + ' ' + TRIM(OrdHdr.cAddress3),34)

    IF LEN(TRIM(XSTADDR2)) = 0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT CUSTOMER
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

  SCAN REST WHILE PikTkt+Order+cGrupDetal+STR(LineNo,6) = lcPikTkt + lcOrder + 'D'

    IF Row >= 53
      DO lpOldFoter
      DO lpHrdLabls
    ENDIF

    *-- Modified to get the Style/Color location.
    SELECT STYLE
    IF SEEK(&lcTmpOrdL..Style)
      XSTYDESC   = DESC
      lcStyLocat = Location
      lcScale    = Scale
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
      lcScale    = ''
    ENDIF

    *-- To get the color description.
    lcClrDesc = LEFT(gfCodDes(SUBSTR(Style.Style,14,6) ,'COLOR'),15)

    SELECT (lcTmpOrdL)
    @ ROW,00 SAY SUBSTR(Style.Style,1,12)
    @ ROW,19 SAY SUBSTR(Style.Style,14,6)
    @ ROW,28 SAY lcScale
    @ ROW,29 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,33 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,37 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,41 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,45 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,49 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,57 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,61 SAY TOTPIK PICTURE '99999'

    IF llRpStyPrc
      @ ROW,66 SAY PRICE PICTURE '9999.99'
    ENDIF

    @ ROW,75 SAY TOTPIK PICTURE '9999'
    DO lpSidLabls
    ROW = ROW + 1

    IF !llRpSkuBck
      @ ROW,06 SAY IIF(llPrntClr,'CLR DESC.:   ' + lcClrDesc, '')
      IF llRpStyLoc
        =lfGetLoc()
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
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
      IF SEEK('P'+PIKTKT.Account+&lcTmpOrdL..Style,'SPCK_LIN')
        @ ROW,00 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,25 SAY 'STYLE DESC.:'+XStyDesc
        @ ROW,59 SAY IIF(llPrntClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,10), '')
        DO lpSidLabls
        Row = Row + 1
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        IF !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+XStyDesc
          lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
          @ ROW,lcCol SAY IIF(llPrntClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,76-lcCol), '')
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
          @ ROW,06 SAY IIF(llPrntClr,'CLR DESC.:   ' + lcClrDesc, '')
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
              DO lpSidLabls
              Row = Row + 1

              IF LEN(lcStr) > 18
                =lfContuLoc()
              ENDIF
            ELSE
              DO lpSidLabls
              Row = Row + 1
           ENDIF
          ELSE
            DO lpSidLabls
            Row = Row + 1
          ENDIF
          =lfPrnStrn()
          lcSkuSize = lcStrToPrn + ' ' + lcSkuSize
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)

          DO lpSidLabls
          Row = Row + 1
        ENDIF
      ENDIF
    ENDIF

    *-- To print the color description, And the 
    *-- Style/Color location.
    DO lpSidLabls
    Row = Row + 1

    SELECT (lcTmpOrdL)
    XTOTQTY = XTOTQTY + TOTQTY
    lnValue = lnValue + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK
  ENDSCAN

  IF (lcOrder # &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) ) OR (lcOrder = &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) AND lcPikTkt # &lcTmpOrdL..PIKTKT )
    SKIP - 1
  ENDIF

  *-- END PRINT LINE LOOP
  IF NewDoc
    *-- To print the notepad.
    IF llRpOrdNot
      SELECT NotePad
      IF SEEK('B' + lcOrder)
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
    llEndPT   = .T.
    DO lpFilEmpLn
    DO lpNewFoter
  ELSE
    llEndPT = .F.
    DO lpFilEmpLn
    DO lpOldFoter
    LOOP
  ENDIF

  SELECT (lcTmpOrdL)
ENDSCAN

WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpFilEmpLn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/15/2002
*! Purpose   : Fill the empty pick ticit lines.
*!*************************************************************
*! Called from : ALPKTKKR.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
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
*! Date      : 07/15/2002
*! Purpose   : Print the Side labels.
*!*************************************************************
*! Called from : ALPKTKBR.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
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
  CASE ROW = 30 .OR. ROW = 46
    @ ROW,086 SAY lcCustPO
    @ ROW,100 SAY lcStore
    @ ROW,109 SAY lcDept
    @ ROW,115 SAY lcOrder
  CASE ROW = 31 .OR. ROW = 48
    @ ROW,101 SAY lcPikTkt
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ENDCASE

*--End of lpSidLabls.
*!*************************************************************
*! Name      : lpHrdLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/15/2002
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKKR.PRG
*!*************************************************************
*! Calls       : 
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
@ 05,060 SAY XDIVI_DATA
@ 05,073 SAY lcPikTkt
@ 05,095 SAY IIF(llPrntComp , HLINE4 , '')

*-- Line 4
@ 07,060 SAY OrdHdr.APPROVAL
@ 07,095 SAY XSTNAME

*-- Line 5
@ 08,60 SAY lcDept
@ 08,73 SAY ldDate
@ 08,95 SAY XSTADDR1

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
@ 13,086 SAY lcCustPO
@ 13,100 SAY lcStore
@ 13,109 SAY lcDept
@ 13,119 SAY lcOrder

*-- Line 11
@ 15,101 SAY lcPikTkt
@ 15,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)

*-- Line 13
@ 17,093 SAY IIF(llPrntComp , HLINE1 , '')

*-- Line 15
@ 18,001 SAY XACCOUNT
@ 18,012 SAY lcOrder
@ 18,023 SAY OrdHdr.REP1
@ 18,027 SAY OrdHdr.REP2
@ 18,031 SAY SUBSTR(DTOC(OrdHdr.ENTERED),1,5)
@ 18,037 SAY SUBSTR(DTOC(OrdHdr.START),1,5)
@ 18,044 SAY OrdHdr.COMPLETE
@ 18,054 SAY SUBSTR(XTERM_DATA,1,11)
@ 18,065 SAY lcStore         PICTURE '####'
@ 18,069 SAY lcCustPO
@ 18,079 SAY IIF(!EMPTY(OrdHdr.APPROVAL),lcDept,'')
@ 18,093 SAY IIF(llPrntComp , HLINE2 , '')

@ 19,093 SAY IIF(llPrntComp , HLINE3 , '')

*-- Line 17 PRINT 1 ST SIZE SCALE
@ 20,28 SAY XSCALE1

XSCALE = GETSCALE( XSCALE1,SPACE(1))

*----BUG FIX  SADAF [Begin] Move scale Header to the left by 2 position
@ 20,32 SAY PADL(SCALE.SZ1,3,' ')
@ 20,36 SAY PADL(SCALE.SZ2,3,' ')
@ 20,40 SAY PADL(SCALE.SZ3,3,' ')
@ 20,44 SAY PADL(SCALE.SZ4,3,' ')
@ 20,48 SAY PADL(SCALE.SZ5,3,' ')
@ 20,52 SAY PADL(SCALE.SZ6,3,' ')
@ 20,56 SAY PADL(SCALE.SZ7,3,' ')
@ 20,60 SAY PADL(SCALE.SZ8,3,' ')
*----BUG FIX  SADAF [End] Move scale Header to the left by 2 position

*-- Line 16
@ 20,93 SAY IIF(llPrntComp , HLINE4 , '')

*-- Line 18 PRINT 2ND. SIZE SCALE
IF !EMPTY( XSCALE2 )
  @ 21,28 SAY XSCALE2
  XSCALE = GETSCALE( XSCALE2,SPACE(1))

  @ 21,30 SAY PADL(SCALE.SZ1,3,' ')
  @ 21,34 SAY PADL(SCALE.SZ2,3,' ')
  @ 21,38 SAY PADL(SCALE.SZ3,3,' ')
  @ 21,42 SAY PADL(SCALE.SZ4,3,' ')
  @ 21,46 SAY PADL(SCALE.SZ5,3,' ')
  @ 21,50 SAY PADL(SCALE.SZ6,3,' ')
  @ 21,54 SAY PADL(SCALE.SZ7,3,' ')
  @ 21,58 SAY PADL(SCALE.SZ8,3,' ')
ENDIF
lcSclStrn = XSCALE
ROW       = 23

*--End of lpHrdLabls.
*!*************************************************************
*! Name      : lpOldFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/15/2002
*! Purpose   : Print the form footer at in the middle of a spacific 
*!           : pick ticket. 
*!*************************************************************
*! Called from : ALPKTKKR.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpOldFoter

@ 55,93 SAY XSTNAME
@ 56,93 SAY XSTADDR1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,93 SAY XSTADDR2
@ 58,93 SAY XSTADDR3
@ 63,086 SAY lcCustPO
@ 63,100 SAY lcStore
@ 63,109 SAY lcDept
@ 63,115 SAY lcOrder
@ 64,101 SAY lcPikTkt
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ROW = ROW + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name      : lpNewFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/15/2002
*! Purpose   : Print the form footer at the end of a spacific 
*!           : pick ticket.
*!*************************************************************
*! Called from : ALPKTKKR.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 54,000 SAY lcRpMsg2
@ 55,000 SAY lcRpMsg3
@ 56,093 SAY XSTNAME
@ 57,093 SAY XSTADDR1

lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF

@ 58,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1) <> '*' , SUBSTR(OrdHdr.Note2,1,25), '')
@ 58,076 SAY XPIECES PICTURE '999999'
@ 58,093 SAY XSTADDR2
@ 59,093 SAY XSTADDR3
@ 62,060 SAY SUBSTR(XSHIP_DATA,1,11)
@ 62,076 SAY IIF(llRpStyPrc , lnValue , '') PICTURE '99999.99'
@ 63,086 SAY lcCustPO
@ 63,100 SAY lcStore
@ 63,109 SAY lcDept
@ 63,115 SAY lcOrder
@ 64,101 SAY lcPikTkt
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
lnValue = 0
ROW = ROW + 1

*--End of lpNewFoter.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/15/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKKR.PRG
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : Do lpPrtSku
*!*************************************************************
PROCEDURE lpPrtSku

IF !SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Lin')
  *--- If we print 2 p/t and the style of the second p/t have no
  *--- Sku, leave it empty and do not print the sku of the style
  *--- In the first p/t.
  lcStrToPrn = " "
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M'+PIKTKT.Account,'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  = SEEK('S'+PIKTKT.Account+&lcTmpOrdL..Style,'Spck_Lin')
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

*-- End Of lpPrtSku.
*!*************************************************************
*! Name      : lfPrnStrn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/15/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKKR.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfPrnStrn

lcAlias = ALIAS()
lcSkuSize =' '
lcKey     =' '
IF SEEK('S' + XAccount + &lcTmpOrdL..Style , 'Spck_Lin')
  lcKey = 'S' + XAccount + &lcTmpOrdL..Style
  lnSep = 1
  Q = 1
  W = STR(Q,1)
  X = 1
  Z = STR(X,1)
  SELECT Spck_Lin
  SCAN REST WHILE 'S'+XAccount+&lcTmpOrdL..Style = lcKey

    IF &lcTmpOrdL..Qty&Z > 0
      lcSkuSize = lcSkuSize+'S'+W+':'+SUBSTR(Pack_Id,9,5)+' '
    ENDIF
    lnSep = lnSep + 6
    X = X + 1
    Z = STR(X,1)
    Q = Q + 1
    W = STR(Q,1)
    IF Z ='9'
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
*! Date      : 07/15/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKKR.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfGetLoc

lcAlias = ALIAS()
lcStr = " "
SELECT WHSLOC

IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE Style+Color = &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr = ALLTRIM(lcStr)
  lnLen = LEN(lcStr)
ENDIF
SELECT (lcAlias)

*--End of lfGetLoc.
*!*************************************************************
*! Name      : lfContuLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 07/15/2002
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKKR.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION lfContuLoc

FOR I = 12 TO lnLen
   @ ROW ,00 SAY SUBSTR(lcStr,I,86)
   I = I + 86

   DO lpSidLabls
   Row = Row + 1
ENDFOR

*--End of lfContuLoc.