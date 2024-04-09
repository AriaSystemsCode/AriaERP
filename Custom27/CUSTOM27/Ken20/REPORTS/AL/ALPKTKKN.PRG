*:***************************************************************************
*: Program file  : ALPKTKKN.PRG
*: Program desc. : CUSTOMIZED SALES ORDER ALLOCATION PICKING TICKET FOR KENT.
*: 				 : The program is copy from ALPKTKS.PRG exactly beside shifting it 2 lines up.
*: Date          : 11/05/2001
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102460
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : lpPrtSku,lpNewFoter,lpOldFoter,lpHrdLabls,lpSidLabls,lpFilEmpLn
*:    Functions  : lfContuLoc(),lfGetLoc(),lfPrnStrn()
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKKN
*:***************************************************************************
*: Modifications :
***************************************************************************

           *--Section of Initializing the variables --*
STORE SPACE(1) TO lcStrToPrn , lcSkuSize , lcStr , HLINE1
llNoRec = .F.                          && Variable used to make the endreport function in the main program.
lnLen   =  0
lcCustPoVl  = ''
HLine2  = TRIM(laCompAdd[1])           && variable hold the addressess of the company.
HLine3  = TRIM(laCompAdd[2])
HLine4  = TRIM(laCompAdd[3])
IF LEN(TRIM(HLine3)) = 0
  HLine3 = HLine4
  HLine4 = SPACE(1)
ENDIF

DIMENSION laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'HLINE1'

*--initialize Row globaly and llScale.
ROW       = 0
llScale   = .T.         && Flag to print the size scales at the first page only
llEndPT   = .F.
NEWDOC    = .T.
lnNotLine  = 1          && For the notepad
llRet     = .F.         && Flag to loop if Row>=45 when printing size scales

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
SET ORDER TO WhsLocSt INTO WhsLoc ADDITIVE

SELECT (lcTmpOrdL)
*-- MAIN LOOP
DO WHILE !EOF(lcTmpOrdL)

  SELECT (lcTmpOrdL)
  lnCurRec = RECNO()
  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
     SELECT (lcTmpOrdL)
     XPIKTKT  = PIKTKT
     XORDER   = ORDER
     XDATE    = PIKTKT.DATE
     XSTORE   = PIKTKT.Store

     SELECT ORDHDR
     =SEEK('O'+XORDER,'ORDHDR')
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     STORE 0.00 TO XORDTOT, XVALUE , SVALUE
     lcCustPoVl = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)

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
     lcPTERM = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
     lcSHIPV = gfCodDes(IIF(ALLTRIM(ORDHDR.SHIPVIA) = "*" ,CUSTOMER.SHIPVIA,ORDHDR.SHIPVIA),'SHIPVIA')
     lcDivis = gfCodDes(ORDHDR.CDIVISION,'CDIVISION')
     =gfRltFld(ORDHDR.CDIVISION , @laDivLName , 'CDIVISION')

     HLINE1   = IIF(EMPTY(HLINE1) , lcCompName , HLINE1)
     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
     XPIECES = 0
  ENDIF

  *- Initialize the alt address if ther is any.
  SELECT OrdHdr
  SEEK cordtype + XORDER

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
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' + TRIM(laShipTo[4]) + ' ' + laShipTo[5]

    IF LEN(TRIM(XSTADDR2)) = 0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT Customer
    IF !EMPTY(CUSTOMER.Dist_Ctr)
      =SEEK("S" + CUSTOMER.Account + CUSTOMER.Dist_Ctr )
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
  IF llRet
    LOOP
  ENDIF

  *-- LINE LOOP
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0
  SCAN WHILE piktkt = xpiktkt 
    DO CASE
      CASE EOF()
        NEWDOC = .T.
        EXIT
      CASE TOTPIK <= 0
        LOOP
      CASE ROW >= 53
        NEWDOC = .F.
        EXIT
    ENDCASE

    *-- Modified to get the Style/Color location.
    SELECT Style
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
    SELECT (lcTmpOrdL)
    @ ROW,1 SAY SUBSTR(STYLE,1,12)
    @ ROW,13 SAY SUBSTR(STYLE,14,19)
    @ ROW,19 SAY LEFT(lcScale,2)
    @ ROW,21 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,26 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,30 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,35 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,39 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,44 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,48 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,57 SAY TOTPIK PICTURE '99999'

    IF llRpStyPrc
      @ ROW,66 SAY PRICE PICTURE '9999.99'
    ENDIF
    @ ROW,74 SAY TOTPIK PICTURE '99999'

    DO lpSidLabls
    ROW = ROW + 1
    =lfChkRow("B")
    IF !llRpSkuBck
      @ ROW,1 SAY XStyDesc
      IF llRpStyLoc
        =lfGetLoc()
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
          DO lpSidLabls
          Row = Row + 1
          =lfChkRow("B")
          IF LEN(lcStr) > 18
            =lfContuLoc()
          ENDIF
        ELSE
          DO lpSidLabls
          Row = Row + 1
          =lfChkRow("B")
        ENDIF
      ENDIF
    ELSE
      IF SEEK('P' + ORDLINE.Account + ORDLINE.Style , 'SPCK_LIN' )
        @ ROW,1 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,25 SAY XStyDesc
        DO lpSidLabls
        Row = Row + 1
        =lfChkRow("B")
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        IF !llRpSkuSiz
          @ ROW,00 SAY SUBSTR(lcStrToPrn , 1 , IIF(LEN(lcStrToPrn)>=40 , 40 , LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40 , 41 , LEN(lcStrToPrn)+2) SAY XStyDesc
          DO lpSidLabls
          Row = Row + 1
          =lfChkRow("B")
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,00 SAY 'BINS    :'+ SUBSTR(lcStr,1,85)
              DO lpSidLabls
              Row = Row + 1
              =lfChkRow("B")
              IF LEN(lcStr) > 85
                =lfContuLoc()
              ENDIF
            ELSE
              DO lpSidLabls
              Row = Row + 1
		      =lfChkRow("B")
            ENDIF
          ENDIF
          DO lpSidLabls
          Row = Row + 1
	     =lfChkRow("B")
        ELSE
          @ ROW,00 SAY XStyDesc
          IF llRpStyLoc
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
              DO lpSidLabls
              Row = Row + 1
		      =lfChkRow("B")
              IF LEN(lcStr) > 18
                =lfContuLoc()
              ENDIF
            ELSE
              DO lpSidLabls
              Row = Row + 1
		      =lfChkRow("B")
           ENDIF
          ELSE
            DO lpSidLabls
            Row = Row + 1
		    =lfChkRow("B")
          ENDIF
          =lfPrnStrn()
          lcSkuSize = lcStrToPrn + ' ' + lcSkuSize
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)
          IF !EMPTY(lcSkuSize)
            DO lpSidLabls
            Row = Row + 1
		    =lfChkRow("B")
          ENDIF
        ENDIF
      ENDIF
    ENDIF

    *-- To print the color description, And the Style/Color location.
    DO lpSidLabls
    Row = Row + 1
    =lfChkRow("B")
    SELECT (lcTmpOrdL)
    XTOTQTY = XTOTQTY+ TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK

    SELECT (lcTmpOrdL)
  ENDSCAN
  *--End print line loop.

  *-- To print the notepad.
  IF NewDoc
    SELECT (lcTmpOrdL)
    IF llRpOrdNot
      SELECT NotePad
	  IF SEEK('B' + XORDER)
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)
        @ Row,00 SAY '* -- N O T E S -- *' 
        DO lpSidLabls
        Row = Row + 1
	    =lfChkRow("B")

        DO WHILE lnNotLine <= lnMemLins
          IF Row >= 53
            DO lpOldFoter
            DO lpHrdLabls
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

  llScale = .T.     && Re-initialize llScale to get the size scales for a new P/T
  SELECT (lcTmpOrdL)
ENDDO
SET DEVICE TO SCREEN

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpFilEmpLn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Fill the empty pick ticket lines.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
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

DO WHILE Row <= 51
  DO lpSidLabls
  IF ROW < 51
    Row = Row + 1
  ELSE
    EXIT
  ENDIF
ENDDO

*--End of lpFilEmpLn.
*!*************************************************************
*! Name      : lpSidLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Print the Side labels.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpSidLabls()
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 32 .OR. ROW = 48
    @ ROW,93 SAY IIF(llPrntComp , HLINE1 , '' )
  CASE ROW = 33 .OR. ROW = 49
    @ ROW,93 SAY IIF(llPrntComp , HLINE2 , '' )
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,93 SAY IIF(llPrntComp , HLINE3 , '' )
  CASE ROW = 35 .OR. ROW = 51
    IF llEndPT .AND. Row = 51 .AND. NewDoc
      @ 51,000 SAY lcRpMsg1
    ENDIF
    @ ROW,93 SAY IIF(llPrntComp , HLINE4 , '' )
  CASE ROW = 21 .OR. ROW = 38
    @ ROW,93 SAY XSTNAME
  CASE ROW = 22 .OR. ROW = 39
    @ ROW,93 SAY XSTADDR1
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,93 SAY XSTADDR2
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,93 SAY XSTADDR3
  CASE ROW = 28 .OR. ROW = 44
    @ ROW,086 SAY LEFT(lcCustPoVl,13)
    @ ROW,100 SAY XSTORE
    @ ROW,111 SAY ORDHDR.DEPT
    @ ROW,120 SAY XORDER
  CASE ROW = 29 .OR. ROW = 46
    @ ROW,101 SAY XPIKTKT
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(lcSHIPV,1,10)
ENDCASE

*--End of lpSidLabls.
*!*************************************************************
*! Name      : lpHrdLabls
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
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
@ 00,001 SAY IIF(llPrntComp , HLINE1 , '' )
@ 00,095 SAY IIF(llPrntComp , HLINE1 , '' )

*-- Line 1
@ 01,001 SAY IIF(llPrntComp , HLINE2 , '' )
@ 01,095 SAY IIF(llPrntComp , HLINE2 , '' )

*-- Line 2
@ 02,001 SAY IIF(llPrntComp , HLINE3 , '' )
@ 02,095 SAY IIF(llPrntComp , HLINE3 , '' )

*-- Line 3
@ 03,001 SAY IIF(llPrntComp , HLINE4 , '' )
@ 03,61 SAY LEFT(lcDivis,7)
@ 03,74 SAY XPIKTKT
@ 03,095 SAY IIF(llPrntComp , HLINE4 , '' )

*-- Line 5
@ 05,095 SAY XSTNAME

*-- Line 6
@ 06,61 SAY ORDHDR.APPROVAL
@ 06,74 SAY XDATE
@ 06,095 SAY XSTADDR1

*-- Line 7
@ 07,095 SAY XSTADDR2

*-- Line 8
@ 08,005 SAY XBTNAME
@ 08,048 SAY XSTNAME
@ 08,095 SAY XSTADDR3

*-- Line 9
@ 09,005 SAY XBTADDR1
@ 09,048 SAY XSTADDR1

*-- Line 10
@ 10,005 SAY XBTADDR2
@ 10,048 SAY XSTADDR2

*-- Line 11
@ 11,005 SAY XBTADDR3
@ 11,048 SAY XSTADDR3
@ 11,086 SAY LEFT(lcCustPoVl,13)
@ 11,100 SAY XSTORE
@ 11,111 SAY ORDHDR.DEPT
@ 11,120 SAY XORDER

*-- Line 13
@ 13,101 SAY XPIKTKT
@ 13,108 SAY 'SHIP VIA: ' + SUBSTR(lcSHIPV,1,10)

*-- Line 15
@ 15,093 SAY IIF(llPrntComp , HLINE1 , '' )

*-- Line 16
@ 16,1 SAY XACCOUNT
@ 16,7 SAY XORDER
@ 16,13 SAY ORDHDR.REP1
@ 16,17 SAY ORDHDR.REP2
@ 16,21 SAY ORDHDR.ENTERED
@ 16,30 SAY ORDHDR.START
@ 16,38 SAY ORDHDR.COMPLETE
@ 16,46 SAY SUBSTR(lcPTERM,1,14)
@ 16,61 SAY XSTORE
@ 16,69 SAY lcCustPoVl  PICTURE '##########'
@ 16,80 SAY ORDHDR.DEPT
@ 16,093 SAY IIF(llPrntComp , HLINE2 , '' )

*-- Line 17
@ 17,093 SAY IIF(llPrntComp , HLINE3 , '' )

ROW = 18
IF llScale
  DIMENSION laScaleSz[5]
  STORE SPACE (0) TO laScaleSz
  lnCount   = 1
  lcScalCnt = 0
  lcSelFile = SELECT(0)
  SELECT ORDLINE
  SEEK ORDHDR.CORDTYPE + XORDER
  lcKey = cordtype+order+STR(lineno,6)
  SCAN WHILE XORDER = ORDER  .AND. lcScalCnt <= 5
    IF ASCAN(laScaleSz,SUBSTR(ORDLINE.SCALE,1,2)) = 0 .AND. SEEK('S'+ORDLINE.SCALE,'SCALE')
      lcScalCnt = lcScalCnt +1
      DIMENSION laScaleSz[lcScalCnt]
      lcScalSiz = SUBSTR(ordLine.SCALE,1,2)         + PADL(ALLTRIM(SCALE.SZ1),4,' ') + ' ' + ;
                     PADL(ALLTRIM(SCALE.SZ2),4,' ') + PADL(ALLTRIM(SCALE.SZ3),4,' ') + ' ' + ;
                     PADL(ALLTRIM(SCALE.SZ4),4,' ') + PADL(ALLTRIM(SCALE.SZ5),4,' ') + ' ' + ;
                     PADL(ALLTRIM(SCALE.SZ6),4,' ') + PADL(ALLTRIM(SCALE.SZ7),4,' ') + ' ' + ;
                     PADL(ALLTRIM(SCALE.SZ8),4,' ')
      laScaleSz[lcScalCnt] = lcScalSiz
    ENDIF
  ENDSCAN

  SEEK (lcKey)
    FOR lnCount = 1 TO ALEN(laScaleSz,1)
      @ ROW,19 SAY laScaleSz[lnCount]
      IF lnCount = 1
        @ ROW,93 SAY IIF(llPrntComp , HLINE4 , '' )
      ENDIF

      DO lpSidLabls
      ROW = ROW + 1
    ENDFOR
    llScale =.F.
  SELECT (lcSelFile)
ELSE
  @ ROW,93 SAY IIF(llPrntComp , HLINE4 , '' )
ENDIF
ROW = IIF(ROW < 21 , 21 , ROW )

*--End of lpHrdLabls.
*!*************************************************************
*! Name      : lpOldFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Print the form footer at the middle.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpOldFoter()
*!*************************************************************
PROCEDURE lpOldFoter

@ 53,93 SAY XSTNAME
@ 54,93 SAY XSTADDR1
@ 55,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 55,93 SAY XSTADDR2
@ 56,93 SAY XSTADDR3
@ 61,086 SAY LEFT(lcCustPoVl,13)
@ 61,100 SAY XSTORE
@ 61,111 SAY ORDHDR.DEPT
@ 61,120 SAY XORDER
@ 62,101 SAY XPIKTKT
@ 62,108 SAY 'SHIP VIA: ' + SUBSTR(lcSHIPV,1,10)
ROW = ROW + 1

*--End of lpOldFoter.
*!*************************************************************
*! Name      : lpNewFoter
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Print the form footer at the end.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpNewFoter()
*!*************************************************************
PROCEDURE lpNewFoter

=lfChkRow("A")
@ 52,000 SAY lcRpMsg2
@ 53,000 SAY lcRpMsg3
@ 54,093 SAY XSTNAME
@ 55,093 SAY XSTADDR1
=SEEK('O'+XORDER,'ORDHDR')

lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 56,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 56,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', SUBSTR(OrdHdr.Note2,1,25), '')
@ 56,076 SAY XPIECES PICTURE '999999'
@ 56,093 SAY XSTADDR2
@ 57,093 SAY XSTADDR3
@ 60,060 SAY SUBSTR(lcSHIPV,1,11)
@ 60,076 SAY IIF(llRpStyPrc , XVALUE , '' ) PICTURE '999999.99'
@ 61,086 SAY LEFT(lcCustPoVl,13)
@ 61,100 SAY XSTORE
@ 61,111 SAY ORDHDR.DEPT
@ 61,120 SAY XORDER
@ 62,101 SAY XPIKTKT
@ 62,108 SAY 'SHIP VIA: ' + SUBSTR(lcSHIPV,1,10)
XVALUE = 0.00
ROW = ROW + 1

*--End of lpNewFoter.
*!*************************************************************
*! Name      : lpPrtSku
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Print the ,main style/color Skus for a specific account.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpPrtSku()
*!*************************************************************
PROCEDURE lpPrtSku

IF !SEEK('S'+ORDLINE.Account+ORDLINE.Style,'Spck_Lin')
  lcStrToPrn = " "
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M'+ORDLINE.Account,'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  = SEEK('S'+ORDLINE.Account+ORDLINE.Style,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)
  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLocCol = LEN(lcStrToPrn) + 5
ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'
ENDIF
RETURN

*--End of lpPrtSku.
*!*************************************************************
*! Name      : lfGetLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Function to get the location.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfGetLoc()
*!*************************************************************
FUNCTION lfGetLoc

lcAlias = ALIAS()
lcStr = " "        && Initialize the variable to prevent carrying over of locations.
SELECT WHSLOC
IF SEEK(&lcTmpOrdL..Style)
  SCAN REST WHILE Style = &lcTmpOrdL..Style
    lcStr = lcStr + " " + cLocation
  ENDSCAN
  lcStr = ALLTRIM(lcStr)
  lnLen = LEN(lcStr)
ENDIF
SELECT (lcAlias)

*--End of lfGetLoc.
*!*************************************************************
*! Name      : lfContuLoc
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : Print the location.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
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
   @ ROW ,00 SAY SUBSTR(lcStr,I,86)
   I = I + 86
   DO lpSidLabls
   Row = Row + 1
  =lfChkRow("B")
ENDFOR

*--End of lfContuLoc.
*!*************************************************************
*! Name      : lfChkRow
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : To check the row position.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =llfChkRow()
*!*************************************************************
FUNCTION lfChkRow
PARAMETER lcType

IF Row  >= IIF(lcType = "A" , 52 , 53)
  DO lpOldFoter
  DO lpHrdLabls
ENDIF

*--End of lfChkRow.
*!*************************************************************
*! Name      : lfPrnStrn
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 11/04/2001
*! Purpose   : To print the acount/style/color skus.
*!*************************************************************
*! Called from : ALPKTKKN.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lfPrnStrn()
*!*************************************************************
FUNCTION lfPrnStrn

PRIVATE lnAlias
lnAlias = SELECT(0)

lcSkuSize = ''

*-- To check the existence of Account/Style/Color SKUs 
IF SEEK('S'+ORDLINE.Account+ORDLINE.Style,'Spck_Lin')
  lcKey = 'S'+ORDLINE.Account+ORDLINE.Style
  =SEEK('S'+Style.Scale,'Scale')

  *-- Declaring this array to hold the Skus in order to sort them
  DIMENSION laSkus[Scale.Cnt,2]
  laSkus = ''

  *-- Saving the customer pointer
  lnCustRec = RECNO('Customer')

  *-- Searching for the main account to get check the customer skutmpl
  = SEEK('M'+ordline.Account,'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF BETWEEN(lnCustRec,1,RECCOUNT('Customer'))
    GOTO lnCustRec IN Customer
  ENDIF

  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8
    lnDime2 = 8
  ENDIF

  SELECT Spck_Lin
  SCAN REST WHILE 'S'+Account+Style = lcKey
    FOR lnCounter = 1 TO Scale.Cnt
      lcCounter = STR(lnCounter,1)
      IF Qty&lcCounter > 0
        laSkus[lnCounter,1] = STR(lnCounter,1)
        laSkus[lnCounter,2] = SUBSTR(Pack_Id,lnDime1+1,5)+' '
        EXIT
      ENDIF
    ENDFOR
  ENDSCAN
  =ASORT(laSkus,1)

  FOR lnArrayCnt = 1 TO ALEN(laSkus,1)
    lcArrayCnt = ALLTRIM(STR(lnArrayCnt))
    IF &lcTmpOrdL..Qty&lcArrayCnt > 0
      lcSkuSize = lcSkuSize+'S'+lcArrayCnt+':'+ ALLTRIM(laSkus[lnArrayCnt,2])+ ' '
    ENDIF
  ENDFOR

  lcSkuSize = ALLTRIM(lcSkuSize)
ENDIF
SELECT (lnAlias)
RETURN

*--End of lfPrnStrn.