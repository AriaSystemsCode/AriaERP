*:****************************************************************
*: Program file  : ALPKTKHF.PRG
*: Program desc. : Pick Ticket Form HF  (For Customer HandCraft)
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation (AL)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102432,1
*: Date          : 08/16/2001
*:****************************************************************
*: Calls 
*:               : FUNCTIONS  : lfGetLoc  , lfContuLoc
*:               : PROCEDURE  : lpFilEmpLn , lpSidLabls , lpHrdLabls
*:               :            : lpOldFoter , lpNewFoter , lpPrtSku
*:               :            : lfPrnStrn  , 
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*:C#102432,1         : 
*:****************************************************************
*:Modifications :
*:B804509,1 BWA 04/11/2001 Fix the bug of alignment the report in printing.
*:B605138,1 BWA 11/18/2001 Fix the bug of not printing the store in case multi store.
*:B605295,1 ASH 12/30/2001 1-Fix the bug of not printing the CustPo in case multi store.
*:B605295,1                2-Fix the bug of option (Print Sku/Pack) not appear in the option grid. (The fix in SYFRMCDD record)  
*:B605554,1 BWA 02/14/2002 Fix the bug of printing wrong SKU and PACk values.
*:****************************************************************
*-- Declaration variables.

SELECT (lcTmpOrdL)
lcOldKey = ORDER()
INDEX ON PikTkt + Order +IIF(PIKTKT = "******",ORDLINE.CWARECODE,'') + cGrupDetal + STR(LineNo , 6) ;
	 TAG NewIndex 

*B804509,1
*lcPrtAdr    = IIF(TYPE('lcPrtAdr') = "U","W",lcPrtAdr)
lcPrtAdr    = "W"

XPIKNAME    = lcPrtAdr # "N"
PRTPRICE    = llRpStyPrc                   && variable hold the printing of he style price
lcPrnStLoc  = llRpStyLoc                   && for the style location
lcPrnSkPck  = llRpSkuBck                   && for the sku\pack
lcPrnSkSiz  = llRpSkuSiz                   && for the sku\by size 

PRTNPAD  = IIF(llRpOrdLNt , 'Y' , 'N')
PRTPRICE = IIF(llRpStyPrc,'Y','N')
STORE 0.00 TO XORDTOT, XVALUE , SVALUE , XTOTQTY
STORE .F. TO llNoRec
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3]) + ' ' + laCompAdd[4] + ' ' + laCompAdd[5] TO HLINE4

XPHONE = lcXphone

HLINE5 = XPHONE
MSGL1 = lcRpMsg1
MSGL2 = lcRpMsg2
MSGL3 = lcRpMsg3
IF lcPrtAdr = "C"
  IF LEN(HLINE3) = 0
    STORE HLINE4 TO HLINE3
    STORE HLINE5 TO HLINE4
    STORE ' '    TO HLINE5
  ENDIF
ENDIF

*--Get Memorey Variables[Start]
XDYELOT_S = IIF(ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y', .T. , .F.)
llWareHous = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)
*--Get Memorey Variables[End]
SELECT (lcTmpOrdL)

IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

lcSpcOrd = ORDER("SPCK_LIN")
lcWhsOrd = ORDER("WHSLOC") 
SET ORDER TO TAG WHSLOCST IN WHSLOC
SET ORDER TO TAG SPCKLINS IN SPCK_LIN

lnLength=07
lcStrToPrn = ' '
lcSkuSize  = ' '
lcSclStrn  = ' '
lnLen      = 0
lcStr      = ' '

*-- MAIN LOOP
llEndPT = .F.
NEWDOC  = .T.

lnNotLine = 1
SET DEVICE TO PRINT

SCAN
  SELECT (lcTmpOrdL)
  lcWareH = OrdLine.CWareCode     &&variable to hold warehouse code
  IF lcPrtAdr = "W"
    *MAN Start changed the if statement
    *IF PIKTKT = "******"     && just for ALLOCATED lines
    IF !EMPTY(ORDLINE.CWARECODE)
      =SEEK(ORDLINE.CWARECODE,"WAREHOUS")
    ENDIF
    *MAN End
    HLINE2 = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    HLINE3 = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    HLINE4 = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    HLINE5 = TRANSFORM(WAREHOUS.CPHONE,lcPhonPict)
    IF LEN(HLINE3) = 0
      STORE HLINE4 TO HLINE3
      STORE HLINE5 TO HLINE4
      STORE ' '    TO HLINE5
    ENDIF
  ENDIF

  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
     XPIKTKT  = PIKTKT
     XORDER   = ORDER
     XDATE    = PIKDATE

     *B605138,1 BWA 11/18/2001 Fix the bug of not printing the store in case multi store.[START]
     *XSTORE   = ORDHDR.STORE
     XSTORE   = PIKTKT.Store 
     *B605138,1 BWA 11/18/2001.[END]

     SELECT ORDHDR
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     STORE 0.00 TO XORDTOT, XVALUE , SVALUE
     *-- We added the STORE# to use the new created index (ORDLINST) which
     *-- consists of ORDER+STORE+STYLE+STR(LINENO,6) in order to make
     *-- the search faster because one PIKTKT can't exceed one STORE. This
     *-- will work both form single or multi store orders. In single store
     *-- order, either the store will be empty or have something and all
     *-- the lines will have the same store. So this change will not improve
     *-- anything on the single store orders but it will on big multistore
     *-- orders. And in most cases the single store orders are not that big
     *-- anyway.
     IF ORDHDR.MultiPO 
       *B605295,1 ASH 12/30/2001 (Begin) Get the custpo from the ordline in case of multipo.
       *lcCustPO = CustPO
       *B605554,1 BWA 02/14/2002 (Begin) Get the custpo from the temp. file.
       *lcCustPO = OrdLine.CustPO
       lcCustPO = &lcTmpOrdL..CustPO
       *B605554,1 BWA 02/14/2002 (End)
       *B605295,1 ASH 12/30/2001 (End)
     ENDIF 

     *-- To copy the records which have the selected
     *-- PIKTKT from ORLINE file into a temp file (LNTEMP) instead of
     *-- LOCATING in the ORDLINE file for each selected piktkt

     XBTNAME  = CUSTOMER.BTNAME
     XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2') 
     XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
     XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
     
     IF LEN(TRIM(XBTADDR2)) =0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
     ENDIF

     *-- GET DESCRIPTIONS FOR CODED FIELDS

     SELECT CODES
     SET ORDER TO CODES IN CODES 

     *-- TERMS
     XTERM_DATA = gfCodDes(ORDHDR.CTERMCODE , 'CTERMCODE')

     *-- SHIPVIA
     XSHIP_DATA = gfCodDes(ORDHDR.SHIPVIA , 'SHIPVIA')

     *-- SPC INST.
     XSPCI_DATA = gfCodDes(ORDHDR.SPCINST , 'SPCINST')

     *-- SEASON
     XSEAS_DATA  = gfCodDes(ORDHDR.SEASON , 'SEASON')

     *-- DIVISION
     XDIVI_DATA  = gfCodDes(ORDHDR.CDIVISION , 'CDIVISION')

     SELECT (lcTmpOrdL)
    *MAN Start changed the if statement
    *IF PIKTKT = "******"     && just for ALLOCATED lines
    IF !EMPTY(ORDLINE.CWARECODE)
      =SEEK(ORDLINE.CWARECODE,"WAREHOUS")
    ENDIF
    *MAN End
     HLINE1  = IIF(EMPTY(lcDivLName) ,IIF(lcPrtAdr = "W", SUBSTR(WAREHOUS.CDESC,1,30) , lcCompName) , lcDivLName)      

     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
     XPIECES = 0
     *-- GET DESCRIPTIONS FOR CODED FIELDS.

     *-- GET THE SIZE SCALES
     *-- Moved and enhanced to get the printed scales
     *-- once outside the loop not every time in the loop because it will 
     *-- be the same REM1
     XSCALE2   = SPACE(1)
     SELECT (lcTmpOrdL)
     XSCALE1 = SCALE
  ENDIF
  *-- END PICK TKT/ORDER SELECTION.
  
  *-- SHIP-TO ADDRESS FOR THIS STORE
  *- Initialize the alt address if ther is any.
  SELECT OrdHdr
  
  *-- Defined variable to hold the distribution 
  
  lcDistCntr = ""
  IF Alt_ShpTo
    *-- Get the customer addresses from the right fields
    lcShpTName  = OrdHdr.STName   
    laShipTo[1] = OrdHdr.cAddress1
    laShipTo[2] = OrdHdr.cAddress2
    laShipTo[3] = OrdHdr.cAddress3
    laShipTo[4] = OrdHdr.cAddress4
    laShipTo[5] = OrdHdr.cAddress5

    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]

    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT Customer
    lnRecNo = RECNO()       && Variable to hold the record number.
    *-- This Form will not print the store information regardless if the 
    *-- Goods are going to a DC or not.
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 ) 
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 ) 
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
   
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  SELECT ORDHDR
  *-- START PRINTING
  
  DO lpHrdLabls

  *-- LINE LOOP  rem2
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0
  ROW     = 23
    
  DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT  .AND. LINENO <> 0 .AND. IIF(PIKTKT = "******",ORDLINE.CWareCode = lcWareh,.T.)
    *--  Put Page breaker if row execced spc. No.
    IF Row >= 53
      DO lpOldFoter
      DO lpHrdLabls
      Row = 23
    ENDIF

    *-- Modified to get the Style/Color location.     
    SELECT Style
    IF SEEK(&lcTmpOrdL..Style)    HDM         && + &LNTEMP.Color)
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

    *B804509,1 BWA 04/11/2001 Fix the bug of alignment the report in printing.[START]
    *@ ROW,00 SAY STYLE
    **-- Move the color 6 characters to right.
    *@ ROW,34 SAY ALLTRIM(lcScale)
    *@ ROW,36 SAY PIK1   PICTURE '@Z 9999'
    *@ ROW,40 SAY PIK2   PICTURE '@Z 9999'
    *@ ROW,45 SAY PIK3   PICTURE '@Z 9999'
    *@ ROW,49 SAY PIK4   PICTURE '@Z 9999'
    *@ ROW,53 SAY PIK5   PICTURE '@Z 9999'
    *@ ROW,58 SAY PIK6   PICTURE '@Z 9999'
    *@ ROW,63 SAY PIK7   PICTURE '@Z 9999'
    *@ ROW,67 SAY PIK8   PICTURE '@Z 9999'
    *@ ROW,72 SAY TOTPIK PICTURE '99999'

    @ ROW,00 SAY STYLE
    @ ROW,28 SAY ALLTRIM(lcScale)
    @ ROW,29 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,33 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,37 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,41 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,45 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,49 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,57 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,61 SAY TOTPIK PICTURE '99999'
    *B804509,1 BWA 04/11/2001.[END]

    IF PRTPRICE = 'Y'
      *B804509,1
      *@ ROW,78 SAY PRICE PICTURE '9999.99'
      @ ROW,66 SAY PRICE PICTURE '9999.99'
    ENDIF
    *B804509,1
    *@ ROW,88 SAY TOTPIK PICTURE '9999'
    @ ROW,75 SAY TOTPIK PICTURE '9999'

    DO lpSidLabls
    ROW = ROW + 1

    IF lcPrnSkPck = .F.
      @ ROW,00 SAY 'STYLE DESC.:' + XStyDesc
      IF lcPrnStLoc = .T.
        =lfGetLoc()
        IF !EMPTY(lcStr)
          @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
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
      *B605554,1 BWA 02/14/2002 (Begin) Print only the pack id if the order was done from pack.
      *IF SEEK('P' + ORDHDR.Account + &lcTmpOrdL..Style , 'SPCK_LIN')
      IF !EMPTY(&lcTmpOrdL..PACK_ID)
      *B605554,1 BWA 02/14/2002 (End)
        *B605554,1 BWA 02/14/2002 Fix the bug of printing wrong SKU and PACk values.[START]
        *@ ROW,00 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,00 SAY 'PACK ID#:' + ALLTRIM(&lcTmpOrdL..PACK_ID)
        *B605554,1 BWA.[END]

        @ ROW,25 SAY 'STYLE DESC.:'+XStyDesc
        DO lpSidLabls
        Row = Row + 1
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
        IF lcPrnSkSiz = .F.
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+XStyDesc
          lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
          DO lpSidLabls
          Row = Row + 1
          IF lcPrnStLoc = .T.
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,00 SAY 'BINS    :'+ SUBSTR(lcStr,1,85)
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
          *ash1
          *DO lpSidLabls
          *Row = Row + 1
        ELSE
          @ ROW,00 SAY 'STYLE DESC.:' + XStyDesc
          IF lcPrnStLoc = .T.
            =lfGetLoc()
            IF !EMPTY(lcStr)
              @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
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
          = lfPrnStrn() 
          lcSkuSize=lcStrToPrn+' '+lcSkuSize
          @ ROW,00 SAY SUBSTR(lcSkuSize,1,86)

          DO lpSidLabls
          Row = Row + 1  
        ENDIF
      ENDIF  
    ENDIF  
    *B605295,1 ASH 12/30/2001 (Begin) Return the right Alias after printing the SKU.
    SELECT (lcTmpOrdL)
    *B605295,1 ASH 12/30/2001 (End)
    *-- To print the color description, And the 
    *-- Style/Color location.
    DO lpSidLabls
    Row = Row + 1
    
    *-- SUM ORDER TOTALS
    XTOTQTY = XTOTQTY+ TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK
    SKIP                        
  ENDDO

  *B605138,1 BWA 11/18/2001 Fix the bug of not printing the 2nd Piktkt.[START]
  IF (XORDER # &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) ) OR (XORDER = &lcTmpOrdL..ORDER AND !EOF(lcTmpOrdL) AND XPIKTKT # &lcTmpOrdL..PIKTKT )
    SKIP - 1  
  ENDIF
  *B605138,1 BWA 11/18/2001.[END]

  *-- END PRINT LINE LOOP
  IF NewDoc
    *-- To print the notepad.
    IF PrtNPad = 'Y'
      SELECT NotePad
      IF SEEK('B' + OrdHdr.Order)
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)

        @ Row,02 SAY '* -- N O T E S -- *' 
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

  *-- Not to display "*" in front of the PIKTKT
  *-- if it was printed once.
  SELECT PIKTKT
  SELECT (lcTmpOrdL)
  IF PIKTKT = "******" .AND. LINENO <> 0 .AND. !EOF()
    SKIP -1
  ENDIF
ENDSCAN
SET DEVICE TO SCREEN

SET ORDER TO &lcSpcOrd IN SPCK_LIN
SET ORDER TO &lcWhsOrd IN WHSLOC

SELECT (lcTmpOrdL)
SET ORDER TO lcOldKey
RETURN
*-- END of Alpktkhf.
*:*************************************************************
*: Name      : lpFilEmpLn.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Fill the empty pick ticit lines.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpFilEmpLn
*:*************************************************************
*
PROCEDURE lpFilEmpLn

DO WHILE Row <= 53
  DO lpSidLabls 
  Row = Row + 1
ENDDO

*-- Enf OF lpFilEmpLn.
*:*************************************************************
*: Name      : lpSidLabls.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Print the Side labels.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpSidLabls
*:*************************************************************
*
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 34 .OR. ROW = 50
    @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE1,'')
  CASE ROW = 35 .OR. ROW = 51
    @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE2,'')
  CASE ROW = 36 .OR. ROW = 52
     @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE3,'')
  CASE ROW = 37 .OR. ROW = 53
    IF llEndPT .AND. Row = 53 .AND. NewDoc
      @ 53,000 SAY MSGL1                    
    ENDIF  
    @ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE4,'')
  CASE ROW = 23 .OR. ROW = 40
    @ ROW,93 SAY XSTNAME
  CASE ROW = 24 .OR. ROW = 41
    @ ROW,93 SAY XSTADDR1
  CASE ROW = 25 .OR. ROW = 42
    @ ROW,93 SAY XSTADDR2
  CASE ROW = 26 .OR. ROW = 43
    @ ROW,93 SAY XSTADDR3
    CASE ROW = 30 .OR. ROW = 46
    @ ROW,086 SAY IIF(ORDHDR.MultiPO , LEFT(lcCustPO,13), LEFT(OrdHdr.CUSTPO,13))
    @ ROW,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
    @ ROW,109 SAY ORDHDR.DEPT
    @ ROW,115 SAY XORDER
    *MAN Start
  *CASE ROW = 31 .OR. ROW = 48
   CASE ROW = 32 .OR. ROW = 48
    *MAN End

    @ ROW,101 SAY XPIKTKT
    @ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ENDCASE

*-- End of lpSidLabls.
*:*************************************************************
*: Name      : lpHrdLabls.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Print the Header labels.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpHrdLabls
*:*************************************************************
*
PROCEDURE lpHrdLabls
*-- Line 0
@ 02,001 SAY IIF(XPIKNAME=.T.,HLINE1,'')
*B804509,1
*@ 02,104 SAY IIF(XPIKNAME=.T.,HLINE1,'')
@ 02,095 SAY IIF(XPIKNAME=.T.,HLINE1,'')


*-- Line 1
@ 03,001 SAY IIF(XPIKNAME=.T.,HLINE2,'')
*B804509,1
*@ 03,104 SAY IIF(XPIKNAME=.T.,HLINE2,'')
@ 03,095 SAY IIF(XPIKNAME=.T.,HLINE2,'')


*-- Line 2
@ 04,001 SAY IIF(XPIKNAME=.T.,HLINE3,'')
*B804509,1
*@ 04,104 SAY IIF(XPIKNAME=.T.,HLINE3,'')
@ 04,095 SAY IIF(XPIKNAME=.T.,HLINE3,'')
 
*-- Line 3
@ 05,001 SAY IIF(XPIKNAME=.T.,HLINE4,'')
*B804509,1
*@ 05,070 SAY SUBSTR(ALLTRIM(XDIVI_DATA),1,11)
*@ 05,090 SAY XPIKTKT
*@ 05,104 SAY IIF(XPIKNAME=.T.,HLINE4,'')
@ 05,060 SAY SUBSTR(ALLTRIM(XDIVI_DATA),1,11)
@ 05,073 SAY XPIKTKT
@ 05,095 SAY IIF(XPIKNAME=.T.,HLINE4,'')

*-- Line 4
@ 07,060 SAY ORDHDR.APPROVAL
*B804509,1
*@ 07,104 SAY XSTNAME
@ 07,095 SAY XSTNAME
  
*-- Line 5
*B804509,1
*@ 08,90 SAY XDATE
*@ 08,104 SAY XSTADDR1
  
@ 08,073 SAY XDATE
@ 08,095 SAY XSTADDR1

*-- Line 6
*B804509,1
*@ 09,104 SAY XSTADDR2
@ 09,095 SAY XSTADDR2

*-- Line 7
@ 10,005 SAY XBTNAME
*B804509,1
*@ 10,054 SAY XSTNAME
*@ 10,104 SAY XSTADDR3

@ 10,048 SAY XSTNAME
@ 10,095 SAY XSTADDR3

*-- Line 8
@ 11,005 SAY XBTADDR1
*B804509,1
*@ 11,054 SAY XSTADDR1
@ 11,048 SAY XSTADDR1

*-- Line 9
@ 12,005 SAY XBTADDR2
*B804509,1
*@ 12,054 SAY XSTADDR2
@ 12,048 SAY XSTADDR2

*-- Line 10
@ 13,005 SAY XBTADDR3
*B804509,1
*@ 13,054 SAY XSTADDR3
*@ 13,102 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)
*@ 13,118 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
*@ 13,126 SAY ORDHDR.DEPT
*@ 13,137 SAY XORDER
  
@ 13,048 SAY XSTADDR3
@ 13,086 SAY IIF(ORDHDR.MultiPO , LEFT(lcCustPO,13), LEFT(OrdHdr.CUSTPO,13))
@ 13,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 13,109 SAY ORDHDR.DEPT
@ 13,119 SAY XORDER

*-- Line 11
*B804509,1
*@ 15,117 SAY XPIKTKT
*@ 15,124 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10) 

@ 15,101 SAY XPIKTKT
@ 15,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10) 

*-- Line 13
*B804509,1
*@ 17,104 SAY IIF(XPIKNAME=.T.,HLINE1,'')
@ 17,093 SAY IIF(XPIKNAME=.T.,HLINE1,'')

*-- Line 15
*B804509,1
*@ 18,001 SAY XACCOUNT
*@ 18,012 SAY XORDER
*@ 18,026 SAY ORDHDR.REP1
*@ 18,030 SAY ORDHDR.REP2
*@ 18,036 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)
*@ 18,044 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
*@ 18,052 SAY ORDHDR.COMPLETE
*@ 18,063 SAY SUBSTR(XTERM_DATA,1,11)
*@ 18,077 SAY XSTORE         PICTURE '####'
*@ 18,082 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  PICTURE '########'
*@ 18,089 SAY ORDHDR.DEPT 
*@ 18,104 SAY IIF(XPIKNAME=.T.,HLINE2,'')
*@ 19,104 SAY IIF(XPIKNAME=.T.,HLINE3,'')

@ 18,001 SAY XACCOUNT
@ 18,012 SAY XORDER
@ 18,023 SAY ORDHDR.REP1
@ 18,027 SAY ORDHDR.REP2
@ 18,031 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)
@ 18,039 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 18,045 SAY ORDHDR.COMPLETE
@ 18,054 SAY SUBSTR(XTERM_DATA,1,11)
@ 18,065 SAY XSTORE         PICTURE '####'
@ 18,070 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  PICTURE '########'  
@ 18,078 SAY ORDHDR.DEPT
@ 18,093 SAY IIF(XPIKNAME=.T.,HLINE2,'')
@ 19,093 SAY IIF(XPIKNAME=.T.,HLINE3,'')


*-- Line 17 PRINT 1 ST SIZE SCALE
*B804509,1
*@ 20,34 SAY XSCALE1
*XSCALE = GETSCALE( XSCALE1,SPACE(1))
*@ 20,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*@ 20,41 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*@ 20,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*@ 20,50 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*@ 20,54 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*@ 20,59 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
*@ 20,64 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
*@ 20,68 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')

@ 20,28 SAY XSCALE1
XSCALE = GETSCALE( XSCALE1,SPACE(1))
@ 20,30 SAY SUBSTR(SCALE.SZ1, 1 , 3 )
@ 20,35 SAY SUBSTR(SCALE.SZ2, 1 , 3 )
@ 20,39 SAY PADL(SCALE.SZ3, 3 , ' ' )
@ 20,43 SAY PADL(SCALE.SZ4, 3 , ' ' )
@ 20,47 SAY PADL(SCALE.SZ5, 3 , ' ' )
@ 20,51 SAY PADL(SCALE.SZ6, 3 , ' ' )
@ 20,55 SAY PADL(SCALE.SZ7, 3 , ' ' )
@ 20,59 SAY PADL(SCALE.SZ8, 3 , ' ' )

*-- Line 16
*B804509,1
*@ 20,104 SAY IIF(XPIKNAME=.T.,HLINE4,'')
@ 20,093 SAY IIF(XPIKNAME=.T.,HLINE4,'')

*-- Line 18 PRINT 2ND. SIZE SCALE
IF .NOT. EMPTY( XSCALE2 )
  *B804509,1
  *@ 21,34 SAY XSCALE2
  *XSCALE = GETSCALE( XSCALE2,SPACE(1))
  *@ 21,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  *@ 21,41 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  *@ 21,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  *@ 21,50 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  *@ 21,54 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  *@ 21,59 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  *@ 21,64 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  *@ 21,68 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')


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
*-- End OF lpHrdLabls.
*:*************************************************************
*: Name      : lpOldFoter.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Print the form footer at the middle of a
*:           : Spacific pick ticket. 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpOldFoter
*:*************************************************************
*
PROCEDURE lpOldFoter

@ 55,93 SAY XSTNAME
@ 56,93 SAY XSTADDR1
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,93 SAY XSTADDR2
@ 58,93 SAY XSTADDR3
@ 63,086 SAY IIF(ORDHDR.MultiPO , LEFT(lcCustPO,13), LEFT(OrdHdr.CUSTPO,13))

@ 63,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 63,109 SAY ORDHDR.DEPT            
@ 63,115 SAY XORDER
@ 64,101 SAY XPIKTKT
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
ROW = ROW + 1

*-- End OF lpOldFoter
*:*************************************************************
*: Name      : lpNewFoter.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Print the form footer at the end of a spacific 
*:           :  pick ticket. 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpNewFoter
*:*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 54,000 SAY MSGL2          
@ 55,000 SAY MSGL3          
@ 56,093 SAY XSTNAME
@ 57,093 SAY XSTADDR1

lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', SUBSTR(OrdHdr.Note2,1,25), '')

@ 58,075 SAY XPIECES PICTURE '999999' 
@ 58,093 SAY XSTADDR2
@ 59,093 SAY XSTADDR3
@ 62,060 SAY SUBSTR(XSHIP_DATA,1,11) 
@ 62,075 SAY IIF(PRTPRICE = 'Y',XVALUE,'') PICTURE '9999999.99'
@ 62,086 SAY IIF(ORDHDR.MultiPO , LEFT(lcCustPO,13), LEFT(OrdHdr.CUSTPO,13))  
@ 62,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 62,109 SAY ORDHDR.DEPT            
@ 62,115 SAY XORDER
@ 64,101 SAY XPIKTKT
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
XVALUE = 0.00
ROW = ROW + 1

*-- End Of lpNewFoter.
*:*************************************************************
*: Name      : lpPrtSku.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Print the ,main style/color Skus for a 
*:           : specific account.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : Do lpPrtSku
*:*************************************************************
PROCEDURE lpPrtSku

IF ! SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = " "
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M' + ORDHDR.Account , 'Customer')
  lcSkuTmpl = IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  
  *B605554,1 BWA 02/14/2002 To get the lenth of the header of the SKU.[START]
  PRIVATE lcOrdrSpk
  lcOrdrSpk = ORDER('SPCK_HDR')
  SET ORDER TO SKU_STYLE IN SPCK_HDR
  IF SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'SPCK_HDR')
    lnDime1 = LEN(ALLTRIM(SPCK_HDR.SKU))
    lnDime2 = 16 - lnDime1
    IF EMPTY(lcOrdrSpk)
      SET ORDER TO 0 IN SPCK_HDR
    ELSE
      SET ORDER TO EVAL(lcOrdrSpk) IN SPCK_HDR
    ENDIF
  ENDIF
  *B605554,1 BWA 02/14/2002.[END]

  = SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)

  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLength   = LEN(lcStrToPrn)+2
  lnLocCol   = LEN(lcStrToPrn) + 5
ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'
 
ENDIF
RETURN
*-- End Of lpPrtSku.
*:*************************************************************
*: Name      : lfPrnStrn.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Print the ,main style/color Skus for a 
*:           : specific account.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfPrnStrn ()
*:*************************************************************
FUNCTION lfPrnStrn
PRIVATE lcAlias
lcAlias = ALIAS()
lcSkuSize =' '
lcKey     =' '
IF SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style , 'Spck_Lin')
  lcKey='S' + ORDHDR.Account + &lcTmpOrdL..Style
  lnSep=1
  Q=1
  W=STR(Q,1)

  *B605554,1 BWA 02/14/2002 To get the lenth of the header of the SKU.[START]
  PRIVATE lcOrdrSpk
  lcOrdrSpk = ORDER('SPCK_HDR')
  SET ORDER TO SKU_STYLE IN SPCK_HDR
  IF SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'SPCK_HDR')
    lnDime1 = LEN(ALLTRIM(SPCK_HDR.SKU))
    IF EMPTY(lcOrdrSpk)
      SET ORDER TO 0 IN SPCK_HDR
    ELSE
      SET ORDER TO EVAL(lcOrdrSpk) IN SPCK_HDR
    ENDIF
  ENDIF
  *B605554,1 BWA 02/14/2002.[END]

  X=1
  Z=STR(X,1) 
  SELECT Spck_Lin
  *B605295,1 ASH 12/30/2001 (Begin) Use Ordhdr.Account instead of lcTmpOrdL.account
  *SCAN REST WHILE 'S' + &lcTmpOrdL..Account + &lcTmpOrdL..Style = lcKey
  SCAN REST WHILE 'S' + OrdHdr.Account + &lcTmpOrdL..Style = lcKey
  *B605295,1 ASH 12/30/2001 (End)
    IF &lcTmpOrdL..Qty&Z > 0

      *B605554,1 BWA 02/14/2002 To get the lenth of the header of the SKU.[START]
      *lcSkuSize = lcSkuSize+'S'+W+':'+SUBSTR(Pack_Id,9,5)+' '
      lcSkuSize = lcSkuSize + 'S' + W + ':' + SUBSTR(Pack_Id, lnDime1+1 ,5) + ' '
      *B605554,1 BWA 02/14/2002.[END]

    ENDIF
    lnSep=lnSep+6
    X=X+1
    Z=STR(X,1) 
    Q=Q+1
    W=STR(Q,1)

    IF Z='9'
      EXIT
    ENDIF  
  ENDSCAN
  lcSkuSize=ALLTRIM(lcSkuSize)
ENDIF
SELECT (lcAlias)
RETURN

*-- End OF lfPrnStrn.
*:*************************************************************
*: Name      : lfGetLoc.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Print the ,main style/color Skus for a 
*:           : specific account.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfGetLoc ()
*:*************************************************************
FUNCTION lfGetLoc
PRIVATE lcAlias
lcAlias = ALIAS()
lcStr = " "
SELECT WhsLoc
IF SEEK(&lcTmpOrdL..Style)   &&                +&LnTemp..Color)
  SCAN REST WHILE Style= &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr = ALLTRIM(lcStr)
  lnLen = LEN(lcStr)
ENDIF
SELECT (lcAlias)

*-- End OF lfGetLoc.
*:*************************************************************
*: Name      : lfContuLoc.
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 08/16/2000
*: Purpose   : Print the ,main style/color Skus for a
*:           : specific account.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : = lfContuLoc()
*:*************************************************************
*
FUNCTION lfContuLoc
FOR I = 12 TO lnLen  
   @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   DO lpSidLabls
   Row = Row + 1
ENDFOR

*-- End Of lfContuLoc.
*:***************************************************************
