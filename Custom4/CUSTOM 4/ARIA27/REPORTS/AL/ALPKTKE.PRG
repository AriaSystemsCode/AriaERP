*:***************************************************************************
*: Program file  : ALPKTKE.PRG
*: Program desc. : ALLOCATION PIKTICKT FORM "E".
*: Date          : 03/05/1999
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION(AL)
*: Developer     : 
*: Tracking Job Number: 
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKE
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*: B602847,1 KHM 04/29/99 Fixed the bug of not displaying a message when
*: B602847,1              there were no records to print.
*: B802227,1 BWA Get the customer addresses from the right fields
*: B802479,1 HDM 08/02/1999 Assign phone value from main program
*: B802520,1 BWA 15/08/1999 Fix the bug of always print the company name and
*: B802520,1 address even if the parameter of "Print company name"  is setup to NO.
*: B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*: B803131,1 HBG 08/03/2000 Fix bug of printing the data out of the boxes
*: B803891,1 AME 01/23/2001 give the ability to print either company address or warehouse's
*: B604206,1 AME 02/15/2001 Print page breaker if the line no. execeds spc. No.
*: B604833,1 SSH 20/Aug/2001 Fix problem Variable ‘LCPRTADR’ not found.
*: B605138,1 BWA 11/18/2001 Fix the bug of not printing the store in case multi store.
*: B605882,1 BWA 04/21/2002 Fix many bugs in the program.
*: B605926,1 BWA 07/09/2002 Print the store even if there is DC.
**************************************************************************** 

*B803891,1 AME [START] PUT WARE CODE INTO INDEX .
SELECT (lcTmpOrdL)
lcOldKey = ORDER()
INDEX ON PikTkt + Order +IIF(PIKTKT = "******",ORDLINE.CWARECODE,'') + cGrupDetal + STR(LineNo , 6) ;
	 TAG NewIndex 
*B803891,1 AME [END]

*B605882,1 BWA 04/21/2002 Search for the color postion and the lenght of it 
*                         and initialize the variable hold the custpo.[START]
*--llRpPrtClr :- print Color Desc.
*--THE COLOR LENGTH
STORE 0 TO lnClrLnCd , lnClrPosCd
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnE  = LEN(laItemSeg[lnCount,3])
    lnClrPosE = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
lcCustPo = ""
*B605882,1 BWA 04/21/2002.[END]

*-- Set the memory variables.
*lcMClr = 'Y'
*llPrntClr = IIF(lcMClr = 'Y', .T., .F.)
*PRTPRICE = 'Y'
*STORE 'Y' TO lcPrnStLoc , lcPrnSkPck , lcPrnSkSiz
*XPIKNAME='Y'

*B802520,1 BWA 15/08/1999 Fix the bug of the name and the address
*XPIKNAME    = TYPE('llPrntComp') = 'L'
*B803891,1 AME [Start]
*XPIKNAME    =  llPrntComp
*: *B604833,1 SSH 20/Aug/2001 Fix problem Variable ‘LCPRTADR’ not found.
lcPrtAdr = IIF(TYPE("lcPrtAdr")="C",lcPrtAdr,"C")
*: *B604833,1 SSH [END]
XPIKNAME     =  lcPrtAdr # "N"
*B803891,1 AME [End]
*B802520,1 [END]

PRTPRICE   = llRpStyPrc                   && variable hold the printing of he style price
lcPrnStLoc = llRpStyLoc                   && for the style location
lcPrnSkPck = llRpSkuBck                   && for the sku\pack
lcPrnSkSiz = llRpSkuSiz                   && for the sku\by size 

PRTNPAD = IIF(llRpOrdLNt , 'Y' , 'N')

*B605882,1 BWA 04/21/2002 Comment this line because it is hardcoded.[START]
*PRTPRICE ='Y'
*B605882,1 BWA 04/21/2002.[END]

STORE 0.00 TO XORDTOT, XVALUE , SVALUE , XTOTQTY
STORE .F. TO llNoRec
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4

*-- B802479,1 [Start] 
*XPHONE = laCompAdd[6]
XPHONE = lcXphone
*-- B802479,1 [End]

HLINE5 = XPHONE
MSGL1 = lcRpMsg1
MSGL2 = lcRpMsg2
MSGL3 = lcRpMsg3
*B803891,1 AME [START]
IF lcPrtAdr = "C"
*B803891,1 AME [END]
  IF LEN(HLINE3) = 0
    STORE HLINE4 TO HLINE3
    STORE HLINE5 TO HLINE4
    STORE ' '    TO HLINE5
  ENDIF
*B803891,1 AME [START]
ENDIF
*B803891,1 AME [END]
*--Get Memorey Variables[Start]
XDYELOT_S = IIF(ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y', .T. , .F.)
llWareHous = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)
*--Get Memorey Variables[End]
SELECT (lcTmpOrdL)

IF EOF()
  llNoRec = .T.
  *B602847,1 KHM 04/29/99 (Begin) Displaying a message to indicate that no
  *B602847,1              records were found to display.
  =gfModalGen('TRM00052B00000','DIALOG' )
  *B602847,1 KHM 04/29/99 (End)

  SET DEVICE TO SCREEN
  RETURN
ENDIF

*B604206,1 AME [START] SET ORDERS TO PROPER SEARCH (STYLE) ORDERS
lcSpcOrd = ORDER("SPCK_LIN")
lcWhsOrd = ORDER("WHSLOC") 
SET ORDER TO TAG WHSLOCST IN WHSLOC
SET ORDER TO TAG SPCKLINS IN SPCK_LIN
*B604206,1 AME [END]

lnLength=07
lcStrToPrn=' '
lcSkuSize =' '
lcSclStrn =' '
lnLen     = 0
lcStr     =' '
*E800068,1 TMI 06/26/96  (End)

*-- MAIN LOOP
llEndPT = .F.
NEWDOC  = .T.

lnNotLine = 1
SET DEVICE TO PRINT

*DO WHILE INKEY() <> 32
SCAN
  SELECT (lcTmpOrdL)
  *B803891,1 AME [Start] set addresses to ware house address.
  lcWareH = OrdLine.CWareCode     &&variable to hold warehouse code
  IF lcPrtAdr = "W"
    IF PIKTKT = "******"     && just for ALLOCATED lines
      =SEEK(ORDLINE.CWARECODE,"WAREHOUS")
    ENDIF
    HLINE2     = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    HLINE3     = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    HLINE4     = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    HLINE5     = TRANSFORM(WAREHOUS.CPHONE,lcPhonPict)
    IF LEN(HLINE3) = 0
      STORE HLINE4 TO HLINE3
      STORE HLINE5 TO HLINE4
      STORE ' '    TO HLINE5
    ENDIF
  ENDIF
  *B803891,1 AME [END]
  

  *-- GET ORDERHDR & LINE ITEMS
  IF NEWDOC
     XPIKTKT = PIKTKT
     XORDER  = ORDER
     XDATE   = PIKDATE

     *B605882,1 BWA 04/21/2002 Variable hold the departement all over the piktkt.[START]
     lcDept  = ORDHDR.DEPT
     *B605882,1 BWA 04/21/2002.[END]

     *B605138,1 BWA 11/18/2001 Fix the bug of not printing the store in case multi store.[START]
     *XSTORE   = ORDHDR.STORE
     XSTORE   = PIKTKT.STORE
     *B605138,1 BWA 11/18/2001.[END]

     SELECT ORDHDR
     XMULTI   = MULTI
     XACCOUNT = ACCOUNT
     STORE 0.00 TO XORDTOT, XVALUE , SVALUE
     *-- We added the STORE# to use the new created index (ORDLINST) which
     *-- consists of ORDER+STORE+STYLE+COLOR+STR(LINENO,6) in order to make
     *-- the search faster because one PIKTKT can't exceed one STORE. This
     *-- will work both form single or multi store orders. In single store
     *-- order, either the store will be empty or have something and all
     *-- the lines will have the same store. So this change will not improve
     *-- anything on the single store orders but it will on big multistore
     *-- orders. And in most cases the single store orders are not that big
     *-- anyway.
     *B803891,1 AME [START] NOT APLICABLE LINES 
     *SELECT ORDLINE
     *SEEK XORDER+XSTORE 
     *B803891,1 AME [END]

     *B605882,1 BWA 04/21/2002 Variable hold the custpo all over the piktkt.[START]
     *IF ORDHDR.MultiPO 
     *  lcCustPO=CustPO
     *ENDIF 
      lcCustPo = IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO)
     *B605882,1 BWA 04/21/2002.[END]

     *-- To copy the records which have the selected
     *-- PIKTKT from ORLINE file into a temp file (LNTEMP) instead of
     *-- LOCATING in the ORDLINE file for each selected piktkt

     XBTNAME  = CUSTOMER.BTNAME
     
     *B802227,1 BWA Get the customer addresses from the right fields
     *XBTADDR1 = CUSTOMER.BTADDR1
     *XBTADDR2 = CUSTOMER.BTADDR2
     *XBTADDR3 = TRIM(CUSTOMER.BTCITY) + ' ' +TRIM(CUSTOMER.BTSTATE)+ ' '+;
      CUSTOMER.BTZIP
     
     XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2') 
     XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
     XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
     *B802227,1 BWA [END]
     
     IF LEN(TRIM(XBTADDR2)) =0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
     ENDIF

     *-- GET DESCRIPTIONS FOR CODED FIELDS

     SELECT CODES
     SET ORDER TO CODES IN CODES 
     XTERM_DATA = gfCodDes(ORDHDR.CTERMCODE , 'CTERMCODE')
     XSHIP_DATA = gfCodDes(ORDHDR.SHIPVIA , 'SHIPVIA')
     XSPCI_DATA = gfCodDes(ORDHDR.SPCINST , 'SPCINST')
     XSEAS_DATA  = gfCodDes(ORDHDR.SEASON , 'SEASON')
     XDIVI_DATA  = gfCodDes(ORDHDR.CDIVISION , 'CDIVISION')

     *B803891,1 AME [Start] set Name to  propper name.
     *HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)      
     SELECT (lcTmpOrdL)
     IF PIKTKT = "******"
       =SEEK(ORDLINE.CWARECODE,"WAREHOUS")
     ENDIF
     HLINE1 = IIF(EMPTY(lcDivLName) ,IIF(lcPrtAdr = "W", SUBSTR(WAREHOUS.CDESC,1,30) , lcCompName) , lcDivLName)
     *B803891,1 AME [END]

     STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
     XPIECES = 0
     * GET DESCRIPTIONS FOR CODED FIELDS [end]

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
  *SEEK xOrder
  
  *B801524,1 OWA 03/19/98 (Begin) Defined variable to hold the distribution 
  *B801524,1              center.
  lcDistCntr = ""
  *B801524,1 OWA 03/19/98 (End) 

  IF Alt_ShpTo
    
    *B802227,1 BWA Get the customer addresses from the right fields
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

    *XSTNAME  = STNAME
    *XSTADDR1 = STADDR1
    *XSTADDR2 = STADDR2
    *XSTADDR3 = TRIM(STCITY) + ' ' +TRIM(STSTATE) + ' ' + STZIP

    *XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 ) 
    *XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 ) 
    *XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
    *B802227,1 BWA [END]
    
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT CUSTOMER

    *B801524,1 OWA 03/19/98 (Begin) To get the distribution center ship 
    *B801524,1              to address if exists instead of the store 
    *B801524,1              ship to address .
    * XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    * XSTADDR1 = STADDR1
    * XSTADDR2 = STADDR2
    * XSTADDR3 = TRIM(STCITY) + ' ' +TRIM(STSTATE) + ' ' + STZIP
    
    lnRecNo = RECNO()       && Variable to hold the record number.

    IF !EMPTY(Dist_Ctr)

      *B605926,1 BWA 07/09/2002 Print the store name and DC address.[START]
      *lcDistCntr = ALLTRIM(Dist_Ctr)
      *=seek('S'+XACCOUNT+lcDistCntr)

      lcOrgnalSt = CUSTOMER.STORE
      lcDistCntr = ALLTRIM(Dist_Ctr)
      =seek('S'+XACCOUNT+lcDistCntr)
      lcDistCntr = lcOrgnalSt
      *B605926,1 BWA 07/09/2002.[END]

    ENDIF

    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    
    *B802227,1 BWA Get the customer addresses from the right fields
    *XSTADDR1 = STADDR1
    *XSTADDR2 = STADDR2
    *XSTADDR3 = TRIM(STCITY) + ' ' +TRIM(STSTATE) + ' ' + STZIP

    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 ) 
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 ) 
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
    *B802227,1 BWA [END]
    
    IF !EMPTY(lcDistCntr) 
      GO lnRecNo        
    ENDIF         
    *B801524,1 OWA 03/19/98 (End) 

    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  SELECT ORDHDR
  *-- START PRINTING
  
  DO lpHrdLabls

  *-- LINE LOOP  rem2
*  SELECT &LNTEMP
  SELECT (lcTmpOrdL)
  NEWDOC  = .T.
  XTOTQTY = 0
  ROW     = 23
    

* DO WHILE .T.
  *B803891,1 AME [START] IN CASE OF ALLOCATED ORDERS SORT ON WARECODE
  *DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT .AND. LINENO <> 0
  DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT  .AND. LINENO <> 0 .AND. IIF(PIKTKT = "******",ORDLINE.CWareCode = lcWareh,.T.)
  *B803891,1 AME [END]
*    DO CASE
*      CASE EOF()
*        NEWDOC = .T.
*        EXIT
*      CASE TOTPIK <= 0
*        SKIP                              
*        LOOP
*      CASE ROW >= 47
*        NEWDOC = .F.
*        EXIT
*    ENDCASE

    *B604206,1 AME [Start] Put Page breaker if row execced spc. No.
    IF Row >= 53
      DO lpOldFoter
      DO lpHrdLabls
      Row = 23
    ENDIF
    *B604206,1 AME [END]

    *-- Modified to get the Style/Color location.     
    SELECT Style
    IF SEEK(&lcTmpOrdL..Style)    HDM         && + &LNTEMP.Color)
*     XSTYDESC   = SUBSTR(DESC,1,15)
      XSTYDESC   = DESC
      lcStyLocat = Location
      *B600607,1 added scale.
      lcScale    = Scale
    ELSE
      XSTYDESC   = ''
      lcStyLocat = ''
      lcScale    = ''
    ENDIF
    
    *-- To get the color description.
    *SELECT Code
    *lcClrDesc = IIF(SEEK('C'+&LnTemp.Color), cData, SPACE(15))

    *B605882,1 BWA 04/21/2002 Variable hold the color descreption.[START]
    lcClrDesc = gfCodDes(SUBSTR(Style.Style,lnClrPosE,lnClrLnE) ,'COLOR')
    *B605882,1 BWA 04/21/2002.[END]

    *SELECT &LNTEMP
    SELECT (lcTmpOrdL)
    
    @ ROW,00 SAY STYLE
    *B800985,1 ASH 02/25/97 (Begin) Move the color 6 characters to right.
    *@ ROW,19 SAY COLOR
    *B800985,1 ASH 02/25/97 (End)
    *B600607,1 added scale.
    
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
    *@ ROW,28 SAY lcScale
    @ ROW,34 SAY ALLTRIM(lcScale)
    
    *@ ROW,29 SAY PIK1   PICTURE '@Z 9999'
    *@ ROW,33 SAY PIK2   PICTURE '@Z 9999'
    *@ ROW,37 SAY PIK3   PICTURE '@Z 9999'
    *@ ROW,41 SAY PIK4   PICTURE '@Z 9999'
    *@ ROW,45 SAY PIK5   PICTURE '@Z 9999'
    *@ ROW,49 SAY PIK6   PICTURE '@Z 9999'
    *@ ROW,53 SAY PIK7   PICTURE '@Z 9999'
    *@ ROW,57 SAY PIK8   PICTURE '@Z 9999'
    *@ ROW,61 SAY TOTPIK PICTURE '99999'
    @ ROW,36 SAY PIK1   PICTURE '@Z 9999'
    @ ROW,40 SAY PIK2   PICTURE '@Z 9999'
    @ ROW,45 SAY PIK3   PICTURE '@Z 9999'
    @ ROW,49 SAY PIK4   PICTURE '@Z 9999'
    @ ROW,53 SAY PIK5   PICTURE '@Z 9999'
    @ ROW,58 SAY PIK6   PICTURE '@Z 9999'
    @ ROW,63 SAY PIK7   PICTURE '@Z 9999'
    @ ROW,67 SAY PIK8   PICTURE '@Z 9999'
    @ ROW,72 SAY TOTPIK PICTURE '99999'
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[END]
    
    IF PRTPRICE
      *@ ROW,66 SAY PRICE PICTURE '9999.99'
      @ ROW,78 SAY PRICE PICTURE '9999.99'
    ENDIF
    *@ ROW,75 SAY TOTPIK PICTURE '9999'  && Moved on chr. to the right. 
    @ ROW,88 SAY TOTPIK PICTURE '9999'  && Moved on chr. to the right. 
    
    
    DO lpSidLabls
    ROW = ROW + 1
    *E800068,1 TMI 03/26/96 Print the main SKU# for the style/color.

    *IF lcPrnSkPck='N'
    IF lcPrnSkPck=.F.
      @ ROW,00 SAY 'STYLE DESC.:'+XStyDesc
      @ ROW,22 SAY IIF(llRpPrtClr,'CLR DESC.: ' + lcClrDesc, '')
*      IF lcPrnStLoc = 'Y'
      IF lcPrnStLoc = .T.
        =lfGetLoc()
        IF !EMPTY(lcStr)
          *B604206,1 AME [START]
          *@ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
          @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
          *B604206,1 [END]
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
*      IF SEEK('P' + &LnTemp.Account + &LnTemp.Style + &LnTemp.Color,'SPCK_LIN')
      IF SEEK('P' + ORDHDR.Account + &lcTmpOrdL..Style , 'SPCK_LIN')
        @ ROW,00 SAY 'PACK ID#:'+Spck_Lin.Pack_Id
        @ ROW,25 SAY 'STYLE DESC.:'+XStyDesc
        @ ROW,59 SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,10), '')
        DO lpSidLabls
        Row = Row + 1
      ELSE
        DO lpPrtSku      && Procedure to print the Style/Color Sku no.
*        IF lcPrnSkSiz='N'
        IF lcPrnSkSiz=.F.
          @ ROW,00 SAY SUBSTR(lcStrToPrn,1,IIF(LEN(lcStrToPrn)>=40,40,LEN(lcStrToPrn)))
          @ ROW,IIF(LEN(lcStrToPrn)>=40,41,LEN(lcStrToPrn)+2) SAY 'STYLE DESC.:'+XStyDesc
          lcCol=IIF(LEN(lcStrToPrn)>=40,73,LEN(lcStrToPrn)+34)
          @ ROW,lcCol SAY IIF(llRpPrtClr,'CLR DESC.:' + SUBSTR(lcClrDesc,1,76-lcCol), '')
          DO lpSidLabls
          Row = Row + 1
*          IF lcPrnStLoc='Y'
          IF lcPrnStLoc = .T.
            =lfGetLoc()
            IF !EMPTY(lcStr)
              *B604206,1 AME [START]
              *@ ROW,00 SAY 'LOCATION:'+ SUBSTR(lcStr,1,85)
              @ ROW,00 SAY 'BINS    :'+ SUBSTR(lcStr,1,85)
              *B604206,1 AME [END]
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
          @ ROW,33 SAY IIF(llRpPrtClr,'CLR DESC.:' + lcClrDesc, '')
*          IF lcPrnStLoc='Y'
          IF lcPrnStLoc = .T.
            =lfGetLoc()
            IF !EMPTY(lcStr)
              *B604206,1 AME [START]
              *@ ROW,60 SAY 'LOCATION:'+ SUBSTR(lcStr,1,10)
              @ ROW,60 SAY 'BINS    :'+ SUBSTR(lcStr,1,10)
              *B604206,1 AME [END]
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

    *-- To print the color description, And the 
    *-- Style/Color location.
    DO lpSidLabls
    Row = Row + 1

    * CUM ORDER TOTALS
    XTOTQTY = XTOTQTY+ TOTQTY
    XVALUE  = XVALUE + TOTPIK * PRICE
    XPIECES = XPIECES + TOTPIK

    *SELECT &LNTEMP              
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

        *E100207,1 MFM 03/30/95 (Begin).
        lnOldMemW = SET("MEMOWIDTH")
        SET MEMOWIDTH TO 75
        lnMemLins = MEMLINES(NOTEPAD.MNOTES)

        @ Row,00 SAY '* -- N O T E S -- *' 
        DO lpSidLabls
        Row = Row + 1
        
        *SCAN WHILE Type + Key = 'B' + OrdHdr.Order

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
        *E100207,1 MFM 03/30/95 (End).

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

  *-- TAK 05/24/94 Not to display "*" in front of the PIKTKT
  *-- if it was printed once.
  SELECT PIKTKT
*  SEEK &PIKTEMP.PIKTKT
  *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
  *REPLACE PRTFLAG WITH 'P'
  *B603487,1 BWA 03/08/2000 [END]

*  SELECT &PIKTEMP
*  SKIP
*ENDDO
  *B803891,1 AME [START]
  SELECT (lcTmpOrdL)
  IF PIKTKT = "******" .AND. LINENO <> 0 .AND. !EOF()
    SKIP -1
  ENDIF
  *B803891,1 AME [END]
ENDSCAN
SET DEVICE TO SCREEN

*B604206,1 AME [START]  RESTORE ORIGINAL ORDERS
SET ORDER TO &lcSpcOrd IN SPCK_LIN
SET ORDER TO &lcWhsOrd IN WHSLOC
*B604206,1 AME [END]

*B803891,1 AME [START] RETURN WITH OLD INDEX
SELECT (lcTmpOrdL)
SET ORDER TO lcOldKey
*B803891,1 AME [END]
RETURN
*-- END ALO820E.PRG


*!*************************************************************
*! Name : lpFilEmpLn.
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 07/13/94.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Fill the empty pick ticit lines.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedures : Alo820E.
*!*************************************************************
PROCEDURE lpFilEmpLn

DO WHILE Row <= 53
  DO lpSidLabls 
  Row = Row + 1
ENDDO


*!*************************************************************
*! Name : lpSidLabls.
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 07/13/94.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the Side labels.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedures : Alo820E.
*!*************************************************************
PROCEDURE lpSidLabls

DO CASE
  CASE ROW = 34 .OR. ROW = 50
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
    *@ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE1,'')
    @ ROW,104 SAY IIF(XPIKNAME=.T.,HLINE1,'')
    *B803131,1 [end]
  CASE ROW = 35 .OR. ROW = 51
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
    *@ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE2,'')
    @ ROW,104 SAY IIF(XPIKNAME=.T.,HLINE2,'')
    *B803131,1 [end]
  CASE ROW = 36 .OR. ROW = 52
     *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]  
     *@ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE3,'')
     @ ROW,104 SAY IIF(XPIKNAME=.T.,HLINE3,'')
     *B803131,1 [end]
  CASE ROW = 37 .OR. ROW = 53
    IF llEndPT .AND. Row = 53 .AND. NewDoc
      @ 53,000 SAY MSGL1                    
    ENDIF  
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]  
    *@ ROW,93 SAY IIF(XPIKNAME=.T.,HLINE4,'')
    @ ROW,104 SAY IIF(XPIKNAME=.T.,HLINE4,'')
    *B803131,1 [end]
  CASE ROW = 23 .OR. ROW = 40
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]  
    *@ ROW,93 SAY XSTNAME
    @ ROW,104 SAY XSTNAME
    *B803131,1 [end]
  CASE ROW = 24 .OR. ROW = 41
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]  
    *@ ROW,93 SAY XSTADDR1
    @ ROW,104 SAY XSTADDR1
    *B803131,1 [end]
  CASE ROW = 25 .OR. ROW = 42
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]  
    *@ ROW,93 SAY XSTADDR2
    @ ROW,104 SAY XSTADDR2
    *B803131,1 [end]
  CASE ROW = 26 .OR. ROW = 43
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
    *@ ROW,93 SAY XSTADDR3
    @ ROW,104 SAY XSTADDR3
    *B803131,1 [end]
  CASE ROW = 30 .OR. ROW = 46
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
    *@ ROW,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO) 

    *B605882,1 BWA 04/21/2002 Variable hold the custpo all over the piktkt.[START]
    *@ ROW,102 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO) 
    @ ROW,102 SAY lcCustPO
    *B605882,1 BWA 04/21/2002.[END]

    *B801524,1 OWA 03/19/98 (Begin) Printed the distribution center if 
    *B801524,1              exists instead of the store.
    *@ ROW,100 SAY XSTORE
    *@ ROW,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
    @ ROW,118 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
    *B801524,1 OWA 03/19/98 (End) 
    *@ ROW,109 SAY ORDHDR.DEPT

    *B605882,1 BWA 04/21/2002 Variable hold the departement all over the piktkt.[START]
    *@ ROW,126 SAY ORDHDR.DEPT
    @ ROW,126 SAY lcDept
    *B605882,1 BWA 04/21/2002.[END]

    *@ ROW,115 SAY XORDER
    @ ROW,137 SAY XORDER

    *B803131,1 [end]
  CASE ROW = 31 .OR. ROW = 48
    *B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
    *@ ROW,101 SAY XPIKTKT
    @ ROW,117 SAY XPIKTKT
    *E500068,1 TMI 06/26/96
    *@ ROW,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
    @ ROW,124 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
    *E500068,1 TMI 06/26/96 (End)
    *B803131,1 [end]
ENDCASE


*!*************************************************************
*! Name : lpHrdLabls.
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 07/13/94.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the Header labels.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedures : Alo820E.
*!*************************************************************
PROCEDURE lpHrdLabls
*-- Line 0
@ 02,001 SAY IIF(XPIKNAME=.T.,HLINE1,'')            && MFM 02/02/94
*B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
*@ 02,095 SAY IIF(XPIKNAME=.T.,HLINE1,'')            && MFM 02/02/94  && TAK 05/24/94
@ 02,104 SAY IIF(XPIKNAME=.T.,HLINE1,'')            && MFM 02/02/94  && TAK 05/24/94  

*-- Line 1
@ 03,001 SAY IIF(XPIKNAME=.T.,HLINE2,'')            && MFM 02/02/94    
*@ 03,095 SAY IIF(XPIKNAME=.T.,HLINE2,'')            && MFM 02/02/94  && TAK 05/24/94
@ 03,104 SAY IIF(XPIKNAME=.T.,HLINE2,'')            && MFM 02/02/94  && TAK 05/24/94  

*-- Line 2
@ 04,001 SAY IIF(XPIKNAME=.T.,HLINE3,'')            && MFM 02/02/94    
*@ 04,095 SAY IIF(XPIKNAME=.T.,HLINE3,'')            && MFM 02/02/94  && TAK 05/24/94
@ 04,104 SAY IIF(XPIKNAME=.T.,HLINE3,'')            && MFM 02/02/94  && TAK 05/24/94
  
*-- Line 3
@ 05,001 SAY IIF(XPIKNAME=.T.,HLINE4,'')            && MFM 02/02/94    
*@ 05,060 SAY XDIVI_DATA
@ 05,070 SAY SUBSTR(ALLTRIM(XDIVI_DATA),1,11)
*@ 05,073 SAY XPIKTKT
@ 05,090 SAY XPIKTKT

*@ 05,095 SAY IIF(XPIKNAME=.T.,HLINE4,'')            && TAK 06/09/94
@ 05,104 SAY IIF(XPIKNAME=.T.,HLINE4,'')            && TAK 06/09/94
  
*-- Line 4
@ 07,060 SAY ORDHDR.APPROVAL
*@ 07,095 SAY XSTNAME                                && TAK 05/24/94
@ 07,104 SAY XSTNAME                                && TAK 05/24/94
  
*-- Line 5
*@ 08,073 SAY XDATE
@ 08,90 SAY XDATE
*@ 08,095 SAY XSTADDR1                               && TAK 05/24/94 
@ 08,104 SAY XSTADDR1                               && TAK 05/24/94 
  
*-- Line 6
*@ 09,095 SAY XSTADDR2
@ 09,104 SAY XSTADDR2
  
*-- Line 7
@ 10,005 SAY XBTNAME
*@ 10,048 SAY XSTNAME
@ 10,054 SAY XSTNAME
*@ 10,095 SAY XSTADDR3                               && TAK 05/24/94
@ 10,104 SAY XSTADDR3                               && TAK 05/24/94
  
*-- Line 8
@ 11,005 SAY XBTADDR1
*@ 11,048 SAY XSTADDR1
@ 11,054 SAY XSTADDR1
  
*-- Line 9
@ 12,005 SAY XBTADDR2
*@ 12,048 SAY XSTADDR2
@ 12,054 SAY XSTADDR2
  
*-- Line 10
@ 13,005 SAY XBTADDR3
*@ 13,048 SAY XSTADDR3
@ 13,054 SAY XSTADDR3

*@ 13,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  &&TAK 08/22/94

*B605882,1 BWA 04/21/2002 Variable hold the custpo all over the piktkt.[START]
*@ 13,102 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  &&TAK 08/22/94
@ 13,102 SAY lcCustPO
*B605882,1 BWA 04/21/2002.[END]

*B803131,1 [End]

*B801524,1 OWA 03/19/98 (Begin) Printed the distribution center if 
*B801524,1              exists instead of the store.
*@ 13,100 SAY XSTORE                                 && TAK 05/24/94
*B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
*@ 13,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 13,118 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
*B803131,1 [End]
*B801524,1 OWA 03/19/98 (End) 

*B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
*@ 13,109 SAY ORDHDR.DEPT                           && TAK 05/24/94

*B605882,1 BWA 04/21/2002 Variable hold the departement all over the piktkt.[START]
*@ 13,126 SAY ORDHDR.DEPT                           && TAK 05/24/94
@ 13,126 SAY lcDept
*B605882,1 BWA 04/21/2002.[END]

*@ 13,119 SAY XORDER
@ 13,137 SAY XORDER
  
*-- Line 11
*@ 15,101 SAY XPIKTKT
@ 15,117 SAY XPIKTKT
*E500068,1 TMI 06/26/96
*@ 15,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10) 
@ 15,124 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10) 
*E500068,1 TMI 06/26/96(End)
*-- Line 13
*@ 17,093 SAY IIF(XPIKNAME=.T.,HLINE1,'')            && MFM 02/02/94
@ 17,104 SAY IIF(XPIKNAME=.T.,HLINE1,'')            && MFM 02/02/94
*B803131,1 [End]

*-- Line 15
@ 18,001 SAY XACCOUNT
@ 18,012 SAY XORDER
*B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
*@ 18,023 SAY ORDHDR.REP1
*@ 18,027 SAY ORDHDR.REP2
@ 18,026 SAY ORDHDR.REP1
@ 18,030 SAY ORDHDR.REP2

*@ 18,031 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)
@ 18,036 SAY SUBSTR(DTOC(ORDHDR.ENTERED),1,5)

*B800985,1 ASH 02/25/97 (Begin) Move start date 2 characters to left and 
*B800985,1               complete date 1 character to left.
*@ 18,039 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
*@ 18,045 SAY ORDHDR.COMPLETE
*@ 18,037 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 18,044 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
*@ 18,044 SAY ORDHDR.COMPLETE
@ 18,052 SAY ORDHDR.COMPLETE
*B800985,1 ASH 02/25/97 (End)
*@ 18,054 SAY SUBSTR(XTERM_DATA,1,11)                && TAK 05/24/94
@ 18,063 SAY SUBSTR(XTERM_DATA,1,11)                && TAK 05/24/94
*B800985,1 ASH 02/25/97 (Begin) Move store and custpo 1 characeter to left.
*@ 18,066 SAY XSTORE         PICTURE '####'
*@ 18,070 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  PICTURE '########'  &&TAK 08/22/94
*@ 18,065 SAY XSTORE         PICTURE '####'
@ 18,077 SAY XSTORE         PICTURE '####'
*@ 18,069 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  PICTURE '########'  

*B605882,1 BWA 04/21/2002 Variable hold the custpo all over the piktkt.[START]
*@ 18,082 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  PICTURE '########'  
@ 18,082 SAY lcCustPO PICTURE '########'  
*B605882,1 BWA 04/21/2002.[END]

*B800985,1 ASH 02/25/97 (End)
*@ 18,078 SAY ORDHDR.DEPT

*B605882,1 BWA 04/21/2002 Variable hold the departement all over the piktkt.[START]
*@ 18,089 SAY ORDHDR.DEPT 
@ 18,089 SAY lcDept
*B605882,1 BWA 04/21/2002.[END]

*@ 18,093 SAY IIF(XPIKNAME=.T.,HLINE2,'')            && MFM 02/02/94  && YMA
@ 18,104 SAY IIF(XPIKNAME=.T.,HLINE2,'')            && MFM 02/02/94  && YMA

*@ 19,093 SAY IIF(XPIKNAME=.T.,HLINE3,'')            && MFM 02/02/94
@ 19,104 SAY IIF(XPIKNAME=.T.,HLINE3,'')            && MFM 02/02/94

*-- Line 17 PRINT 1 ST SIZE SCALE
*@ 20,28 SAY XSCALE1
@ 20,34 SAY XSCALE1

XSCALE = GETSCALE( XSCALE1,SPACE(1))
*--TAK 11/29/94 Put in SubStr.

*B500670,1 MFM 04/26/95 (Begin) Fixed the print off scale desc.
*@ 20,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
*@ 20,34 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
*@ 20,38 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
*@ 20,42 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
*@ 20,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
*@ 20,50 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
*@ 20,54 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
*@ 20,58 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
@ 20,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
@ 20,41 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
@ 20,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
@ 20,50 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
@ 20,54 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
@ 20,59 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
@ 20,64 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
@ 20,68 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
*B500670,1 MFM 04/26/95 (End)
  
*-- Line 16
*@ 20,93 SAY IIF(XPIKNAME=.T.,HLINE4,'')            && TAK 06/09/94
@ 20,104 SAY IIF(XPIKNAME=.T.,HLINE4,'')            && TAK 06/09/94

*-- Line 18 PRINT 2ND. SIZE SCALE
IF .NOT. EMPTY( XSCALE2 )
  *@ 21,28 SAY XSCALE2
  @ 21,34 SAY XSCALE2
  XSCALE = GETSCALE( XSCALE2,SPACE(1))
  *@ 21,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  *@ 21,34 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  *@ 21,38 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  *@ 21,42 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  *@ 21,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  *@ 21,50 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  *@ 21,54 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  *@ 21,58 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
  @ 21,30 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,3),3,' ')
  @ 21,41 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,3),3,' ')
  @ 21,46 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,3),3,' ')
  @ 21,50 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,3),3,' ')
  @ 21,54 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,3),3,' ')
  @ 21,59 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,3),3,' ')
  @ 21,64 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,3),3,' ')
  @ 21,68 SAY PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,3),3,' ')
ENDIF
*B803131,1 [End]
*E500068,1 TMI 06/26/96
lcSclStrn = XSCALE
*E500068,1 TMI 06/26/96(End)
*!*************************************************************
*! Name : lpOldFoter.
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 07/13/94.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the form footer at in the middle of a spacific 
*!            pick ticket. 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedures : Alo820E.
*!*************************************************************
PROCEDURE lpOldFoter

*B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
*@ 55,93 SAY XSTNAME
*@ 56,93 SAY XSTADDR1
*@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
*@ 57,93 SAY XSTADDR2
*@ 58,93 SAY XSTADDR3
@ 55,104 SAY XSTNAME
@ 56,104 SAY XSTADDR1
*B604206,1 AME [START] PREVENT PRINTING EMPTY PAGE
*@ 58,12 SAY '*** CONTINUED NEXT PAGE ***'
@ 57,12 SAY '*** CONTINUED NEXT PAGE ***'
*B604206,1 AME [END]
@ 57,104 SAY XSTADDR2
@ 58,104 SAY XSTADDR3
*B803131,1 [End]
*B800985,1 ASH 02/25/97 (Begin) Print the last line in the side label.

*B605882,1 BWA 04/21/2002 Variable hold the custpo all over the piktkt.[START]
*@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  
@ 63,086 SAY lcCustPO
*B605882,1 BWA 04/21/2002.[END]

*B801524,1 OWA 03/19/98 (Begin) Printed the distribution center if 
*B801524,1              exists instead of the store.
*@ 63,100 SAY XSTORE                    && TAK 05/24/94
@ 63,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
*B801524,1 OWA 03/19/98 (End) 

*B605882,1 BWA 04/21/2002 Variable hold the departement all over the piktkt.[START]
*@ 63,109 SAY ORDHDR.DEPT
@ 63,109 SAY lcDept
*B605882,1 BWA 04/21/2002.[END]

@ 63,115 SAY XORDER
@ 64,101 SAY XPIKTKT
@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
*B800985,1 ASH 02/25/97 (End)
ROW = ROW + 1

*!*************************************************************
*! Name : lpNewFoter.
*! Auth : Yasser Mohammed Aly (YMA).
*! Date : 07/13/94.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the form footer at the end of a spacific 
*!            pick ticket. 
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Called from : 
*!         Procedures : Alo820E.
*!*************************************************************
PROCEDURE lpNewFoter

*-- Not to print the total amount of the piktkt if the 
*-- user does not want to print the style prices.
@ 54,000 SAY MSGL2          
@ 55,000 SAY MSGL3          
*B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
*@ 56,093 SAY XSTNAME
@ 56,104 SAY XSTNAME

*@ 57,093 SAY XSTADDR1
@ 57,104 SAY XSTADDR1
*B803131,1 [End]
*-- YMA 08/03/94 Moved in the comments part of the form.
*-- YMA 07/13/94 Modified to chick the first character in each
*-- note to prevent printing any one of them if it has an '*'
*-- in front of it.
lnCol = 11
IF SUBSTR(OrdHdr.Note1, 1, 1) <> '*'
  @ 58,lnCol SAY OrdHdr.Note1
  lnCol = 42
ENDIF
@ 58,lnCol SAY IIF(SUBSTR(OrdHdr.Note2, 1, 1)<>'*', SUBSTR(OrdHdr.Note2,1,25), '')

*B803131,1 HBG 08/03/2000 Shifting the data to print it in its boxes[Begin]
*@ 58,076 SAY XPIECES PICTURE '999999' 
@ 58,089 SAY XPIECES PICTURE '999999' 
*@ 58,093 SAY XSTADDR2
@ 58,104 SAY XSTADDR2
*@ 59,093 SAY XSTADDR3
@ 59,104 SAY XSTADDR3
*@ 62,060 SAY SUBSTR(XSHIP_DATA,1,11) 
@ 62,070 SAY SUBSTR(XSHIP_DATA,1,11) 
*@ 62,076 SAY IIF(PRTPRICE = 'Y',XVALUE,'') PICTURE '99999.99'
@ 62,088 SAY IIF(PRTPRICE , XVALUE , '') PICTURE '99999.99'

*@ 63,086 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  

*B605882,1 BWA 04/21/2002 Variable hold the custpo all over the piktkt.[START]
*@ 62,102 SAY IIF(ORDHDR.MultiPO,lcCustPO,OrdHdr.CUSTPO)  
@ 62,102 SAY lcCustPO
*B605882,1 BWA 04/21/2002.[END]

*B801524,1 OWA 03/19/98 (Begin) Printed the distribution center if 
*B801524,1              exists instead of the store.
*@ 63,100 SAY XSTORE                    && TAK 05/24/94
*@ 63,100 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
@ 62,118 SAY IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
*B801524,1 OWA 03/19/98 (End) 

*@ 63,109 SAY ORDHDR.DEPT

*B605882,1 BWA 04/21/2002 Variable hold the departement all over the piktkt.[START]
*@ 62,126 SAY ORDHDR.DEPT 
@ 62,126 SAY lcDept
*B605882,1 BWA 04/21/2002.[END]
           
*@ 63,115 SAY XORDER
@ 62,137 SAY XORDER

*@ 64,101 SAY XPIKTKT
@ 64,117 SAY XPIKTKT
*E500068,1 TMI 06/26/96
*@ 64,108 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
@ 64,124 SAY 'SHIP VIA: ' + SUBSTR(XSHIP_DATA,1,10)
*E500068,1 TMI 06/26/96 (End)
*B803131,1 [End]
XVALUE = 0.00
ROW = ROW + 1

*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Tarek Mohamed Ismael
*! Date : 03/28/96.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

PROCEDURE lpPrtSku

IF ! SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  *B800711,1 ASH  3- If we print 2 p/t and the style of the second p/t have no
  *B800711,1 ASH     sku, leave it empty and do not print the sku of the style
  *B800711,1 ASH     in the first p/t.
  lcStrToPrn = " "
  
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  = SEEK('S'+Style.Scale,'Scale')
  = SEEK('M' + ORDHDR.Account , 'Customer')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  *B801386,1 SHA 11/24/97(Begin)Refresh the relation between the piktemp
  *B801386,1 SHA                file and the customer file.
*  GO RECNO() IN &PIKTEMP
  *B801386,1 SHA 11/24/97(End)
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1 + SkuTmpl.Len2 + SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 
  = SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style ,'Spck_Lin')
  lcStrToPrn = 'SKU#: ' + SUBSTR(Pack_Id,1,lnDime1)

  lcStrToPrn = IIF(LEN(lcStrToPrn)>65,SUBSTR(lcStrToPrn,1,65),ALLTRIM(lcStrToPrn))
  lnLength   = LEN(lcStrToPrn)+2
*  @ ROW,01 SAY lcStrToPrn
  lnLocCol = LEN(lcStrToPrn) + 5

ELSE
  @ ROW,01 SAY SUBSTR(Sku,1,8)
  lnLocCol = 35
  @ ROW,17 SAY 'CUSTOMER SKU #'
 
ENDIF
RETURN

*!***************************************************************
*! Name : lfPrnStrn.
*! Auth : Tarek Mohamed Ismael
*! Date : 03/26/96.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfPrnStrn
lcAlias=ALIAS()
lcSkuSize =' '
lcKey     =' '
IF SEEK('S' + ORDHDR.Account + &lcTmpOrdL..Style , 'Spck_Lin')
  lcKey='S' + ORDHDR.Account + &lcTmpOrdL..Style
  lnSep=1
  Q=1
  W=STR(Q,1)

  X=1
  Z=STR(X,1) 
  SELECT Spck_Lin
  SCAN REST WHILE 'S' + &lcTmpOrdL..Account + &lcTmpOrdL..Style = lcKey

    IF &LnTemp..Qty&Z > 0
*      lcSkuSize = lcSkuSize+SUBSTR(lcSclStrn,lnSep,5)+':'+SUBSTR(Pack_Id,1,8)+' '
       *B800711,1 ASH 08/14/96 1- Print the SKU by size from the character no. 9 to no. 13 
       *B800711,1 ASH             instead of 1 to 5.
       *lcSkuSize = lcSkuSize+'S'+W+':'+SUBSTR(Pack_Id,1,5)+' '
        lcSkuSize = lcSkuSize+'S'+W+':'+SUBSTR(Pack_Id,9,5)+' '
      
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

*!***************************************************************
*! Name : lfScndGrd.
*! Auth : Tarek Mohamed Ismael
*! Date : 03/26/96.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfScndGrd

SET COLOR TO &QCLRNRM
CLEAR

*lcPreDev=SET('DEVICE')
SET DEVICE TO SCREEN
DO HEADER WITH 'ALO820','Print Piktkt form'
DO GRID WITH 2,'N'
@ 03,03 SAY 'Print style location [Y/N]..................?'
@ 05,03 SAY 'Print SKU/PACK       [Y/N]..................?'
SAVE SCREEN TO lsAlo820            

DO WHILE .T.
  @ 03,49 GET lcPrnStLoc  PICTURE '@!' VALID lcPrnStLoc $ 'YN'
  @ 05,49 GET lcPrnSkPck  PICTURE '@!' VALID lcPrnSkPck $ 'YN'

  READ
  IF lcPrnSkPck='Y'
    DO GRID WITH 3,'N'
    @ 07,03 SAY 'Print SKU by size    [Y/N]..................?'
    @ 07,49 GET lcPrnSkSiz PICTURE lcPrnSkSiz VALID lcPrnSkSiz $ 'YN'
    READ 
  ELSE
    DO GRID WITH 2,'N'
    SET COLOR TO &QCLRNRM
    @ 07,00 TO 09,79 CLEAR
  ENDIF
  DO MSG22 WITH '<P>roceed  <M>odify  <C>ancel => ','PMC'

  DO CASE
    CASE CHOICE='C'
      llOk =.F.
      EXIT
    CASE CHOICE='M'
      LOOP
    CASE CHOICE='P'
      llOk =.T.
      EXIT
  ENDCASE
  
ENDDO

*!***************************************************************
*! Name : lfGetLoc.
*! Auth : Tarek Mohamed Ismael
*! Date : 03/26/96.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfGetLoc
lcAlias=ALIAS()
*B800805,1 ASH 10/27/96 (Begin) Initialize the variable to prevent carrying over of locations.
lcStr=" "
*B800805,1 ASH 10/27/96 (End)
SELECT WhsLoc
IF SEEK(&lcTmpOrdL..Style)   &&                +&LnTemp..Color)
  SCAN REST WHILE Style= &lcTmpOrdL..Style
    lcStr = lcStr +" "+cLocation
  ENDSCAN
  lcStr=ALLTRIM(lcStr)
  lnLen=LEN(lcStr)
ENDIF
SELECT (lcAlias)

*!***************************************************************
*! Name : lfContuLoc.
*! Auth : Tarek Mohamed Ismael
*! Date : 03/26/96.
*! ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ
*! Synopsis : Print the ,main style/color Skus for a specific account.
*!***************************************************************

FUNCTION lfContuLoc
FOR I = 12 TO lnLen  
  *B800711,1 ASH Increase the size of the substring.
  *@ ROW ,00 SAY SUBSTR(lcStr,I,85) 
  *I=I+85
   @ ROW ,00 SAY SUBSTR(lcStr,I,86) 
   I=I+86
   
   DO lpSidLabls
   Row = Row + 1
ENDFOR

*--------------------------------
*    END ALO820E.PRG
*--------------------------------
