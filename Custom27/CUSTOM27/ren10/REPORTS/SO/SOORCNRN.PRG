*!********************************************************************
*: Program file  : SOORCNRN.PRG                
*: Program desc. : ORDER CONFIRMATION FORMAT A FOR RENA LANGE.
*:                 This is converted from the aria26 form A ord510z.prg for Rena
*:         System: ARIA27
*:         Module: SO2
*:      Developer: TMI - TAREK MOHAMED IBRAHIM
*:       Entry ID: C102751,1
*!********************************************************************

*C102751,1 TMI [START] Suppress some options in the Order Conf. Option Grid 
*-- The expression "lcSortBy|lcPrnSizs|lcShwOptMs|lcPrnDciml|lcPrnBokQt~N|N|N|N|N" is added to the
*-- field MFRMSPSETS in syfrmcdd file
* It contains the following variables 
*-- lcSortBy      ==> to suppress Sort By option 
*-- lcPrnSizs     ==> to suppress Print Sizes option 
*-- lcShwOptMs    ==> to suppress Option message option 
*-- lcPrnDciml    ==> to suppress Show decimals option 
*-- lcPrnBokQt    ==> to suppress Print Book Qty option 

* Do not try to use these variables throgh this program or any program called by this program

*C102751,1 TMI [END  ] Suppress some options in the Order Conf. Option Grid 


*-- lnClrPos              && Color lenght in style code structur
*-- lnClrLen              && Color position in style code structur
*-- lnMajwid              && Major part width

STORE 0 TO lnClrPos,lnClrLen,lnMajwid
=lfClrData()

*--Create and fill the temp order header file 
OrdHtemp = gfTempName()
ORDLTEMP = gfTempName()
OrdPTemp = gfTempName()
CREATE TABLE (gcWorkDir+OrdHtemp) (CORDTYPE C(1),ORDER C(6) , ACCOUNT C(5))

SELECT ORDHDR
SET SKIP TO 
SET RELATION TO 
GO TOP

*--Try to make use of the files opened by soorcn.prg program
SCAN FOR EVAL(lcRpExp)
  INSERT INTO (gcWorkDir+OrdHtemp) VALUES (ORDHDR.CORDTYPE , ORDHDR.ORDER , ORDHDR.ACCOUNT )
ENDSCAN

SET DEVICE TO SCREEN

SELECT &OrdHtemp
IF RECCOUNT() = 0
  *- There are no records to display.
  =gfModalGen('QRM00052B00000','DIALOG')  
  *--Erase unneeded files
  =lfErase()
  RETURN
ENDIF

SET DEVICE TO PRINT

*-- Opening the necessary files.
=gfOpenFile (gcDataDir+'CODES','CODES','SH')
IF llRpOrdNot
  =gfOpenFile (gcDataDir+'NOTEPAD','NOTEPAD','SH')
ENDIF
*C102751,4 TMI [Start] Set order in Ordline file
SET ORDER TO ORDLINE IN ORDLINE 
*C102751,4 TMI [End  ]

= gfOpenFile (gcDataDir+'SPCK_HDR','SPCK_HDR','SH')
= gfOpenFile (gcDataDir+'SPCK_LIN','SPCK_LIN','SH')
= gfOpenFile (gcDataDir+'CUSTOMER','CUSTOMER','SH')

SELECT OrdLine
COPY STRU TO (gcWorkDir+OrdPTemp)
= gfOpenFile (gcWorkDir+OrdPTemp,' ','EX')

*-- Fill the temp ordhdr file
SELECT (OrdHtemp)
SCAN
 lcSeekOrdr = &OrdHtemp..CORDTYPE+&OrdHtemp..ORDER
 =SEEK(lcSeekOrdr,'OrdLine') 
 DO WHILE  lcSeekOrdr = OrdLine.CORDTYPE+OrdLine.ORDER
   SELECT (OrdPTemp)  
   APPEND BLANK 
   REPLACE  CORDTYPE WITH OrdLine.CORDTYPE         ,;
            ORDER    WITH OrdLine.ORDER            ,; 
            STYLE    WITH OrdLine.STYLE            ,;
            STORE    WITH OrdLine.STORE            ,;
            LINENO   WITH OrdLine.LINENO           ,;
            PACK_ID  WITH OrdLine.PACK_ID
   SELECT OrdLine
   SKIP
 ENDDO 
ENDSCAN
SELECT (OrdPTemp)

IF llRpPack
  INDEX ON CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6)+PACK_ID  TAG &OrdPTemp
ELSE 
  INDEX ON CORDTYPE+ORDER+STORE+STYLE+STR(LINENO,6) TAG &OrdPTemp
ENDIF

SELECT (OrdHtemp)
GO TOP

A= REPLICATE('*',80)
lcPrePack  = ' '
*   ....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
B='|  START  | COMPLETE |    TERMS      |   SHIPVIA     | SEASON   |REP1 REP2'
*   MM/DD/YY   MM/DD/YY  123456789012345 123456789012345 1234567890 123  123
C='STYLE            COLOR                       W.PRICE R.PRICE    PCS     AMOUNT'

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*    STYLE   CLR                                     PRICE   PIECES       AMOUNT
*123 1234567 123 12345678901234567890              1234.99   123456   123,456.99

*-------------------------------
* SETUP HEADINGS
*-------------------------------
MAXROW = 48
STORE 1 TO lnNoteLn,lnSvNoteLn 
llNrmPrt = .T.

SELE ORDHDR
GO TOP

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
NEWDOC = .T.

WAIT WINDOW NOWAIT 'PRINTING - <Space Bar> TO ABORT'

DO WHILE INKEY() <>32
   SELE &ORDHTEMP
   IF EOF()
      EXIT
   ENDIF
   IF NEWDOC
      *-- Intialize a var. to hold the page # to 
      *-- be reset with each new order
      lnPage = 1
      XTYP   = &OrdPTemp->CORDTYPE
      XORDER = &OrdPTemp->ORDER
      SELE ORDHDR
      SEEK XTYP+XORDER
      IF EOF()
         SELE &ORDHTEMP
         SKIP
         LOOP
      ENDIF
      XMULTI = MULTI
      XACCOUNT = ORDHDR->ACCOUNT
      SELE ORDLINE
      SEEK XTYP+XORDER
      IF EOF()
         SELE &ORDHTEMP
         SKIP
         LOOP
      ENDIF
      IF USED(ORDLTEMP)   
         USE IN &ORDLTEMP
      ENDIF
      SELE ORDLINE

      IF XMULTI = 'Y'
        COPY REST TO  (gcWorkDir+ORDLTEMP) FOR CORDTYPE+ORDER = XTYP+XORDER
         = gfOpenFile (gcWorkDir+OrdLTemp,' ','EX')  
        IF llRpPack
          INDEX ON STORE+STYLE+STR(LINENO,6)+PACK_ID TAG &ORDLTEMP
        ELSE 
          INDEX ON STORE+STYLE+STR(LINENO,6) TAG &ORDLTEMP
        ENDIF

        GO TOP

        XSTORE = STORE
        SELE CUSTOMER
        SEEK 'S'+XACCOUNT+XSTORE
        XBTNAME  = BTNAME
        *C102751,4 TMI [Start] 
        *XBTADDR1 = CADDRESS1 
        *XBTADDR2 = CADDRESS2 
        *XBTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
        XBTADDR1 = CADDRESS12
        XBTADDR2 = CADDRESS22 
        XBTADDR3 = ALLTRIM(PADR(CADDRESS32,15)) + ' ' +ALLT(PADR(CADDRESS42,3)) + ' ' + ALLT(PADR(CADDRESS52,10))
        *C102751,4 TMI [End  ] 
        IF LEN(TRIM(XBTADDR2)) =0
          XBTADDR2 = XBTADDR3
          XBTADDR3 = ''
        ENDIF

        *-- Initialize the alt address if ther is any. 
        SELE OrdHdr
        SEEK XTYP+XORDER
        IF Alt_ShpTo
          XSTNAME  = STNAME
          XSTADDR1 = CADDRESS1 
          XSTADDR2 = CADDRESS2
          XSTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
          IF LEN(TRIM(XSTADDR2)) =0
            XSTADDR2 = XSTADDR3
            XSTADDR3 = ''
          ENDIF
        ELSE
          SELE Customer
          XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
          XSTADDR1 = CADDRESS1
          XSTADDR2 = CADDRESS2
          XSTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
          IF LEN(TRIM(XSTADDR2)) =0
            XSTADDR2 = XSTADDR3
            XSTADDR3 = ''
          ENDIF
        ENDIF

      ENDIF           && IF MULTI STORE

      SELE ORDHDR
      *------------------------------------
      * GET DESCRIPTIONS FOR CODED FIELDS
      *------------------------------------
      lcTermDesc = ALLTRIM(gfCodDes(OrdHdr.CTERMCODE,'CTERMCODE'))
      *C102751,4 TMI [Start] If wieght exceeds break wieght use alternat ship via
      *lcShipDesc = ALLTRIM(gfCodDes(OrdHdr.SHIPVIA,'SHIPVIA'))
      lcShipDesc = lfShipVia()
      *C102751,4 TMI [End  ] 
      lcDivDesc  = ALLTRIM(gfCodDes(OrdHdr.CDIVISION,'CDIVISION'))
      lcSeaDesc  = ALLTRIM(gfCodDes(OrdHdr.SEASON,'SEASON'))
      
      ****** IF NOT MULTIPLE STORE ORDER GET SHIPTO AND BILLTO
      XSTORE   = SPACE(8)

      IF XMULTI <> 'Y'
      
        SELE CUSTOMER
      
        SEEK IIF(ORDHDR->STORE=SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+ORDHDR->STORE)

        XBTNAME  = BTNAME
        *C102751,4 TMI [Start] 
        *XBTADDR1 = CADDRESS1 
        *XBTADDR2 = CADDRESS2 
        *XBTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
        XBTADDR1 = CADDRESS12 
        XBTADDR2 = CADDRESS22 
        XBTADDR3 = ALLTRIM(PADR(CADDRESS32,15)) + ' ' +ALLT(PADR(CADDRESS42,3)) + ' ' + ALLT(PADR(CADDRESS52,10))
        *C102751,4 TMI [End  ] 
        IF LEN(TRIM(XBTADDR2)) =0
          XBTADDR2 = XBTADDR3
          XBTADDR3 = ''
        ENDIF

        XSTORE   = STORE
        *-- Initialize the alt address if ther is any. 04/07/94 MFM.
        SELE OrdHdr
        SEEK XTYP+XOrder
        IF Alt_ShpTo
          XSTNAME  = STNAME
          XSTADDR1 = CADDRESS1
          XSTADDR2 = CADDRESS2
          XSTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
         IF LEN(TRIM(XSTADDR2)) =0
            XSTADDR2 = XSTADDR3
            XSTADDR3 = ''
          ENDIF
        ELSE
          SELE Customer
          XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
          XSTADDR1 = CADDRESS1
          XSTADDR2 = CADDRESS2
          XSTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
          IF LEN(TRIM(XSTADDR2)) =0
            XSTADDR2 = XSTADDR3
            XSTADDR3 = ''
          ENDIF
        ENDIF
      ENDIF

      NOTELOOP = .F.

      XTOTQTY  = 0.00
      XTOTORD  = 0.00
   ENDIF &&& NEWDOC

   DO WHILE  XMULTI = 'Y'
      IF (&ORDLTEMP->STORE=XSTORE)  .OR. (NOTELOOP)
        EXIT
      ENDIF
      XSTORE   = &ORDLTEMP->STORE
      XACCOUNT = &ORDLTEMP->ACCOUNT
      SELE CUSTOMER
      SEEK 'S'+XACCOUNT+XSTORE
      XBTNAME  = BTNAME
      *C102751,4 TMI [Start]       
      *XBTADDR1 = CADDRESS1 
      *XBTADDR2 = CADDRESS2 
      *XBTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
      XBTADDR1 = CADDRESS12 
      XBTADDR2 = CADDRESS22 
      XBTADDR3 = ALLTRIM(PADR(CADDRESS32,15)) + ' ' +ALLT(PADR(CADDRESS42,3)) + ' ' + ALLT(PADR(CADDRESS52,10))
      *C102751,4 TMI [End  ] 
      IF LEN(TRIM(XBTADDR2)) =0
        XBTADDR2 = XBTADDR3
        XBTADDR3 = ''
      ENDIF
      XSTNAME  = IIF( EMPTY(DBA),STNAME,DBA)
      XSTADDR1 = CADDRESS1
      XSTADDR2 = CADDRESS2
      XSTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
      IF LEN(TRIM(XSTADDR2)) =0
        XSTADDR2 = XSTADDR3
        XSTADDR3 = ''
      ENDIF
      XTOTQTY  = 0.00
      XTOTORD  = 0.00
      NOTELOOP = .F.
      EXIT
   ENDDO

   SELE ORDHDR
   IF XMULTI = 'Y'
     IF !USED(ORDLTEMP)
       = gfOpenFile (gcWorkDir+OrdLTemp,' ','EX') 
     ENDIF
     SELECT &ORDLTEMP
     IF Totqty <=0
       NEWDOC = .F.
       IF EOF() .AND. !NOTELOOP
         EXIT
       ELSE
         IF !EOF() 
           SKIP
           SKIP IN &OrdPTemp
           LOOP
         ENDIF        
       ENDIF
     ENDIF
   ENDIF

   *-- (Begin) The following lines are commented
   *-- out and replaced by the company logo.
   *-- Printing the R and L letters with different 
   *-- font when the device is printer by sending the 
   *-- escape sequece of the printer that allows
   *-- changing the font.   

   @ 07,25 SAY 'O R D E R   P R I N T   O U T'
   
   SELE ORDHDR
   @ 09,03 SAY 'ORDER: '+ORDER
   @ 09,60 SAY 'DATE: '
   @ 09,66 SAY ENTERED
   @ 10,60 SAY 'PO #: '
   @ 10,66 SAY IIF(MultiPO,&ORDLTEMP->CUSTPO,CUSTPO)

   @ 12,04 SAY '........ SOLD TO ........'
   @ 12,41 SAY '.............. SHIP TO ..............'
   @ 13,04 SAY ACCOUNT
   *C102751,4 TMI [Start] 
   *@ 13,46 SAY 'STORE: ' + XSTORE + '  DEPT: ' + DEPT   
   @ 13,41 SAY 'STORE:' + XSTORE + ;
               IIF(!EMPTY(CUSTOMER.DIST_CTR),' DC:' + CUSTOMER.DIST_CTR,'') + ' DEPT:' + DEPT
   IF !EMPTY(CUSTOMER.DIST_CTR)
     =SEEK('S'+ORDHDR.ACCOUNT+CUSTOMER.DIST_CTR,'CUSTOMER')
     SELECT CUSTOMER
     XSTNAME = STNAME
     XSTADDR1 = CADDRESS1 
     XSTADDR2 = CADDRESS2
     XSTADDR3 = ALLTRIM(PADR(CADDRESS3,15)) + ' ' +ALLT(PADR(CADDRESS4,3)) + ' ' + ALLT(PADR(CADDRESS5,10))
     IF LEN(TRIM(XSTADDR2)) =0
       XSTADDR2 = XSTADDR3
       XSTADDR3 = ''
     ENDIF
     SELECT ORDHDR
   ENDIF
   *C102751,4 TMI [End  ] 
  
   @ 14,04 SAY XBTNAME
   @ 14,41 SAY XSTNAME

   @ 15,04 SAY XBTADDR1
   @ 15,41 SAY XSTADDR1

   @ 16,04 SAY XBTADDR2
   @ 16,41 SAY XSTADDR2

   @ 17,04 SAY XBTADDR3
   @ 17,41 SAY XSTADDR3

   @ 19,00 SAY A
   @ 20,00 SAY B
   @ 21,00 SAY A

   @ 22,01 SAY START
   @ 22,12 SAY COMPLETE
   @ 22,22 SAY lcTermDesc
   @ 22,38 SAY lcShipDesc
   @ 22,54 SAY lcSeaDesc
   @ 22,65 SAY REP1
   @ 22,70 SAY REP2

   @ 23,00 SAY A
   @ 24,00 SAY C
   @ 25,00 SAY A

   ****** START PRINTING THE ORDER LINES
   ROW = 26
   IF !EMPTY(Note1) .AND. SUBSTR(Note1,1,1) <> '*'      
     @ ROW,00 SAY  SUBSTR(Note1,1,17)
   ENDIF       
   
   NEWDOC   = .T.
   XSCALE   = ' '

   XPREVSCALE = ' ' 
   XCOUNT     = 0

   IF XMULTI = 'Y'
     SELE &ORDLTEMP
     IF llRpPack
       =SEEK(&OrdPTemp->STORE+;
       &OrdPTemp->STYLE+STR(&OrdPTemp->LINENO,6)+&OrdPTemp->PACK_ID)      
     ELSE 
       =SEEK(&OrdPTemp->STORE+;
       &OrdPTemp->STYLE+STR(&OrdPTemp->LINENO,6))
     ENDIF
   ELSE
     SELE ORDLINE
   ENDIF

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*LN#   GRP STYLE           CLR                        PRICE   PIECES       AMOUNT
*123456 X  123456789012345 123 12345678901234567890 1234.99   123456   123,456.99
*            11111 22222 33333 44444 55555 66666 77777 88888
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

   DO WHILE .T.

      DO CASE
        CASE EOF() .OR. &OrdPTemp->CORDTYPE+&OrdPTemp->ORDER <> XTYP+XORDER
          NEWDOC   = .T.
          EXIT
        CASE STORE # XSTORE .AND. XMULTI = 'Y'
          NEWDOC = .F.
          EXIT
        CASE TOTQTY<=0
          SKIP
          SKIP IN (OrdPTemp)  
          LOOP
        CASE ROW>=MAXROW
          lcPrePack  = ' '
          NEWDOC = .F.
          EXIT
      ENDCASE
      
      @ ROW,18 SAY ALLT(gfCodDes(SUBSTR(STYLE,lnClrPos,lnClrLen),'COLOR'))

      ROW=ROW+1

      KEY = STYLE
      SELE STYLE
      SEEK KEY
      XSTYDESC = DESC
      
      *-- Added XPATTERN to print after the style#
      XPATTERN=PATTERN
      
      IF XMULTI = 'Y'
        IF llRpPack
          =SEEK(&OrdPTemp->STORE+&OrdPTemp->STYLE+STR(&OrdPTemp->LineNo,6)+&OrdPTemp->PACK_ID,'&ORDLTEMP')
        ELSE
          =SEEK(&OrdPTemp->STORE+&OrdPTemp->STYLE+STR(&OrdPTemp->LineNo,6),'&ORDLTEMP')
        ENDIF
        =SEEK(&OrdPTemp->CORDTYPE+&OrdPTemp->ORDER+STR(&OrdPTemp->LineNo,6),'OrdLine')
      ENDIF
      =SEEK('P'+XACCOUNT+OrdLine.PACK_ID+&OrdPTemp->Style,'SPCK_LIN')
      IF !SEEK('P'+XACCOUNT+OrdLine.PACK_ID,'SPCK_HDR')
         =SEEK('P'+'*****'+OrdLine.PACK_ID,'SPCK_HDR')
      ENDIF
      IF OrdLine.PACK_ID <> lcPrePack .AND. llRpPack
        lcOldAlias = ALIAS()
        SELECT SPCK_LIN

        SUM REST WHILE OrdLine.PACK_ID = SPCK_LIN.PACK_ID SPCK_LIN.QTY1,;
                 SPCK_LIN.QTY2,SPCK_LIN.QTY3,SPCK_LIN.QTY4,;
                 SPCK_LIN.QTY5,SPCK_LIN.QTY6 TO ARRAY lnQty
        SELECT (lcOldAlias)
        lcPrePack = OrdLine.PACK_ID
        IF !EMPTY(OrdLine.PACK_ID)
          @ ROW,00 SAY 'PACK_ID : '+OrdLine.PACK_ID+'  '+'DESC.:'+SPCK_HDR.DESC+' ' +;
                       +STR(lnQty[1],3)+' '+ STR(lnQty[2],3)+' '+STR(lnQty[3],3)+' ' +;
                       STR(lnQty[4],3)+' '+STR(lnQty[5],3)+' '+STR(lnQty[6],3)
          ROW=ROW+1
        ENDIF 
      ENDIF
      IF SCALE<>XPREVSCALE .AND. llNrmPrt
         XPREVSCALE=SCALE 
         XSCALE   = GETSCALE(STYLE->SCALE,SPACE(1) )
         XCOUNT   = SCALE->CNT
         IF XCOUNT > 1
            @ ROW,18 SAY XSCALE        &&-DOUBLE PRINT BOLD
            ROW = ROW+1
         ENDIF
      ENDIF

      IF XMULTI = 'Y'
        SELE &ORDLTEMP
      ELSE
        SELE ORDLINE
      ENDIF

      IF llNrmPrt
        @ ROW,00 SAY ALLTRIM(alltrim(SUBSTR(STYLE,1,lnMajwid))+substr(XPATTERN,1,5)) 
        @ ROW, 18 SAY SUBSTR(STYLE,lnClrPos,5)
        *-- As per Tanya of Rena Langa, they do not use color codes more than 3 char
        
        @ ROW,24 SAY XSTYDESC
        @ ROW,46 say price picture "@$ 99,999"
        IF SEEK(Style, 'Style')
          @ ROW,54 say Style.PriceB picture "@$ 99,999" 
        ENDIF
        @ ROW,61 SAY TOTQTY   PICTURE '999999'        
        XAMOUNT= TOTQTY * PRICE
        @ ROW,71 SAY xamount picture "@$ 999,999"
               
        ROW =ROW+1

        X = 1
        DO WHILE XCOUNT >1 .AND. X<=8
          Z =STR(X,1)
          @ ROW,(X*6)+12 SAY QTY&Z   PICTURE '@Z 99999'
          X = X+1
        ENDDO
        ROW = ROW+1
        * SUM ORDER TOTALS
        XTOTQTY  = XTOTQTY+ TOTQTY
        XTOTORD  = XTOTORD+ XAMOUNT
      ENDIF
      *IF llLnNote .AND. lcPrOLNt= 'Y' .AND. MEMLINES(Note_Mem) <> 0 .AND. ! lfPrtONte() 
      IF llRpOrdLnt .AND. MEMLINES(Note_Mem) <> 0 .AND. ! lfPrtONte() 
      
        llNrmPrt = .F.
        EXIT
      ELSE
        llNrmPrt = .T.
      ENDIF
      ROW = ROW+1
      SELECT (OrdPTemp) 
      SKIP

      IF XMULTI = 'Y'
        SELE &ORDLTEMP
      ELSE
        SELE ORDLINE
      ENDIF
      SKIP
   ENDDO
   *------------------------ END PRINT LINE LOOP ----------------------

   IF (NEWDOC .AND. !NOTELOOP .AND. llRpOrdNot) .OR. ;
     (XMULTI='Y' .AND. (!NOTELOOP) .AND. (!NEWDOC) .AND. ;
      XSTORE <> &ORDLTEMP->STORE .AND. llRpOrdNot)
     SELE NOTEPAD
     SEEK 'B'+XORDER
     NOTELOOP = IIF(FOUND(), .T., .F.)
     lnMline = 1
   ENDIF

   *---------------------------------------------------
   * [NOTEPAD] COMMENT LINES
   *---------------------------------------------------
   
   IF llRpOrdNot
     lnMemWidth = SET("MEMOWIDTH")
     SET MEMOWIDTH TO 75
     lnMTotLin = MEMLINES(NOTEPAD.MNOTES)
   ENDIF
   DO WHILE NOTELOOP .AND. llRpOrdNot
     SELE NOTEPAD
     IF lnMline > lnMTotLin
       IF XMULTI='Y' .AND. (XORDER=&ORDLTEMP->ORDER)
         NEWDOC   = .F.
       ELSE
         NEWDOC   = .T.
       ENDIF
       NOTELOOP = .F.
       EXIT
     ENDIF
     IF ROW >= MAXROW
       NEWDOC   = .F.
       NOTELOOP = IIF(lnMline = lnMTotLin,.F.,.T.)   
       EXIT
     ENDIF
     IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)  
       @ ROW,04 SAY MLINE(MNOTES,lnMline)
       ROW = ROW+1
     ENDIF
     lnMline = lnMline + 1
   ENDDO
   IF llRpOrdNot
     SET MEMOWIDTH TO lnMemWidth
   ENDIF  
   IF ((XMULTI='Y' .AND. XSTORE= &ORDLTEMP->STORE .AND. !NEWDOC ).OR. NOTELOOP ) .OR. ;
   (XMULTI <> 'Y' .AND. !NEWDOC)
     @ 50,00 SAY REPLICATE('-',80)
     @ 51,13 SAY 'C O N T I N U E D ...'
     @ 51,69    SAY '**********'
     @ 52,00 SAY REPLICATE('-',80)
     *-- Print the page #
     @ 56,1 SAY PADC("PAGE # " + STR(lnPage,2),80," ") 
     lnPage = lnPage + 1
     *-- Printing the company name and address
     *-- by sending the escape sequence of compressing and bold
     LOOP
   ENDIF

   XMER_DISC = ORDHDR->DISC
   XMER_AMNT = (XMER_DISC * XTOTORD)/100
   XNET_AMNT = XTOTORD - XMER_AMNT
   @ 49,00 SAY A

   @ 51,10 SAY 'TOTAL AMOUNT      =>'
   @ 51,61 SAY XTOTQTY PICTURE '999999'
   @ 51,71 SAY XTOTORD picture "@$ 999,999"
   @ 53,00 SAY A
   @ 54,22 SAY '______________________________________   ____________'
   @ 55,22 SAY 'CUSTOMER APPROVAL SIGNATURE                  DATE    '
   *-- Print the page #
   @ 56,1 SAY PADC("PAGE # " + STR(lnPage,2),80," ") 
   lnPage = lnPage + 1
   IF !NEWDOC
      LOOP
   ENDIF

   SELE &ORDHTEMP
   SKIP
   IF EOF()
      EXIT
   ENDIF

ENDDO

DO ENDREPORT
SET DEVICE TO SCREEN

*--Erase unneeded files
=lfErase()

*:**************************************************************************
*:* Name        : lfErase
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/04/2002
*:* Purpose     : Erase unneeded files
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfErase()
*:***************************************************************************
FUNCTION lfErase

*** <<<<< Remove temp created files >>>>>
IF USED(OrdHtemp)
  USE IN (OrdHtemp)
ENDIF
ERASE (gcWorkDir+OrdHtemp+'.DBF')
ERASE (gcWorkDir+OrdHtemp+'.CDX')

IF USED(OrdPTemp)
  USE IN (OrdPTemp)
ENDIF
ERASE (gcWorkDir+OrdPTemp+'.DBF')
ERASE (gcWorkDir+OrdPTemp+'.CDX')

IF USED(ORDLTEMP)
  USE IN (ORDLTEMP)
ENDIF
ERASE (gcWorkDir+ORDLTEMP+'.DBF')
ERASE (gcWorkDir+ORDLTEMP+'.CDX')

*-- end of lfErase.


*!*************************************************************
*! Name      : lfPrtONte
*! Developer : KHALD MOHI EL-DIN
*! Date      : 09/07/1997
*! Purpose   : Print Order line notes.
*!*************************************************************
*! Calls     :
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!*************************************************************
*! Example            :  lfPrtONte()
*!*************************************************************
FUNCTION lfPrtONte

ROW = ROW-1
lnNoteLn = lnSvNoteLn 
DO WHILE lnNoteLn <= MEMLINES(Note_Mem)
  IF ROW >= MaxRow - 1
    NewDoc   = .F.
    lnSvNoteLn = lnNoteLn
    RETURN(.F.)
  ELSE
    ROW = ROW + 1
    @ ROW,01 SAY MLINE(Note_Mem,lnNoteLn)
  ENDIF
  lnNoteLn = lnNoteLn + 1
ENDDO
IF lnNoteLn > MEMLINES(Note_Mem)
  ROW = ROW + 1
ENDIF
lnSvNoteLn = 1
RETURN(.T.)



*:**************************************************************************
*:* Name        : lfClrData
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 10/31/2002
*:* Purpose     : Get color position and lenght from icistru
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfClrData()
*:***************************************************************************
FUNCTION lfClrData
PRIVATE laSegVal,lnI
DIMENSION laMajSeg[1]
=gfItemMask(@laMajSeg)
FOR lnI = 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = 'C'
    lnClrPos = laMajSeg[lnI,4]
    lnClrLen = LEN(laMajSeg[lnI,3])
    EXIT
  ENDIF
ENDFOR

lnMajwid = LEN(gfItemMask('PM'))
*-- end of lfClrData.

*:**************************************************************************
*:* Name        : GETSCALE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/04/2002
*:* Purpose     : Get Scales
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = GETSCALE()
*:***************************************************************************
PROCEDURE GETSCALE
PARAMETERS XSC,XSPACE
PRIVATE XOLDALIAS,X,XSTRING,XSC,XSPACE,Z

XOLDALIAS= ALIAS()
=gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')

SELE SCALE
SEEK 'S'+XSC
X       = 1
XSTRING = ''

DO WHILE FOUND() .AND. X<=CNT
  Z = STR(X,1)
  XSTRING = XSTRING + PADL(ALLT(SZ&Z),5) + IIF(X=CNT,'',XSPACE)
  X= X + 1
ENDDO

IF .NOT. FOUND()
  XSTRING ='* * * E R R O R * * *'
ENDIF

IF LEN(TRIM(XOLDALIAS)) > 0
  SELE &XOLDALIAS
ENDIF

RETURN(XSTRING)

*:**************************************************************************
*:* Name        : lfShipVia                                       *C102751,4 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/26/2002
*:* Purpose     :  If wieght exceeds break wieght use alternat ship via
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfShipVia()
*:***************************************************************************
FUNCTION lfShipVia
PRIVATE lnWght,lcRet, lnCust,lnOrdLn,lnStyl

lnCust = RECNO('CUSTOMER')
lnOrdLn = RECNO('ORDLINE')
lnStyl = RECNO('STYLE')

=SEEK(IIF(EMPTY(ORDHDR.STORE),'M','S')+ORDHDR.ACCOUNT+ORDHDR.STORE,'CUSTOMER')
IF CUSTOMER.NBRKWEIGHT <= 0
  lcRet = ALLTRIM(gfCodDes(OrdHdr.SHIPVIA,'SHIPVIA'))
ELSE
  SELECT ORDLINE
  SET RELATION TO STYLE INTO STYLE
  SEEK ORDHDR.CORDTYPE+ORDHDR.ORDER
  lnWght = 0
  SUM ORDLINE.TOTQTY*STYLE.NSTYWEIGHT FOR ORDLINE.CORDTYPE+ORDLINE.ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER;
      TO lnWght
  IF lnWght > CUSTOMER.NBRKWEIGHT AND !EMPTY(CUSTOMER.CALTSHPVIA)
    lcRet = ALLTRIM(gfCodDes(CUSTOMER.CALTSHPVIA,'SHIPVIA'))
  ELSE
    lcRet = ALLTRIM(gfCodDes(OrdHdr.SHIPVIA,'SHIPVIA'))
  ENDIF
  SET RELATION OFF INTO STYLE
  IF BETWEEN(lnCust,1,RECCOUNT('CUSTOMER'))
    GOTO (lnCust) IN CUSTOMER
  ENDIF
  IF BETWEEN(lnOrdLn,1,RECCOUNT('ORDLINE'))
    GOTO (lnOrdLn) IN ORDLINE
  ENDIF
  IF BETWEEN(lnStyl,1,RECCOUNT('STYLE'))
    GOTO (lnStyl) IN STYLE
  ENDIF
ENDIF

RETURN lcRet
*-- end of lfShipVia.
