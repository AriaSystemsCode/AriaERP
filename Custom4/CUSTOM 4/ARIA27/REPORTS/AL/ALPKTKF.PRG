*:***************************************************************************
*: Program file  : ALPKTKF.PRG
*: Program desc. : PICK TICKET FORM "F".
*! Date          : 
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : 
*: Tracking Job Number: 
*: 
*:***************************************************************************
*: Calls : 
*:    Procedures : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKF
*:***************************************************************************
*: Modifications
*:***************************************************************************
*:B602847,1 KHM 04/29/99 Fixed the bug of not displaying a message when
*:B602847,1              there were no records to print.
*:B802227,1 BWA Get the customer addresses from the right fields
*:B802479,1 HDM 08/02/1999 Assign phone value from main program
*:B802520,1 BWA 15/08/1999 Fix the bug of always print the company name and
*:B802520,1                address even if the parameter of "Print company name"  is setup to NO.
*:B803010,1  SAM 01/30/2000 Fix Bug of not printing color code.
*:                          and position header of report (not included in entry)
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*:B604170,1 MHM 02/04/2001 Fix the bug of Picking Ticket Form "F" does not have a page break  
*:B604666,1 ASH 07/12/2001 Fix the bug of some lines missing while printing more than one P/T.	
*:B804416,1 HBG 11/09/2001 Wrong printing for P/T in case of multi store orders.
*:B804423,1 HBG 18/09/2001 Fix bug of printing the price even if Print style prices option = "NO"
*:B604682,1 BWA 23/04/2002 Fix bug of printing footer in a new page when there is notepad.
**************************************************************************** 

*******************  From Original *******************
*-- We Will use the temp file of pktkt.prg to make this report work
lcStyTitl = gfItemMask("HI")
*B804416,1 HBG 11/09/2001 print order notepad according to the option of Print order notepad
*B804416,1                not Print order line notepad [Begin]
*PRTNPAD = IIF(llRpOrdLNt , 'Y' , 'N')
PRTNPAD = IIF(llRpOrdNot , 'Y' , 'N')
*B804416,1 [End]
*B804423,1 HBG 18/09/2001 Use the value of Print style prices option in OG [Begin]
*PRTPRICE ='Y'
PRTPRICE = IIF(llRpStyPrc,'Y','N')
*B804423,1 [End}
STORE 0.00 TO XORDTOT, XVALUE , SVALUE , XTOTQTY
STORE .F. TO llNoRec
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4

*-- B802479,1 [Start] 
*XPHONE = laCompAdd[6]
XPHONE = lcXphone
*-- B802479,1 [End]

*B604682,1 BWA 23/04/2002 Initial the variable to be global in the program.[START]
lcBar ='|'
*B604682,1 BWA 23/04/2002.[END]

HLINE5 = XPHONE
MSGL1 = lcRpMsg1
MSGL2 = lcRpMsg2
MSGL3 = lcRpMsg3
IF LEN(HLINE3) = 0
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ' '    TO HLINE5
ENDIF

*B802520,1 BWA 15/08/1999 [start]
HLINE2 = IIF(llPrntComp,HLINE2,'')
HLINE3 = IIF(llPrntComp,HLINE3,'')
HLINE4 = IIF(llPrntComp,HLINE4,'')
*B802520,1 [END]

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
RELEASE ALL LIKE M_*

 A='--------------------------------------------------------------------------------'
 B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER   |'
B1='                |        |                  |          |       |               |'
 IF XDYELOT_S
   *G000000,1 HISH 08/17/95. (Begin).
   *C='   G STYLE   CLR     DESCRIPTION              DYELOT #        PRICE      AMOUNT'

   *B803010,1 [Start] reconstruct headr (not in entery) 
   *C='   G STYLE        COLOR     DESCRIPTION       DYELOT #        PRICE      AMOUNT'
   *B804423,1 HBG 18/09/2001 Print the price & Amount in header according to 
   *B804423,1                the value of Print style prices option [Begin]
   IF PRTPRICE ='Y'
   *B804423,1 [End]
     C='   G      ' + lcStyTitl + ' DESCRIPTION          DYELOT #    PRICE     AMOUNT'
   *B804423,1 HBG 18/09/2001 Print the price & Amount in header according to 
   *B804423,1                the value of Print style prices option [Begin]
   ELSE
     C='   G      ' + lcStyTitl + ' DESCRIPTION          DYELOT #                    '
   ENDIF  
   *B804423,1 [End]   
   *B803010,1 [End]
   *G000000,1 (End).
 ELSE
   *G000000,1 HISH 08/17/95. (Begin).
   *C='   G STYLE   CLR     DESCRIPTION                              PRICE      AMOUNT'
   *B803010,1 [Start] reconstruct headr (not in entery) 
   *C='   G STYLE        COLOR     DESCRIPTION                       PRICE      AMOUNT'
   *B804423,1 HBG 18/09/2001 Print the price & Amount in header according to 
   *B804423,1                the value of Print style prices option [Begin]
   IF PRTPRICE ='Y'
   *B804423,1 [End] 
     C='   G      ' + lcStyTitl + ' DESCRIPTION                     PRICE      AMOUNT'
   *B804423,1 HBG 18/09/2001 Print the price & Amount in header according to 
   *B804423,1                the value of Print style prices option [Begin]
   ELSE
     C='   G      ' + lcStyTitl + ' DESCRIPTION                                       '
   ENDIF
   *B804423,1 [End]   
   *B803010,1 [End]
   *G000000,1 (End).
 ENDIF
D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___  ___  ___ ____'
E='|  BILL OF LADING | # CARTONS | WEIGHT|SHIPPED VIA| PACKED BY | # PCS PACKED  |'
F='|                 |           |       |           |           |               |'
G='|  MERCHANDISE    |  FREIGHT  | INSUR | TERMS     | PICKED BY | CHECKED BY    |'
H='|  $              | $         | $     |           |           |               |'
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
   
** DYELOT # IS PRINTED ONLY FOR SYSTEMS USING DYELOTS ***
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*   G STYLE   CLR     DESCRIPTION              DYELOT #         PRICE      AMOUNT
*12 X 1234567 123 12345678901234567890         XXXXXXXXXX     1234.99  123,456.99
*G000000,1 HISH 08/17/95. (Begin).
*   G STYLE        COLOR     DESCRIPTION       DYELOT #         PRICE      AMOUNT
*12 X 123456789012 123456 12345678901234567890 XXXXXXXXXX     1234.99  123,456.99
*G000000,1 (End)
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
SELECT ORDHDR
lcWDesc  = CWARECODE
XBTNAME  = CUSTOMER->BTNAME

*B802227,1 BWA Change the fields of customer 26 to customer 27
*XBTADDR1 = CUSTOMER->BTADDR1
*XBTADDR2 = CUSTOMER->BTADDR2
*XBTADDR3 = TRIM(CUSTOMER->BTCITY) + ' ' +TRIM(CUSTOMER->BTSTATE) +;
' ' + CUSTOMER->BTZIP


XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2') 
XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
*B802227,1 BWA [END]

IF LEN(TRIM(XBTADDR2)) =0
  XBTADDR2 = XBTADDR3
  XBTADDR3 = ''
ENDIF

NEWDOC = .T.

CLEAR TYPEAHEAD
XTIME = TIME()
SET DEVICE TO PRINT

SELECT (lcTmpOrdL)
GO TOP

*DO WHILE INKEY() <>32
SCAN

SELECT (lcTmpOrdL)
   *----------------------------
   * GET ORDERHDR & LINE ITEMS
   *----------------------------
   IF NEWDOC
      lcOldSize = SPACE(7)
      GPIECES  = 0
      *SELECT &PIKTEMP
      XPIKTKT  = PIKTKT
      XORDER   = ORDER
      XDATE    = PIKDATE
      *B804416,1 HBG 11/09/2001 Get the store from (lcTmpOrdL) because in case of order 
      *B804416,1                multi store , ORDHDR.STORE is empty [Begin]
      *XSTORE   = ORDHDR.STORE
      XSTORE   = STORE
      *B804416,1 [End]

      *LORDER   = IIF(XORDER < LORDER, XORDER, LORDER)
      *HORDER   = IIF(XORDER > HORDER, XORDER, HORDER)

      *B800087,1 TAK 08/20/95
      IF llWareHous .AND. SEEK(ORDHDR->cWareCode,'WareHous')
        lcWDesc = 'WARHSE: '+ WareHous.cWareCode+ ' '+SUBSTR(WareHous.cDesc,1,18)
      ELSE
        lcWDesc = SPACE(1)
      ENDIF

      SELECT ORDHDR
      XMULTI   = MULTI
      XACCOUNT = ACCOUNT

      SELECT CUSTOMER
      *SEEK 'M'+XACCOUNT

      STORE 0.00 TO XORDTOT, XVALUE , SVALUE

      SELECT ORDLINE

      ******
      ** We added the STORE# to use the new created index (ORDLINST) which
      ** consists of ORDER+STORE+STYLE+COLOR+STR(LINENO,6) in order to make
      ** the search faster because one PIKTKT can't exceed one STORE. This
      ** will work both form single or multi store orders. In single store
      ** order, either the store will be empty or have something and all
      ** the lines will have the same store. So this change will not improve
      ** anything on the single store orders but it will on big multistore
      ** orders. And in most cases the single store orders are not that big
      ** anyway.
      ******
      *B804416,1 HBG 11/09/2001 Set the correct index and fix the seek expresion [Begin]
      *SEEK XORDER+XSTORE  
      lcOldOrd = ORDER() 
      SET ORDER TO Ordlinst
      SEEK 'O'+XORDER+XSTORE  
      *B804416,1 [End]
      
      IF ORDHDR->MultiPO       &&TAK 08/22/94
        lcCustPO = CustPO
      ENDIF 
      
      *B804416,1 HBG 11/09/2001 Set the old index again [Begin]
      SET ORDER TO lcOldOrd 
      *B804416,1 [End] 
      SELECT ORDHDR
      XBTNAME  = CUSTOMER->BTNAME
      
      *B802227,1 BWA Get the customer addresses from the right fields
      *XBTADDR1 = CUSTOMER->BTADDR1
      *XBTADDR2 = CUSTOMER->BTADDR2
      *XBTADDR3 = TRIM(CUSTOMER->BTCITY) + ' ' +TRIM(CUSTOMER->BTSTATE) +;
       ' ' + CUSTOMER->BTZIP
       
      XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2') 
      XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
      XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
      *B802227,1 BWA [END]
      
      IF LEN(TRIM(XBTADDR2)) =0
         XBTADDR2 = XBTADDR3
         XBTADDR3 = ''
      ENDIF

      *-- GET DESCRIPTIONS FOR CODED FIELDS [start]
      SELECT CODES
      SET ORDER TO CODES IN CODES 
  
      *TERMS
      XTERMS = gfCodDes(ORDHDR->CTERMCODE , 'CTERMCODE')

      *
      XSHIPVIA = gfCodDes(ORDHDR->SHIPVIA , 'SHIPVIA')

      *SPC INST.
      XSPCINST = gfCodDes(ORDHDR->SPCINST , 'SPCINST')

      SEEK 'Z' + ORDHDR->SEASON
      XSEASON  = gfCodDes(ORDHDR->SEASON , 'SEASON')
      
      HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)      
      
      *B802520,1 BWA 15/08/1999 [start]
      HLINE1 = IIF(llPrntComp,HLINE1,'')
      *B802520,1 [END]  

      STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4

      XPIECES = 0
      *-- GET DESCRIPTIONS FOR CODED FIELDS [end]
   ENDIF
   *-------------------- END PICK TKT/ORDER SELECTION ---------------
   *-- Initialize the alt address if ther is any.
   SELE OrdHdr

   *B801524,1 OWA 03/19/98 (Begin) Defined variable to hold the distribution 
   *B801524,1              center.
   lcDistCntr = ""
   *B801524,1 OWA 03/19/98 (End) 


   IF Alt_ShpTo
     XSTNAME  = STNAME
     
     *B802227,1 BWA Get the customer addresses from the right fields
     *XSTADDR1 = STADDR1
     *XSTADDR2 = STADDR2
     *XSTADDR3 = TRIM(STCITY) + ' ' +TRIM(STSTATE) + ' ' + STZIP
     
     
     XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 ) 
     XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 ) 
     XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
     *B802227,1 BWA [END]
     
     IF LEN(TRIM(XSTADDR2)) =0
       XSTADDR2 = XSTADDR3
       XSTADDR3 = ''
     ENDIF
   ELSE
     SELE Customer

     lnRecNo = RECNO()       && Variable to hold the record number.
     IF !EMPTY(Dist_Ctr) 
       lcDistCntr = ALLTRIM(Dist_Ctr)
       =seek('S' + XACCOUNT + lcDistCntr)
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

   *B604682,1 BWA 23/04/2002 Call a function to print the header from anywhere and comment the code.[START]
   DO lpHrdLabls
   IF .F.
     * LINE 1
     @ 00,01 SAY 'P I C K I N G   T I C K E T'
     @ 00,35 SAY HLINE1
     @ 00,70 SAY DATE()
     @ 01,35 SAY HLINE2
     @ 02,01 SAY 'TICKET: '+XPIKTKT
     @ 02,16 SAY XDATE
     @ 02,35 SAY HLINE3
     @ 02,70 SAY XTIME
     @ 03,01 SAY 'ORDER : '+ORDER
     @ 03,16 SAY SUBSTR(DTOC(START),1,5)
     @ 03,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
     @ 03,35 SAY HLINE4

     *B800087,1 TAK 08/20/95
     IF llWareHous .AND. !EMPTY(lcWDesc)
       @ 04,01 SAY lcWDesc
     ENDIF

     *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
     *@ 04,35 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
     *@ 04,35 SAY HLINE5
     *@ 04,35 SAY XPHONE SIZE 1,16
   
     *B802520,1 BWA 15/08/1999 [start]
     @ 04,35 SAY IIF(llPrntComp,LEFT(XPHONE,16),'') 
     *B802520,1 [END]
   
     *B500772,1 HISH 06/29/95.  ( End )
     @ 06,04 SAY '.....SOLD TO .....'
     @ 06,46 SAY '.....SHIP TO .....'
     @ 07,04 SAY ORDHDR.ACCOUNT

     *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
     *@ 07,10 SAY PHONE  PICTURE "@R "+gcPhnFrmt SIZE 1,16  &&    XPHONE
      @ 07,10 SAY XPHONE SIZE 1,16
     *@ 07,10 SAY PHONE
     *B500772,1 HISH 06/29/95.  ( End )

     IF LEN(TRIM(XSTORE))<>0 .AND. XSTORE<>'*'
       *B801524,1 OWA 03/19/98 (Begin) Printed the distribution center if 
       *B801524,1              exists instead of the store.
       *@ 07,46 SAY 'STORE#: ' + XSTORE
       @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
       *B801524,1 OWA 03/19/98 (End) 
     ENDIF
     @ 08,04 SAY XBTNAME
     @ 08,46 SAY XSTNAME
     @ 09,04 SAY XBTADDR1
     @ 09,46 SAY XSTADDR1
     @ 10,04 SAY XBTADDR2
     @ 10,46 SAY XSTADDR2
     @ 11,04 SAY XBTADDR3
     @ 11,46 SAY XSTADDR3
     @ 13,00 SAY A
     @ 14,00 SAY B
     @ 15,00 SAY A
     SELECT ORDHDR

     *B803010,1 [Start] Adjust. position of ShipVia,Season,Spcl. Instr. ,Dept. ,Purch. Order    
     lcBar='|'
     *@ 16,00 SAY B1
     @ 16,00 SAY SUBSTR(ALLTRIM(XSHIPVIA),1,16)
     *@ 16,01 SAY XSHIPVIA
     @ 16,16 SAY lcBar
     @ 16,18 SAY SUBSTR(XSEASON,1,7) 
     @ 16,25 SAY lcBar
     @ 16,26 SAY SUBSTR(XSPCINST,1,17)
     *@ 16,26 SAY XSPCINST
     @ 16,44 SAY lcBar
     *B804416,1 HBG 11/09/2001 Get the store from (lcTmpOrdL) because in case of order 
     *B804416,1                multi store , ORDHDR.STORE is empty [Begin]
     *@ 16,47 SAY SUBSTR(STORE,1,7)
     @ 16,46 SAY SUBSTR(XSTORE,1,7)
     *B804416,1 [End]
     *@ 16,47 SAY STORE
     @ 16,55 SAY lcBar
     *B804416,1 HBG 11/09/2001 Change the width of Dept field to 5 char [Begin]
     *@ 16,58 SAY SUBSTR(DEPT,1,4)
     @ 16,57 SAY SUBSTR(DEPT,1,5)
     *B804416,1 [End]
     *@ 16,58 SAY DEPT
     @ 16,63 SAY lcBar
     *B804416,1 HBG 11/09/2001 Change the width of Dept field to 5 char [Begin]
     *@ 16,64 SAY SUBSTR(IIF(MultiPO,lcCustPO,CUSTPO),1,13)  &&TAK 08/22/94
     @ 16,64 SAY SUBSTR(IIF(MultiPO,lcCustPO,CUSTPO),1,15)  &&TAK 08/22/94
     *B804416,1 [End]
     *@ 16,64 SAY IIF(MultiPO,lcCustPO,CUSTPO)
     @ 16,79 SAY lcBar
     *B803010,1 [End]

     @ 17,00 SAY A
     @ 18,00 SAY C
     @ 19,00 SAY A
  ENDIF
  *B604682,1 BWA 23/04/2002.[END]

   ROW = 20

   *------------------
   * LINE LOOP
   *------------------

   SELECT (lcTmpOrdL)
   NEWDOC  = .T.
   *B804416,1 HBG 11/09/2001 Get the store from (lcTmpOrdL) because in case of order 
   *B804416,1                multi store , ORDHDR.STORE is empty [Begin]
   *XSTORE  = ORDHDR.STORE
   XSTORE  = STORE
   *B804416,1 [End]
   XTOTQTY = 0
   DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT .AND. LINENO <> 0
     WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT
     *B604170,1 MHM 02/04/2001 remove comments to work this part [start]   
     * DO CASE
     *    CASE EOF()
     *      NEWDOC = .T.
     *      EXIT
     *    CASE STORE # XSTORE
     *      NEWDOC = .F.
     *      EXIT
     *
     *    CASE TOTPIK<=0
     *      SKIP
     *      LOOP
     *
     *    CASE ROW >= 49
     *      NEWDOC = .F.
     *      EXIT
     * ENDCASE

      DO CASE
         CASE EOF()
           NEWDOC = .T.
           EXIT
         CASE STORE # XSTORE
           NEWDOC = .F.
           EXIT

         CASE TOTPIK<=0
           SKIP
           LOOP

         CASE ROW >= 49
           NEWDOC = .F.
           SKIP - 1 
           EXIT
      ENDCASE

      *B604170,1 MHM 02/04/2001 [end]
      * TEST PAGE OVERFLOW
      KEY = STYLE
      SELECT STYLE
      *SEEK KEY
      XSTYDESC =  DESC

      SELECT (lcTmpOrdL)

      lcSizeScal = GetScale (Scale,Space(1))      &&TAK 11/29/94
      IF lcOldSize  <> lcSizeScal 
        lcOldSize  = lcSizeScal 
        lcScaleLin = 'SIZE ===>' + lcSizeScal     &&TAK 11/29/94
        @ Row,03 SAY lcScaleLin
        Row = Row + 1
      ENDIF  

      @ ROW,03 SAY STYLE.CSTYGROUP
      *-- MFM 06/27/94.
      *G000000,1 HISH 08/17/95. (Begin).
      *@ ROW,05 SAY SUBSTR( STYLE, 1, 7 )
      
      *B803010,1 [Start] display the style (maj  -nmaj) 
      *@ ROW,05 SAY SUBSTR( STYLE, 1, 12)
      @ ROW,10 SAY STYLE
      *B803010,1 [End]
      
      *@ ROW,13 SAY SUBSTR( COLOR, 1, 3 )
      *@ ROW,18 SAY SUBSTR( COLOR, 1, 6 )
      *@ ROW,17 SAY XSTYDESC
      *B803010,1 [Start] change position of color disc. 
      @ ROW,30 SAY XSTYDESC
      *@ ROW,25 SAY XSTYDESC
      *B803010,1 [End]
      *G000000,1 (End).
      IF XDYELOT_S
        *B803010,1 [Start] change position of dyelot 
        @ ROW,51 SAY DYELOT
        *@ ROW,46 SAY DYELOT
        *B803010,1 [End]
      ENDIF

      IF PRTPRICE ='Y'
         XAMOUNT = PRICE*TOTPIK
         @ ROW,60 SAY PRICE        PICTURE '9999.99'
         @ ROW,69 SAY XAMOUNT      PICTURE '999,999.99'
      ENDIF
      ROW = ROW+2

      @ ROW,05 SAY 'ORDR:'
      @ ROW,12 SAY QTY1     PICTURE '99999'
      @ ROW,18 SAY QTY2     PICTURE '99999'
      @ ROW,24 SAY QTY3     PICTURE '99999'
      @ ROW,30 SAY QTY4     PICTURE '99999'
      @ ROW,36 SAY QTY5     PICTURE '99999'
      @ ROW,42 SAY QTY6     PICTURE '99999'
      @ ROW,48 SAY QTY7     PICTURE '99999'
      @ ROW,54 SAY QTY8     PICTURE '99999'
      @ ROW,61 SAY TOTQTY   PICTURE '999999'

      * CUM ORDER TOTALS
      XTOTQTY  = XTOTQTY+ TOTQTY
      XVALUE   = XVALUE + TOTPIK * PRICE

      PRTPIK=.F.
      PRTPIK = IIF(PIK1 # QTY1, .T., PRTPIK)
      PRTPIK = IIF(PIK2 # QTY2, .T., PRTPIK)
      PRTPIK = IIF(PIK3 # QTY3, .T., PRTPIK)
      PRTPIK = IIF(PIK4 # QTY4, .T., PRTPIK)
      PRTPIK = IIF(PIK5 # QTY5, .T., PRTPIK)
      PRTPIK = IIF(PIK6 # QTY6, .T., PRTPIK)
      PRTPIK = IIF(PIK7 # QTY7, .T., PRTPIK)
      PRTPIK = IIF(PIK8 # QTY8, .T., PRTPIK)

      IF PRTPIK
         ROW = ROW+1
         @ ROW,05 SAY 'ALLO:'
         @ ROW,12 SAY PIK1     PICTURE '99999'
         @ ROW,18 SAY PIK2     PICTURE '99999'
         @ ROW,24 SAY PIK3     PICTURE '99999'
         @ ROW,30 SAY PIK4     PICTURE '99999'
         @ ROW,36 SAY PIK5     PICTURE '99999'
         @ ROW,42 SAY PIK6     PICTURE '99999'
         @ ROW,48 SAY PIK7     PICTURE '99999'
         @ ROW,54 SAY PIK8     PICTURE '99999'
         @ ROW,61 SAY TOTPIK   PICTURE '999999'
      ENDIF

      XPIECES = XPIECES + TOTPIK

      ROW = ROW + 1
      @ ROW,05 SAY 'PICK:'
      @ ROW,12 SAY '_____'
      @ ROW,18 SAY '_____'
      @ ROW,24 SAY '_____'
      @ ROW,30 SAY '_____'
      @ ROW,36 SAY '_____'
      @ ROW,42 SAY '_____'
      @ ROW,48 SAY '_____'
      @ ROW,54 SAY '_____'
      @ ROW,62 SAY '_____'
      ROW=ROW+2
      
      SELECT (lcTmpOrdL)
      SKIP
   ENDDO

   IF XPIECES >0
      ROW = ROW + 1
      @ ROW,10 SAY "TOTAL PIECES TO PICK ............"
      @ ROW,61 SAY XPIECES      PICTURE '999999'
      GPIECES = GPIECES + XPIECES
      XPIECES = 0
   ENDIF
   ROW = ROW + 1
   IF ORDHDR.STORE = XSTORE .AND. !NEWDOC
      @ ROW,30 SAY 'CONTINUED NEXT PAGE ...'
   ENDIF
   IF !NEWDOC
      LOOP
   *B604666,1 ASH 07/12/2001 (Begin) Return the pointer to the last style in the previous p/t, because the ENDSCAN command 
   *B604666,1                will move the pointer to the next p/t
   ELSE
     IF !EOF() AND !BOF() AND LINENO <> 0
       SKIP -1
     ENDIF
   *B604666,1 ASH 07/12/2001 (End)
   ENDIF
  
   @ ROW,10 SAY "GRAND TOTAL PIECES TO PICK ......."
   @ ROW,61 SAY GPIECES      PICTURE '999999'
   ROW = ROW + 1

   *E,100207,1 TMI 03/30/95 (STARTING) new way to print notepad.
   IF PRTNPAD = 'Y'
     lnMemWidth = SET("MEMOWIDTH")
     SET MEMOWIDTH TO 75
     SELE NOTEPAD
     IF SEEK('B'+XORDER)
       lnMline = 1
       lnMTotLin = MEMLINES(NOTEPAD.MNOTES)
       ROW = ROW +1
       @ ROW,02 SAY '*-- N O T E P A D --*'
       ROW = ROW +1
       DO WHILE lnMline < lnMTotLin

         *B604682,1 BWA 23/04/2002 Fix printing the notepad in the piktkt.[START]
         *IF ROW > 62
           *ROW = ROW+1
           *@ 59,02 SAY 'CONTINUED NEXT PAGE ...'
           *ROW = 02

         IF ROW > 52
           @ 59,02 SAY 'CONTINUED NEXT PAGE ...'
           DO lpHrdLabls
           Row = 20
         *B604682,1 BWA 23/04/2002.[END]

         ENDIF
         IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)  
           @ ROW,04 SAY MLINE(MNOTES,lnMline)
           ROW = ROW + 1
         ENDIF
         lnMline = lnMline + 1  
       ENDDO
       @ ROW,02 SAY '*-- END OF NOTEPAD --*'
       SET MEMOWIDTH TO lnMemWidth
     ENDIF
   ENDIF

   *E,100207,1 TMI 03/30/95 (ENDING) new way to print notepad.
   @ 54,01 SAY ORDHDR->NOTE1
   @ 54,41 SAY ORDHDR->NOTE2

   @ 55,00 SAY A
   @ 56,00 SAY G
   
   *B803010,1 [Start]
   *@ 57,00 SAY H
   @ 57,00 SAY lcBar
   *B804423,1 HBG 18/09/2001 Print the Merchandise according to 
   *B804423,1                the value of Print style prices option [Begin]
   IF PRTPRICE ='Y'
   *B804423,1 [End]
     @ 57,02 SAY XVALUE     PICTURE '9999999.99'
   *B804423,1 HBG 18/09/2001 Print the Merchandise according to 
   *B804423,1                the value of Print style prices option [Begin]
   ENDIF
   *B804423,1 [End]  
   @ 57,18 SAY lcBar
   XVALUE   = 0.00
   @ 57,30 SAY lcBar
   @ 57,38 SAY lcBar
   @ 57,40 SAY SUBSTR(XTERMS,1,10)
   @ 57,50 SAY lcBar
   @ 57,62 SAY lcBar
   @ 57,78 SAY lcBar
   *B803010,1 [End]
   

   @ 58,00 SAY A
   @ 59,00 SAY E
   *B803010,1 [Start]
   *@ 60,00 SAY F
   @ 60,00 SAY lcBar
   @ 60,18 SAY lcBar
   @ 60,30 SAY lcBar
   @ 60,38 SAY lcBar
   @ 60,39 SAY SUBSTR(XSHIPVIA,1,10)
   @ 60,50 SAY lcBar
   @ 60,62 SAY lcBar
   @ 60,78 SAY lcBar
   *@ 60,39 SAY SUBSTR(XSHIPVIA,1,10)
   *B803010,1 [End]
   

   @ 61,00 SAY A
   @ 62,00 SAY MSGL1
   @ 63,00 SAY MSGL2
   @ 64,00 SAY MSGL3

   SELE PIKTKT
*   SEEK &PIKTEMP->PIKTKT
   *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
   *REPL PRTFLAG WITH 'P'
   *B603487,1 BWA 03/08/2000 [END]

   SELECT (lcTmpOrdL)
*ENDDO
ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
                       
*B604682,1 BWA 23/04/2002 Function to print the header from anywhere in the program.[START]
*!*************************************************************
*! Name      : lpHrdLabls.
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 04/23/2002
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKF.PRG
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*! Example     : =lpHrdLabls()
*!*************************************************************
PROCEDURE lpHrdLabls

@ 00,01 SAY 'P I C K I N G   T I C K E T'
@ 00,35 SAY HLINE1
@ 00,70 SAY DATE()

@ 01,35 SAY HLINE2
@ 02,01 SAY 'TICKET: '+ XPIKTKT
@ 02,16 SAY XDATE
@ 02,35 SAY HLINE3
@ 02,70 SAY XTIME

@ 03,01 SAY 'ORDER : ' + XORDER
@ 03,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 03,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
@ 03,35 SAY HLINE4

IF llWareHous .AND. !EMPTY(lcWDesc)
  @ 04,01 SAY lcWDesc
ENDIF

@ 04,35 SAY IIF(llPrntComp,LEFT(XPHONE,16),'')
@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'
@ 07,04 SAY ORDHDR.ACCOUNT
@ 07,10 SAY XPHONE SIZE 1,16

IF LEN(TRIM(XSTORE))<>0 .AND. XSTORE<>'*'
  @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,lcDistCntr)
ENDIF

@ 08,04 SAY XBTNAME
@ 08,46 SAY XSTNAME
@ 09,04 SAY XBTADDR1
@ 09,46 SAY XSTADDR1
@ 10,04 SAY XBTADDR2
@ 10,46 SAY XSTADDR2
@ 11,04 SAY XBTADDR3
@ 11,46 SAY XSTADDR3
@ 13,00 SAY A
@ 14,00 SAY B
@ 15,00 SAY A

lcBar='|'
@ 16,00 SAY SUBSTR(ALLTRIM(XSHIPVIA),1,16)
@ 16,16 SAY lcBar
@ 16,18 SAY SUBSTR(XSEASON,1,7)
@ 16,25 SAY lcBar
@ 16,26 SAY SUBSTR(XSPCINST,1,17)
@ 16,44 SAY lcBar
@ 16,46 SAY SUBSTR(XSTORE,1,7)
@ 16,55 SAY lcBar
@ 16,57 SAY SUBSTR(ORDHDR.DEPT,1,5)
@ 16,63 SAY lcBar
@ 16,64 SAY SUBSTR(IIF(ORDHDR.MultiPO , lcCustPO , ORDHDR.CUSTPO),1,15)
@ 16,79 SAY lcBar
@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A

ROW = 20
*--End of lpHrdLabls.
*B604682,1 BWA 23/04/2002.[END]