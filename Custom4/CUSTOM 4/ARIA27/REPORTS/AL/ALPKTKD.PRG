*:***************************************************************************
*: Program file  : ALPKTKD.PRG
*: Program desc. : SALES ORDER ALLOCATION PICKING TICKET FORM 'D'.
*: Date          : 
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : 
*: Tracking Job Number: 
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKD
*:***************************************************************************
*: Modifications
**************************************************************************** 
*: *B602847,1 KHM 04/29/99 Fixed the bug of not displaying a message when
*: *B602847,1              there were no records to print.
*: *B802227,1 BWA Get the customer addresses from the right fields
*: B802479,1 HDM 08/02/1999 Assign phone value from main program
*: *B802520,1 BWA 15/08/1999 Fix the bug of always print the company name and
*: *B802520,1 address even if the parameter of "Print company name"  is setup to NO.
*: *B606360,1 BWA 08/12/2002 Fix some bugs in the piktkt form.
*: *B606456,1 BWA 09/05/2002 Fix some bugs in the piktkt form.
*: *B606468,1 KHM 09/10/2002 Fix the bug of printing the footer without a header page.
**************************************************************************** 

*******************  From Original *******************
*-- We Will use the temp file of pktkt.prg to make this report work
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
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

 A='-------------------------------------------------------------------------------'
 B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
B1='                |        |                  |          |       |              |'

 IF XDYELOT_S
   C='   G STYLE   CLR     DESCRIPTION              DYELOT #        PRICE      AMOUNT'
 ELSE
   C='   G STYLE   CLR     DESCRIPTION                              PRICE      AMOUNT'
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
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345
*---------------------------------------------------------------------------------------
SELECT ORDHDR
NEWDOC = .T.
SET DEVICE TO PRINT

SELECT (lcTmpOrdL)
LOCATE
SCAN
  WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT
  IF NEWDOC
    lcOldSize = SPACE(7)
    GPIECES = 0
    XPIKTKT = PIKTKT
    XORDER  = ORDER
    XSTORE  = STORE
    XDATE   = PIKDATE

    SELECT ORDHDR
    =SEEK('O' + XORDER ,'ORDHDR')
    lcCustPO = LEFT(IIF(ORDHDR.MultiPO,&lcTmpOrdL..CustPO,ORDHDR.CustPO),13)
    lcWDesc  = CWARECODE

    XBTNAME  = CUSTOMER.BTNAME
    XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2') 
    XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
    XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
     
    IF LEN(TRIM(XBTADDR2)) =0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF

    * GET DESCRIPTIONS FOR CODED FIELDS [start]
    SELECT CODES
    SET ORDER TO CODES IN CODES 

    *TERMS
    XTERMS = gfCodDes(ORDHDR.CTERMCODE , 'CTERMCODE')

    *B606360,1 08/11/2002 Fix the bug of printing more the width of the fields.[START]
    *SHIPVIA
    *XSHIPVIA = gfCodDes(ORDHDR.SHIPVIA , 'SHIPVIA')
    *SPC INST.
    *XSPCINST = gfCodDes(ORDHDR.SPCINST , 'SPCINST')

    XSHIPVIA = gfCodDes(ORDHDR.SHIPVIA , 'SHIPVIA')
    XSPCINST = LEFT(gfCodDes(ORDHDR.SPCINST , 'SPCINST'),18)
    *B606360,1 08/11/2002.[END]

    *SEEK 'Z' + ORDHDR.SEASON
    XSEASON  = gfCodDes(ORDHDR.SEASON , 'SEASON')
      
    HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)      
    
    *B802520,1 BWA 15/08/1999 [start]
    HLINE1 = IIF(llPrntComp,HLINE1,'')
    *B802520,1 [END]
 
    STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4

    XPIECES = 0
    * GET DESCRIPTIONS FOR CODED FIELDS [end]
  ENDIF

  *--Get Alt Ship to if exsits if !exist account
  SELE OrdHdr
  lcDistCntr = ""
  IF Alt_ShpTo
    XSTNAME  = lcShpTName
    *B802227,1 BWA Get customer addresses from the right fields       
    *XSTADDR1 = laShipTo[1]
    *XSTADDR2 = laShipTo[2]
    *XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
    *B802227,1 BWA [END]
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ELSE
    SELECT CUSTOMER
    IF !EMPTY(Dist_Ctr) 
      lcDistCntr = ALLTRIM(Dist_Ctr)
      =seek('S' + PIKTKT.ACCOUNT + lcDistCntr)
    ENDIF
    XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
    *B802227,1 BWA Get customer addresses from the right fields             
    *XSTADDR1 = laShipTo[1]
    *XSTADDR2 = laShipTo[2]
    *XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
    *B802227,1 BWA [END]
    IF !EMPTY(lcDistCntr) 
      *GO lnRecNo
    ENDIF
    *B801524,1 OWA 03/19/98 (End) 
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF

  SELECT (lcTmpOrdL)
  DO lpHrdLabls
  IF .F.
    * LINE 1
    @ 00,01 SAY 'P I C K I N G   T I C K E T'
    @ 00,35 SAY HLINE1
    @ 00,70 SAY DATE()
    @ 01,35 SAY HLINE2
    @ 02,01 SAY 'TICKET: ' + PIKTKT
    @ 02,16 SAY XDATE
    @ 02,35 SAY HLINE3
    @ 02,70 SAY TIME()
    @ 03,01 SAY 'ORDER : '+ORDER
    @ 03,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
    @ 03,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
    @ 03,35 SAY HLINE4

    *B800087,1 TAK 08/20/95
    IF llWareHous .AND. !EMPTY(lcWDesc)
      @ 04,01 SAY lcWDesc
    ENDIF

    *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
    *@ 04,35 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
    *@ 04,35 SAY HLINE5
    *@ 04,35 SAY HLINE5  SIZE 1,16
   
    *B802520,1 BWA 15/08/1999 [start]
    @ 04,35 SAY IIF(llPrntComp,LEFT(HLINE5,16),'') 
    *B802520,1 [END]
    *B500772,1 HISH 06/29/95.  ( End )

    @ 06,04 SAY '.....SOLD TO .....'
    @ 06,46 SAY '.....SHIP TO .....'
    @ 07,04 SAY ORDHDR.ACCOUNT

    *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
    *@ 07,10 SAY PHONE  PICTURE "@R "+gcPhnFrmt SIZE 1,16
    @ 07,10 SAY XPHONE SIZE 1,16      && HDM
    *B500772,1 HISH 06/29/95.  ( End )

    IF LEN(TRIM(XSTORE)) <> 0 .AND. XSTORE <> '*'
      *B801524,1 OWA 03/19/98 (Begin) Printed the distribution center if 
      *B801524,1              exists instead of the store.
      *@ 07,46 SAY 'STORE#: ' + XSTORE
      @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr) , XSTORE , lcDistCntr)
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
    @ 16,01 SAY LEFT(XSHIPVIA,15) + "|"
    @ 16,18 SAY SUBSTR(XSEASON,1,7) + "|"
    @ 16,26 SAY XSPCINST + "|"
    @ 16,46 SAY &lcTmpOrdL..STORE + " |"
    @ 16,57 SAY DEPT + " |"
    @ 16,65 SAY lcCustPO + "|"
    @ 17,00 SAY A
    @ 18,00 SAY C
    @ 19,00 SAY A
  ENDIF

  ROW = 20
  XTOTQTY = 0
  SELECT (lcTmpOrdL)
  NEWDOC = .T.
  XSTORE  = STORE

  DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT .AND. LINENO <> 0

    SELECT (lcTmpOrdL)
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
      *B606468,1 KHM 09/10/2002 (Begin)
      *CASE ROW >= 52
      CASE ROW > 45
      *B606468,1 KHM 09/10/2002 (End)
      
        NEWDOC = .F.
        SKIP - 1 
        EXIT
    ENDCASE

    KEY = STYLE
    XSTYDESC =  STYLE.DESC
    SELECT (lcTmpOrdL)
    lcSizeScal = GetScale (Scale,Space(1))
    IF lcOldSize  <> lcSizeScal 
      lcOldSize  = lcSizeScal 
      lcScaleLin = 'SIZE ===>' + lcSizeScal
      @ Row,03 SAY lcScaleLin
      Row = Row + 1
    ENDIF

    *B606456,1 BWA 09/05/2002 Print the group from the ordline.[START]
    *@ ROW,03 SAY &lcTmpOrdL..GROUP
    @ ROW,03 SAY ORDLINE.GROUP
    *B606456,1 BWA 09/05/2002.[END]

    @ ROW,05 SAY SUBSTR(STYLE, 1, 7 )

    *B606456,1 BWA 09/05/2002 Print the color.[START]
    @ ROW,13 SAY SUBSTR(STYLE, lnClrPosM1 , lnClrLnM1 )
    *B606456,1 BWA 09/05/2002.[END]

    @ ROW,21 SAY XSTYDESC
    IF XDYELOT_S
      @ ROW,46 SAY DYELOT
    ENDIF

    IF llRpStyPrc
      XAMOUNT = PRICE * TOTPIK
      @ ROW,60 SAY PRICE        PICTURE '9999.99'
      @ ROW,69 SAY XAMOUNT      PICTURE '999,999.99'
    ENDIF
    *B606468,1 KHM 09/10/2002 (Begin)
    *ROW = ROW + 2
    ROW = ROW + 1    
    *B606468,1 KHM 09/10/2002 (End)

    @ ROW,05 SAY 'ORDR:'
    @ ROW,12 SAY IIF( QTY1 # 0 , QTY1 , "")     PICTURE '99999'
    @ ROW,18 SAY IIF( QTY2 # 0 , QTY2 , "")     PICTURE '99999'
    @ ROW,24 SAY IIF( QTY3 # 0 , QTY3 , "")     PICTURE '99999'
    @ ROW,30 SAY IIF( QTY4 # 0 , QTY4 , "")     PICTURE '99999'
    @ ROW,36 SAY IIF( QTY5 # 0 , QTY5 , "")     PICTURE '99999'
    @ ROW,42 SAY IIF( QTY6 # 0 , QTY6 , "")     PICTURE '99999'
    @ ROW,48 SAY IIF( QTY7 # 0 , QTY7 , "")     PICTURE '99999'
    @ ROW,54 SAY IIF( QTY8 # 0 , QTY8 , "")     PICTURE '99999'
    @ ROW,61 SAY IIF( TOTQTY # 0 , TOTQTY , "") PICTURE '999999'

    *--SUM ORDER TOTALS
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
       ROW = ROW + 1
       @ ROW,05 SAY 'ALLO:'
       @ ROW,12 SAY IIF( PIK1 # 0 , PIK1 , "")       PICTURE '99999'
       @ ROW,18 SAY IIF( PIK2 # 0 , PIK2 , "")       PICTURE '99999'
       @ ROW,24 SAY IIF( PIK3 # 0 , PIK3 , "")       PICTURE '99999'
       @ ROW,30 SAY IIF( PIK4 # 0 , PIK4 , "")       PICTURE '99999'
       @ ROW,36 SAY IIF( PIK5 # 0 , PIK5 , "")       PICTURE '99999'
       @ ROW,42 SAY IIF( PIK6 # 0 , PIK6 , "")       PICTURE '99999'
       @ ROW,48 SAY IIF( PIK7 # 0 , PIK7 , "")       PICTURE '99999'
       @ ROW,54 SAY IIF( PIK8 # 0 , PIK8 , "")       PICTURE '99999'
       @ ROW,61 SAY IIF( TOTPIK # 0 , TOTPIK , "")   PICTURE '999999'
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
    ROW = ROW + 2
    SELECT (lcTmpOrdL)
    SKIP
  ENDDO

  SELECT (lcTmpOrdL)
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
  ELSE
  IF !EOF() AND !BOF() AND LINENO <> 0
    SKIP -1
    ENDIF
  ENDIF

  @ ROW,10 SAY "GRAND TOTAL PIECES TO PICK ......."
  @ ROW,61 SAY GPIECES      PICTURE '999999'
    ROW = ROW + 1

     
  IF llRpOrdNot
    lnMemWidth = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75
    SELECT NOTEPAD
    IF SEEK('B' + XORDER)
      lnMline = 1
      lnMTotLin = MEMLINES(NOTEPAD.MNOTES)
      ROW = ROW +1
      @ ROW,02 SAY '*-- N O T E P A D --*'
      ROW = ROW +1
      DO WHILE lnMline < lnMTotLin
        *B606360,1 BWA 08/12/2002 Fix printing the notepad in the piktkt.[START]
        *IF ROW > 62
        *  ROW = ROW + 1
        *  @ ROW,02 SAY 'CONTINUED NEXT PAGE ...'
        *  ROW = 02
        *ENDIF

        IF ROW > 52
          @ 59,02 SAY 'CONTINUED NEXT PAGE ...'
          DO lpHrdLabls
        ENDIF
        *B606360,1 BWA 08/12/2002.[END]

        IF  '*' <> LEFT(MLINE(MNOTES,lnMline),1)  
          @ ROW,04 SAY MLINE(MNOTES,lnMline)
          ROW = ROW + 1
        ENDIF
        lnMline = lnMline + 1  
      ENDDO
      @ ROW,02 SAY '*-- END OF NOTEPAD --*'
    ENDIF
  ENDIF
  
  *B606468,1 KHM 09/10/2002 (Begin) Checking if the row exceeds 52 then print the header
  IF ROW > 54
     DO lpHrdLabls
   ENDIF
  *B606468,1 KHM 09/10/2002 (End)
  
  @ 54,01 SAY ORDHDR.NOTE1
  @ 54,41 SAY ORDHDR.NOTE2
  @ 55,00 SAY A
  @ 56,00 SAY G
  @ 57,00 SAY "|  $"
  @ 57,05 SAY XVALUE     PICTURE '9999999.99'
  @ 57,18 SAY "| $"
  @ 57,30 SAY "| $"
  XVALUE   = 0.00
  @ 57,38 SAY "|"
  @ 57,39 SAY SUBSTR(XTERMS,1,10)
  @ 57,50 SAY "|"
  @ 57,62 SAY "|"
  @ 57,78 SAY "|"
  @ 58,00 SAY A
  @ 59,00 SAY E
  @ 60,00 SAY "|                 |           |       |"
  @ 60,39 SAY SUBSTR(XSHIPVIA,1,11)
  @ 60,50 SAY "|           |               |"
  @ 61,00 SAY A
  @ 62,00 SAY lcRpMsg1
  @ 63,00 SAY lcRpMsg2
  @ 64,00 SAY lcRpMsg3
     
ENDSCAN
SET DEVICE TO SCREEN
WAIT CLEAR
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpHrdLabls.
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 08/12/2002
*! Purpose   : Print the Header labels.
*!*************************************************************
*! Called from : ALPKTKD.PRG
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

* LINE 1
@ 00,01 SAY 'P I C K I N G   T I C K E T'
@ 00,35 SAY HLINE1
@ 00,70 SAY DATE()
@ 01,35 SAY HLINE2
@ 02,01 SAY 'TICKET: ' + XPIKTKT
@ 02,16 SAY XDATE
@ 02,35 SAY HLINE3
@ 02,70 SAY TIME()
@ 03,01 SAY 'ORDER : ' + XORDER
@ 03,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 03,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
@ 03,35 SAY HLINE4

IF llWareHous .AND. !EMPTY(lcWDesc)

  *B606456,1 BWA 09/05/2002 Print the string 'Warehouse'.[START]
  *@ 04,01 SAY lcWDesc
  @ 04,01 SAY "Warehouse : " + lcWDesc
  *B606456,1 BWA 09/05/2002.[END]

ENDIF

@ 04,35 SAY IIF(llPrntComp,LEFT(HLINE5,16),'') 
@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'
@ 07,04 SAY ORDHDR.ACCOUNT
@ 07,10 SAY TRANSFORM(ORDHDR.PHONE, lcPhonPict)

IF LEN(TRIM(XSTORE)) <> 0 .AND. XSTORE <> '*'
  @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr) , XSTORE , lcDistCntr)
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

@ 16,01 SAY LEFT(XSHIPVIA,15) + "|"
@ 16,18 SAY SUBSTR(XSEASON,1,7) + "|"
@ 16,26 SAY XSPCINST + "|"
@ 16,46 SAY &lcTmpOrdL..STORE + " |"
@ 16,57 SAY ORDHDR.DEPT + " |"
@ 16,65 SAY lcCustPO + "|"
@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A

ROW = 20

*--End of lpHrdLabls.