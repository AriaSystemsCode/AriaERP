*:***************************************************************************
*: Program file  : ALPKTKTH.PRG
*: Program desc. : SALES ORDER ALLOCATION PICKING TICKET FORM 'B'.
*:               : For Customer The 18
*: Date          : 04/16/2003
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Abdou Elgendy [ABD]
*: Tracking Job Number: C#102825,1
*:***************************************************************************
*: Calls :
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKTH
*:***************************************************************************
*: Modifications
*:***************************************************************************
*:Note
*: Basically, we need to take our standard  pick ticket form "B" and create a custom version for this customer.
*: The only modification is the following:
*: 1) - Remove the "Dyelot #" from the heading and the column
*: 2) In it's place put "Clr Name"  and please print the Color
*:    description from the code file for that color.  
*:    Print as many characters of the color desc. as you can fit.  
*:    I am sure you can fit at least 10 since the Dyelot # was 10 characters.
************************************************************************** 
*******************  From Original *******************
*-- We Will use the temp file of pktkt.prg to make this report work
*-- declare variabe to hold shipvia code [Start]
lcShipVia=""
*--  get Division long name
lcDivLName = ''        && Variable to hold the Division long name
DECLARE laDivLName[1,2]
laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

*-- get item header 
lcStyTitl = gfItemMask("HI")


*--  No of order in temp. file 
lnRecord = 1 
PRTPRICE = IIF(llRpStyPrc , 'Y' , 'N')
STORE 0.00 TO XORDTOT, XVALUE , SVALUE
STORE .F. TO llNoRec
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3

*-- Fix the bug of shifting the data in the form 
STORE TRIM(laCompAdd[3])                                    TO HLINE4
HLINE2 = IIF(llPrntComp,HLINE2,'')
HLINE3 = IIF(llPrntComp,HLINE3,'')
HLINE4 = IIF(llPrntComp,HLINE4,'')

*-- To begin with page # 1 [Begin]
STORE 1 TO PAGECT
ROW = 0
XPHONE = lcXphone
MSGL1 = lcRpMsg1
MSGL2 = lcRpMsg2
MSGL3 = lcRpMsg3
IF LEN(HLINE3) = 0
   STORE HLINE4 TO HLINE3
ENDIF

*--Get Memorey Variables[Start]
XDYELOT_S = IIF(ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y', .T. , .F.)
llWareHous = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)
*--Get Memorey Variables[End]
SELECT (lcTmpOrdL)
IF EOF()
  llNoRec = .T.
  *-- Displaying a message to indicate that no
  *--  records were found to display.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF

RELEASE ALL LIKE M_*

    *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
 A='-------------------------------------------------------------------------------'
 B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
B1='                |        |                  |          |       |              |'

IF XDYELOT_S
   C='   G ' + lcStyTitl + ' DESCRIPTION          COLOR DEC.      PRICE      AMOUNT' 
ELSE
  C='   G ' + lcStyTitl + ' DESCRIPTION           COLOR DEC.     PRICE      AMOUNT'  
ENDIF
 
D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___  ___  ___ ____'
E='|  BILL OF LADING | # CARTONS | WEIGHT| PICKED BY | PACKED BY | SHIPPED VIA   |'
F='|                 |           |       |           |           |'
G='|  MERCHANDISE    |  FREIGHT  | INSUR | OTHER CHGS| TERMS     | COMMENTS:     |'
H='|  $              | $         | $     | $         |           |               |'
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

** DYELOT # IS PRINTED ONLY FOR SYSTEMS USING DYELOTS ***
*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*   G STYLE   CLR     DESCRIPTION              DYELOT #         PRICE      AMOUNT
*12 X 1234567 123 12345678901234567890         XXXXXXXXXX     1234.99  123,456.99
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
SELECT ORDHDR
lcWDesc  = CWARECODE


SELECT CUSTOMER
NEWDOC = .T.
CLEAR TYPEAHEAD
XTIME = TIME()
SET DEVICE TO PRINT
SELECT (lcTmpOrdL)
LOCATE

*-->HDM<-- DO WHILE INKEY() <>32
XPIKTKT = SPACE(6)
SCAN FOR LINENO # 0
  HLINE2 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 1) , HLINE2 ) ,'')
  HLINE3 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 2) , HLINE3 ) ,'')
  HLINE4 = IIF(llPrntComp , IIF(llRpWareH , gfGetAdr('WAREHOUS' , '' , '' , '' , 3) , HLINE4 ) ,'')
  HLINE5 = IIF(llPrntComp , IIF(llRpWareH , TRANSFORM(WAREHOUS.CPHONE,lcPhonPict)   , IIF(TYPE('HLINE5') = 'U' , '', HLINE5) ) ,'')
  IF LEN(HLINE3) = 0
    STORE HLINE4 TO HLINE3
    STORE HLINE5 TO HLINE4
    STORE ' '    TO HLINE5
  ENDIF

  IF PIKTKT <> XPIKTKT

    PAGECT = 1
    lnRecord = RECNO()
    =gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
    XVALUE = 0.00
  ENDIF
  XPIKTKT = PIKTKT
  SELECT CUSTOMER
  =SEEK(IIF(EMPTY(PIKTKT.STORE),"M","S")+PIKTKT.ACCOUNT+PIKTKT.STORE)
  XBTNAME  = CUSTOMER->BTNAME
  XBTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  XBTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2') 
  XBTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2') 
  XBTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')),;
                       gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2') ,;
                       gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')  ;
              + ' ' +  gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))
  IF LEN(TRIM(XBTADDR2)) =0
    XBTADDR2 = XBTADDR3
    XBTADDR3 = ''
  ENDIF
  SELECT (lcTmpOrdL)
  XSTORE = STORE
  SELECT CODES
  SET ORDER TO CODES IN CODES 
  *TERMS
  XTERMS = gfCodDes(ORDHDR->CTERMCODE , 'CTERMCODE')

  *SHIPVIA
  lcShipVia = IIF(OrdHdr.ShipVia="*",;
              IIF(SEEK("S"+PIKTKT.ACCOUNT+PIKTKT.STORE,"CUSTOMER"),Customer.ShipVia,"*"),;
              OrdHdr.ShipVia)
  XSHIPVIA = gfCodDes(lcShipVia , 'SHIPVIA')
  *SPC INST.
  XSPCINST = gfCodDes(ORDHDR->SPCINST , 'SPCINST')

  SEEK 'Z' + ORDHDR->SEASON
  XSEASON  = gfCodDes(ORDHDR->SEASON , 'SEASON')

  HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
  HLINE1 = IIF(llRpWareH , LEFT(WAREHOUS.CDESC,30) , HLINE1 )
  HLINE1 = IIF(llPrntComp,HLINE1,'')
  STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4

  XPIECES = 0
  * GET DESCRIPTIONS FOR CODED FIELDS [end]
  
  *--Get Alt Ship to if exsits if !exist account
  SELE OrdHdr
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
    XSTADDR3 = TRIM(laShipTo[3]) 
    XSTADDR4 = IIF(EMPTY(TRIM(laShipTo[4])),TRIM(laShipTo[5]),;
                          TRIM(laShipTo[4]) + ' ' + laShipTo[5])
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
      
    XSTADDR1 = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 )
    XSTADDR2 = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 )
    XSTADDR3 = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 )
    XSTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )) ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )  ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )   ;
                    +','+gfGetAdr('CUSTOMER' , '' , '' , '' , 5 ))
    XSTADDR4 = IIF(EMPTY(gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )) ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 5 )  ,;
                         gfGetAdr('CUSTOMER' , '' , '' , '' , 4 )   ;
                     +''+gfGetAdr('CUSTOMER' , '' , '' , '' , 5 ))
    IF !EMPTY(lcDistCntr) 
*     GO lnRecNo
    ENDIF
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
  ENDIF
  *------Begin Say ((Printing))
  SELECT (lcTmpOrdL)
  * LINE 1
    
     
  @ 00,01 SAY 'P I C K I N G   T I C K E T'
  @ 00,35 SAY HLINE1
  @ 00,68 SAY DATE()
  @ 01,01 SAY 'TICKET: '+ PIKTKT       && XPIKTKT
  @ 01,16 SAY PIKDATE
  @ 01,35 SAY HLINE2
  @ 01,68 SAY 'PAGE  '+STR(PAGECT,3)
  XORDER = ORDER
  @ 02,01 SAY 'ORDER : '+ORDER    &&TTT
  @ 02,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
  @ 02,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
  @ 02,35 SAY HLINE3
  @ 02,68 SAY XTIME
  @ 03,01 SAY IIF(!EMPTY(ORDHDR.APPROVAL),'APPRVL: '+ORDHDR.APPROVAL,'') 
  @ 03,35 SAY HLINE4
  IF llWareHous .AND. !EMPTY(lcWDesc)
    @ 04,01 SAY lcWDesc
  ENDIF
  @ 06,04 SAY '.....SOLD TO .....'
  @ 06,46 SAY '.....SHIP TO .....'
    
  @ 07,04 SAY ORDHDR.ACCOUNT
  @ 07,10 SAY TRANSFORM(ORDHDR.PHONE, lcPhonPict)
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
  @ 12,04 SAY XBTADDR4
  @ 12,46 SAY XSTADDR4
  @ 13,00 SAY A
  @ 14,00 SAY B
  @ 15,00 SAY A
  SELECT ORDHDR
  @ 16,01 SAY LEFT(XSHIPVIA,15) + '|'
  @ 16,18 SAY LEFT(XSEASON,7) + '|'
  @ 16,26 SAY LEFT(XSPCINST,18) + '|'
  @ 16,46 SAY &lcTmpOrdL..STORE + ' ' + '|'
  @ 16,58 SAY DEPT + '|'
  @ 16,65 SAY LEFT(&lcTmpOrdH..Custpo,13) +'|'
  @ 17,00 SAY A
  @ 18,00 SAY C
  @ 19,00 SAY A
  ROW=20
  *--Header Has Finished
  *--Begin Lines Loop
  XTOTQTY = 0
  SELECT (lcTmpOrdL)
  DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT .AND. LINENO <> 0
    SELECT (lcTmpOrdL)
    lnCurRec = RECNO()

    SELECT STYLE
    XSTYDESC =  DESC
    @ ROW,03 SAY &lcTmpOrdL..GROUP
    *-- MFM 06/27/94.
    @ ROW,05 SAY STYLE
    SELECT (lcTmpOrdL)
    *--Say Dyelot[start]
    @ ROW,25 SAY XSTYDESC
    lcColorDesc = LEFT(ALLTRIM(gfCodDes(SUBSTR(Style,lnclrposm1,lnclrlnm1), 'COLOR')),13)
    @ ROW,47 SAY lcColorDesc
    *- Abdou
    
    *--Say Dyelot[end]
 
    IF PRTPRICE ='Y'
      XAMOUNT = PRICE * TOTPIK
      @ ROW,60 SAY PRICE        PICTURE '9999.99'
      @ ROW,69 SAY XAMOUNT      PICTURE '999,999.99'
    ENDIF
      
    Row = Row + 1
    lcScl = SPACE(0)
    IF SEEK('S'+Style.SCALE,'SCALE') 
      lcScl=' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ1),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ2),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(SCALE.SZ3),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ4),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(SCALE.SZ5),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ6),1,5),5,' ')+' '+;
                PADL(SUBSTR(ALLTRIM(SCALE.SZ7),1,5),5,' ')+' '+PADL(SUBSTR(ALLTRIM(SCALE.SZ8),1,5),5,' ')   
      
    ENDIF
    @ ROW,11 SAY lcScl
    *--Actual Lines Printing[start]
      
    SELECT (lcTmpOrdL)
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
    XTOTQTY  = XTOTQTY+ TOTQTY
    XVALUE   = XVALUE + TOTPIK * PRICE
    PRTPIK = PIK1 # QTY1 .OR. PIK2 # QTY2 .OR. PIK3 # QTY3 .OR. ;
             PIK4 # QTY4 .OR. PIK5 # QTY5 .OR. PIK6 # QTY6 .OR. ;
             PIK7 # QTY7 .OR. PIK8 # QTY8
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
     
    ROW =ROW+1
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
    IF ROW > 58
      *-- call function to print header
      PAGECT = PAGECT + 1
      =lfPrntHdr()
    ENDIF
    IF llRpOrdLNt  .AND. !EMPTY(ORDLINE.Note_Mem) .AND. lfFndLine(ORDLINE.Note_Mem)
      @ ROW,02 SAY '*-- Line Note Pad --*'
      ROW = ROW + 1
      lnMemoLnNo = 0
      lnMemoWdth = SET('MEMOWIDTH')
      SET MEMOWIDTH TO 75
      *-- get no of lines in ordline.note_mem
      lnMemolins = MEMLINES(ORDLINE.Note_Mem)
      IF lnMemolins > 0
        DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
          lnMemoLnNo = lnMemoLnNo + 1
          lcNotes = MLINE(ORDLINE.Note_Mem,lnMemoLnNo)
          *B802987,4 WAB - Avoid print the ordlines notes if starting with '*'.
          IF LEFT(lcNotes , 1) # '*'                        
            IF ROW > 58
              *-- call function to print header             
              PAGECT = PAGECT + 1
              =lfPrntHdr()
            ENDIF
            @ ROW,02 SAY lcNotes
            ROW = ROW + 1
          ENDIF
        ENDDO
      ENDIF 
      @ ROW,02 SAY '*-- END OF Line NotePad --*'
      ROW = ROW + 2
    ENDIF
    SELECT (lcTmpOrdL)
    SKIP
  ENDDO
  SELECT (lcTmpOrdL)
  GOTO lnCurRec
  *------------------------
  * [NOTEPAD] COMMENT LINES
  *------------------------
  PRTNPAD = IIF(llRpOrdNot,'Y','N')
  IF PRTNPAD='Y' .AND. NEWDOC
    SELECT NOTEPAD
    IF SEEK('B' + XORDER)
      lnMemoWdth = SET('MEMOWIDTH')
      SET MEMOWIDTH TO 75
      lnMemoLnNo = 0
      lnMemolins = 0
      ROW = ROW +1
      @ ROW,02 SAY '*-- N O T E P A D --*'
      ROW = ROW + 1
      lnMemolins = MEMLINES(mNotes)
      IF lnMemolins > 0
        DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
          IF ROW > 51                 &&62
            ROW = ROW + 1
            @ ROW,2 SAY 'CONTINUED NEXT PAGE ...'
            @ 53,01 SAY IIF(LEFT(ORDHDR->NOTE1,1)#'*',ORDHDR->NOTE1,'')
            @ 53,41 SAY IIF(LEFT(ORDHDR->NOTE2,1)#'*',ORDHDR->NOTE2,'')
       	    @ 54,50 SAY 'TOTPIK'
       	    @ 54,61 SAY XPIECES
        	@ 55,00 SAY A
        	@ 56,00 SAY E
    	    @ 57,00 SAY F
		    @ 57,64 SAY SUBSTR(XSHIPVIA,1,14) + '|'
			@ 58,00 SAY A
      		@ 59,00 SAY G
            ROW = 60
            IF XSTORE = &lcTmpOrdL..STORE .AND. !NEWDOC
              @ 60,00 SAY '|' 
              @ 60,02 SAY '$' 
              @ 60,05 SAY '*******.**'
            ELSE
              @ 60,00 SAY '|' 
              @ 60,02 SAY '$' 
              IF PRTPRICE ='Y'
                @ 60,05 SAY XVALUE     PICTURE '9999999.99' 
              ENDIF
            ENDIF
            @ 60,18 SAY '|' 
            @ 60,30 SAY '|' 
            @ 60,38 SAY '|' 
            @ 60,50 SAY '|' 
            @ 60,52 SAY LEFT(XTERMS,10) 
            @ 60,61 SAY '|' 
            @ 60,78 SAY '|' 
            @ 61,00 SAY A
            @ 62,00 SAY MSGL1
            @ 63,00 SAY MSGL2
            ROW = 64
            @ 64,00 SAY MSGL3
            ROW = 0
            SELECT ORDHDR         
             
            @ 00,01 SAY 'P I C K I N G   T I C K E T'
            @ 00,35 SAY HLINE1
            @ 00,68 SAY DATE()
            @ 01,35 SAY HLINE2
            @ 01,68 SAY 'PAGE  '+STR(PAGECT,3)
            @ 02,01 SAY 'TICKET: '+XPIKTKT
            @ 02,16 SAY PIKTKT.DATE
            @ 02,35 SAY HLINE3
            @ 02,68 SAY XTIME
            XORDER=ORDER
            @ 03,01 SAY 'ORDER : '+ORDER
            @ 03,16 SAY SUBSTR(DTOC(START),1,5)
            @ 03,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
            @ 03,35 SAY HLINE4
            @ 04,01 SAY IIF(!EMPTY(ORDHDR.APPROVAL),;              
                            'APPRVL: '+ORDHDR.APPROVAL,'') 
            @ 06,04 SAY '.....SOLD TO .....'
            @ 06,46 SAY '.....SHIP TO .....'
            @ 07,04 SAY ACCOUNT
            @ 07,10 SAY TRANSFORM(ORDHDR.PHONE, lcPhonPict)
            IF LEN(TRIM(XSTORE))<>0 .AND. XSTORE<>'*'
              @ 07,46 SAY 'STORE#: ' + IIF(EMPTY(lcDistCntr),XSTORE,;
                                           lcDistCntr)
            ENDIF

            @ 08,04 SAY XBTNAME
            @ 08,46 SAY XSTNAME
             
                          
            @ 09,04 SAY XBTADDR1
            @ 09,46 SAY XSTADDR1

            @ 10,04 SAY XBTADDR2
            @ 10,46 SAY XSTADDR2

            @ 11,04 SAY XBTADDR3
            @ 11,46 SAY XSTADDR3
             
            @ 12,04 SAY XBTADDR4
            @ 12,46 SAY XSTADDR4

            @ 13,00 SAY A
            @ 14,00 SAY B
            @ 15,00 SAY A

            SELECT ORDHDR
            @ 16,01 SAY LEFT(XSHIPVIA,15) + '|'
            @ 16,18 SAY LEFT(XSEASON,7) + '|'
            @ 16,26 SAY LEFT(XSPCINST,18) + '|'
            @ 16,46 SAY &lcTmpOrdL..STORE + ' ' + '|'
            @ 16,58 SAY DEPT + '|'
            @ 16,65 SAY LEFT(&lcTmpOrdH..Custpo,13) +'|'
            @ 17,00 SAY A
            @ 18,00 SAY C
            @ 19,00 SAY A
            ROW=20

          ENDIF
          lnMemoLnNo = lnMemoLnNo + 1
          lcNotes = MLINE(NOTEPAD.mNotes,lnMemoLnNo)

          IF LEFT(lcNotes , 1) # '*'
            @ ROW,02 SAY lcNotes
            ROW = ROW + 1
          ENDIF
        ENDDO
      ENDIF
    ENDIF
  ENDIF
  ROW = 54
  @ 53,01 SAY IIF(LEFT(ORDHDR->NOTE1,1)#'*',ORDHDR->NOTE1,'')
  @ 53,41 SAY IIF(LEFT(ORDHDR->NOTE2,1)#'*',ORDHDR->NOTE2,'')
  SELECT (lcTmpOrdL)
  @ 54,50 SAY 'TOTPIK'
  @ 54,61 SAY XPIECES
  @ 55,00 SAY A
  @ 56,00 SAY E
  @ 57,00 SAY F
  @ 57,64 SAY 	LEFT(XSHIPVIA,14) + '|'
  @ 58,00 SAY A
  @ 59,00 SAY G
   
  IF XSTORE = &lcTmpOrdL..STORE .AND. !NEWDOC
    @ 60,00 SAY '|' 
    @ 60,02 SAY '$' 
    @ 60,05 SAY '*******.**'
  ELSE
    @ 60,00 SAY '|' 
    @ 60,02 SAY '$' 
    IF PRTPRICE ='Y'
      @ 60,05 SAY XVALUE     PICTURE '9999999.99' 
    ENDIF  
    XVALUE   = 0.00
  ENDIF
  @ 60,18 SAY '|' 
  @ 60,30 SAY '|' 
  @ 60,38 SAY '|' 
  @ 60,50 SAY '|' 
  @ 60,52 SAY LEFT(XTERMS,10) 
  @ 60,61 SAY '|' 
  @ 60,78 SAY '|' 
  @ 61,00 SAY A
  @ 62,00 SAY MSGL1
  @ 63,00 SAY MSGL2
  @ 64,00 SAY MSGL3
 
  IF !NEWDOC
    LOOP
  ELSE
    IF LINENO = 0
      SKIP
    ENDIF
  ENDIF
  SELE PIKTKT
  *---Print Piktkt details
  SELECT (lcTmpOrdL)
  IF llRpPrtBlk
    ROW = 65 && To print in next page 
    IF BETWEEN(lnRecord,1,RECCOUNT())
      GOTO lnRecord && right record in temp. file
    ENDIF

    DO WHILE XORDER = ORDER .AND. XPIKTKT = PIKTKT .AND. LINENO <> 0
      IF ROW > 55
        PAGECT = PAGECT+1
        @ 01,30 SAY 'BULK PICKING TICKET'
        @ 01,65 SAY 'PAGE  '+STR(PAGECT,3)
        @ 02,00 SAY DATE()
        @ 02,65 SAY 'TIME  ' + XTIME
        @ 04,00 SAY '-------------------------------------------------------------------------------'
        @ 05,00 SAY '   '+lcStyTitl+' DESCRIPTION                                               '
        @ 06,00 SAY '-------------------------------------------------------------------------------'
        ROW=8
      ENDIF
      SELECT STYLE
      = SEEK (&lcTmpOrdL..Style)
      *-- MFM 06/27/94.
      @ ROW,03 SAY STYLE
      @ ROW,23 SAY STYLE->DESC
      ROW = ROW+1
      SELECT (lcTmpOrdL)
 
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
      ROW = ROW+1
 
      @ ROW,00 SAY REPLICATE('-',79)
      ROW=ROW+2
      SELECT (lcTmpOrdL)
      SKIP
    ENDDO
  ENDIF
 
 SELECT (lcTmpOrdL)
 IF LINENO = 0 
   XORDER=ORDER
   XPIKTKT=PIKTKT
 ENDIF
  IF !EOF() AND !BOF() AND LINENO <> 0 AND llRpPrtBlk
    SKIP -1
  ENDIF
*------------------
ENDSCAN
SET DEVICE TO SCREEN

*-- End Of Code
*:**************************************************************************
*: Name      : lfPrntHdr
*: Developer : Abdou Elgendy [ABD]
*: Date      : 04/16/2003
*: Purpose   : print report headr
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Parameters: None
*:**************************************************************************
*: Returns   :  None.
*:**************************************************************************
*: Example   :  =lfPrntHdr()
*:**************************************************************************
*:
FUNCTION lfPrntHdr

@ 00,01 SAY 'P I C K I N G   T I C K E T'
@ 00,35 SAY HLINE1
@ 00,68 SAY DATE()
@ 01,01 SAY 'TICKET: '+ PIKTKT       && XPIKTKT
@ 01,16 SAY PIKDATE
@ 01,35 SAY HLINE2
@ 01,68 SAY 'PAGE  '+STR(PAGECT,3)
@ 02,01 SAY 'ORDER : '+ORDER    &&TTT
@ 02,16 SAY SUBSTR(DTOC(ORDHDR.START),1,5)
@ 02,24 SAY SUBSTR(DTOC(ORDHDR.COMPLETE),1,5)
@ 02,35 SAY HLINE3
@ 02,68 SAY XTIME
@ 03,01 SAY IIF(!EMPTY(ORDHDR.APPROVAL),'APPRVL: '+ORDHDR.APPROVAL,'') 
@ 03,35 SAY HLINE4
IF llWareHous .AND. !EMPTY(lcWDesc)
  @ 04,01 SAY lcWDesc
ENDIF

@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'
@ 07,04 SAY ORDHDR.ACCOUNT
@ 07,10 SAY CUSTOMER.PHONE1 SIZE 1,16
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
@ 12,04 SAY XBTADDR4
@ 12,46 SAY XSTADDR4
@ 13,00 SAY A
@ 14,00 SAY B
@ 15,00 SAY A
SELECT ORDHDR
@ 16,01 SAY LEFT(XSHIPVIA,15) + '|'
@ 16,18 SAY LEFT(XSEASON,7) + '|'
@ 16,26 SAY LEFT(XSPCINST,18) + '|'
@ 16,46 SAY &lcTmpOrdL..STORE + ' ' + '|'
@ 16,58 SAY DEPT + '|'
@ 16,65 SAY LEFT(&lcTmpOrdH..Custpo,13) +'|'
@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A
ROW=20
*--Header Has Finished
*-- End OF lfPrntHdr.
*:**************************************************************************
*: Name      : lfFndLine
*: Developer : Abdou Elgendy [ABD]
*: Date      : 04/16/2003
*: Purpose   : check if there are lines in notes filed didn't start with '*'
*:**************************************************************************
*: Calls     : 
*:**************************************************************************
*: Parameters: None
*:**************************************************************************
*: Returns   :  None.
*:**************************************************************************
*: Example   :  =lfFndLine()
*:**************************************************************************
*:
FUNCTION lfFndLine
PARAMETER lcMemField
PRIVATE lnMemlins,lnMmLnNo,lnCount,lcNotes
lnMemlins = MEMLINES(lcMemField)
STORE 0 TO lnMmLnNo,lnCount
IF lnMemlins > 0
  DO WHILE lnMmLnNo <> lnMemlins
    lnMmLnNo = lnMmLnNo + 1
    lcNotes = MLINE(lcMemField,lnMmLnNo)
    IF LEFT(lcNotes , 1) # '*'                        
      lnCount = lnCount + 1
    ENDIF
  ENDDO
ENDIF 
RETURN IIF(lncount>0,.T.,.F.)

*-- End OF lfFndLine.
*:**************************************************************************