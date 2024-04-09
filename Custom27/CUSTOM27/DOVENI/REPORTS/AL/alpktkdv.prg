****************************************************************************
*: Program file  : ALPKTKDV.PRG 
*: Program desc. : PRINT PICKING TICKETS FORM.
*: System        : Aria Apparel System (A27).
*: Module        : Sales Order Allocation  (AL)
*: Developer     : ABDOU ELGENDI - (ABD) 
*: Date          : 11/22/1999
*:**************************************************************************
*: Calls : FUNCTIONS  : 
*:**************************************************************************
*: Passed Parameters  : None
*:**************************************************************************
*: Due to C#101708,1
*:**************************************************************************
*: Modifications :
*:**************************************************************************
*
*-- Define variables. [ Begin ]
 STORE '' TO lcDivLName
 DIMENSION laDivLName[1,2] , laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1]
 laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
 laDivLName[1,2] = 'lcDivLName'
 laCompAdd = ''           && Array to hold the Company address
 laSoldTo  = ''           && Array to hold the Sold To address
 laShipTo  = ''           && Array to hold the Ship To address
*-- Define variables. [ End ]

*-- If Dyelots used.
XDYELOT_S = ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y'

*-- Fill the array with the Company address.
SELECT SYCCOMP
SEEK gcAct_Comp
laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)

lcOrdTmp = gftempName()

   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
 A='-------------------------------------------------------------------------------'
 B='  SHIPVIA       |SEASON  |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
 C='       G STYLE   CLR     DESCRIPTION                          PRICE      AMOUNT'     
D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___  ___  ___ ____'
E='|  BILL OF LADING | # CARTONS | WEIGHT| PICKED BY | PACKED BY | SHIPPED VIA   |'
G='|  MERCHANDISE    |  FREIGHT  | INSUR | OTHER CHGS| TERMS     | COMMENTS:     |'
H='|  $              | $         | $     | $         |           |               |'
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*   G STYLE   CLR     DESCRIPTION              DYELOT #         PRICE      AMOUNT
*12 X 1234567 123 12345678901234567890                        1234.99  123,456.99
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345
   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

*-------------
*-- MAIN LOOP
*-------------
SELECT ORDLINE
lcOldOrL = ORDER()
SET ORDER TO TAG Ordlinst IN ORDLINE
NEWDOC = .T.
STORE &lcTmpOrdL..ORDER To LORDER,HORDER
lnNotLine = 1
CLEAR TYPEAHEAD
SET DEVICE TO PRINTER 
XTIME = TIME()

SELECT (lcTmpOrdL)
Scan While INKEY() <>32 FOR Piktkt.FLAG != 'N' 
  WAIT WINDOW 'Print Pick Tickets ... <Space Bar> To Abort' NOWAIT  
  *-------------------------------*
  *   GET ORDERHDR & LINE ITEMS   *
  *-------------------------------*
  IF NEWDOC
    SELECT (lcTmpOrdL)
    XPIKTKT  = PIKTKT
    XORDER   = ORDER
    XDATE    = Piktkt.DATE    
    XSTORE   = Piktkt.STORE
    LORDER   = IIF(XORDER<LORDER, XORDER, LORDER)
    HORDER   = IIF(XORDER>HORDER, XORDER, HORDER)
    STORE 0.00 TO XVALUE
    SELECT ORDLINE
    = SEEK ('O'+XORDER+XSTORE,'ORDLINE')
    lcCustPO = IIF(ORDHDR.MultiPO,ORDLINE.CustPO,'')
    IF PIKTKT <> XPIKTKT
      LOCATE REST FOR PIKTKT = XPIKTKT;
                WHILE 'O'+ORDER+STORE = 'O'+XORDER+XSTORE
    ENDIF
    IF 'O'+ORDER+STORE <> 'O'+XORDER+XSTORE
      LOOP
    ENDIF
    IF USED(lcOrdTmp)
      USE IN (lcOrdTmp)
    ENDIF
    SELECT ORDLINE
    COPY REST TO &gcWorkDir.&lcOrdTmp FOR PIKTKT=XPIKTKT;
                              WHILE 'O'+ORDER+STORE = 'O'+XORDER+XSTORE
    =gfOpenFile(gcWorkDir+lcOrdTmp,'','EX')
    SELECT (lcOrdTmp)
    INDEX ON cordtype+order+STR(lineno,6)  TAG &lcOrdTmp
    
    *-- Function to Get the Sold to Address & Ship to Address & the Description
    *-- of the Ship Via , Season ,Special Instructions , Terms
    = lfSolSpAdr()
    *-----------------------------------*
    * GET DESCRIPTIONS FOR CODED FIELDS *
    *-----------------------------------*
    
    *-- Get the TermCode Descrption.
    XTERMS  = lcTerms
    
    *-- Get the ShipVia Descrption.
    XSHIPVIA = lcShipVia
    
    *-- Get the Spcial Inst.  Descrption.
    XSPCINST  = lcSpcInst
      
    *-- Get the Season Descrption.       
    XSEASON  = lcSeason
    
    =gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')
    laCompAdd[1] = IIF(!EMPTY(lcDivLName),lcDivLName,laCompAdd[1])
    XPIECES = 0
  ENDIF
  *-------------------- END PICK TKT/ORDER SELECTION ---------------*

  SELECT ORDHDR
  @ 00,01 SAY 'P I C K I N G   T I C K E T'
  @ 00,34 SAY SUBSTR(laCompAdd[1],1,36)
  @ 00,70 SAY DATE()
  @ 01,35 SAY laCompAdd[2]
  @ 02,01 SAY 'TICKET: '+XPIKTKT
  @ 02,16 SAY XDATE
  @ 02,35 SAY laCompAdd[3]
  @ 02,70 SAY XTIME
  @ 03,01 SAY 'ORDER : '+ORDER
  @ 03,16 SAY SUBSTR(DTOC(START),1,5)
  @ 03,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
  @ 03,35 SAY laCompAdd[4]
  
  *-- Print Approval code if it's not empty.
  @ 04,01 SAY IIF(!EMPTY(APPROVAL),'APPRVL: '+APPROVAL,'')
  @ 04,35 SAY laCompAdd[5]
  @ 06,04 SAY '.....SOLD TO .....'
  @ 06,46 SAY '.....SHIP TO .....'
  @ 07,04 SAY ACCOUNT
  @ 07,10 SAY PHONE
  @ 07,46 SAY IIF(!EMPTY(XSTORE),'STORE#: ' + XSTORE,'')
  @ 08,04 SAY lcSolTName
  @ 08,46 SAY lcShpTName
  @ 09,04 SAY laSoldTo[1]
  @ 09,46 SAY laShipTo[1]
  @ 10,04 SAY laSoldTo[2]
  @ 10,46 SAY laShipTo[2]
  @ 11,04 SAY laSoldTo[3]
  @ 11,46 SAY laShipTo[3]

  @ 13,00 SAY A
  @ 14,00 SAY B
  @ 15,00 SAY A

  SELECT ORDHDR
  @ 16,01 SAY SUBSTR(XSHIPVIA,1,15)
  @ 16,16 SAY '|'
  @ 16,17 SAY SUBSTR(XSEASON,1,8)
  @ 16,25 SAY '|'
  @ 16,26 SAY SUBSTR(XSPCINST,1,10)
  @ 16,44 SAY '|'
  @ 16,46 SAY SUBSTR(&lcOrdTmp..STORE,1,10)
  @ 16,55 SAY '|' 
  @ 16,58 SAY SUBSTR(DEPT,1,7)
  @ 16,63 SAY '|' 
  @ 16,66 SAY IIF(MultiPO,SUBSTR(lcCustPO,1,12),SUBSTR(CUSTPO,1,12))
  @ 16,78 SAY '|' 
  @ 17,00 SAY A
  @ 18,00 SAY C
  @ 19,00 SAY A
  ROW=20
  *---------------*
  *  LINE LOOP    *
  *---------------*
  SELECT (lcOrdTmp)
  NEWDOC  = .T.
  XSTORE  = STORE
  XTOTQTY = 0
  Scan WHILE !EOF()
    DO CASE
      CASE EOF()
        NEWDOC = .T.
        EXIT
      CASE STORE # XSTORE
        NEWDOC = .F.
        EXIT
      CASE TOTPIK<=0
        LOOP
      CASE ROW>=47
        NEWDOC=.F.
        EXIT
    ENDCASE
    =SEEK (STYLE,'STYLE')
    XSTYDESC =  STYLE.DESC
    SELECT (lcOrdTmp)
    @ ROW,00 SAY LINENO 
    @ ROW,07 SAY GROUP
    *-- Adjust the style and the color to be 7,3.
    @ ROW,09 SAY SUBSTR(STYLE,1,7)
    @ ROW,17 SAY SUBSTR(STYLE,14 ,3)
    @ ROW,21 SAY XSTYDESC
    IF XDYELOT_S
      @ ROW,46 SAY DYELOT
    ENDIF
    IF llRpStyPrc
      XAMOUNT = PRICE*TOTPIK
      @ ROW,60 SAY PRICE        PICTURE '9999.99'
      @ ROW,69 SAY XAMOUNT      PICTURE '999,999.99'
    ENDIF
    ROW = ROW+1
    *--  Adjust the scale to be 5.
    XSCALE = GETSCALE(SCALE,SPACE(1) )
    @ ROW,12 SAY XSCALE
    ROW =ROW+1
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

    *-- SUM ORDER TOTALS
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
    SELECT (lcOrdTmp)
  ENDSCAN  
  *------------------------ END PRINT LINE LOOP ----------------------*
  IF XPIECES >0
    ROW = ROW + 1
    @ ROW,30 SAY "TOTAL PIECES TO PICK .........."
    @ ROW,61 SAY XPIECES      PICTURE '999999'
    ROW = ROW + 1
  ENDIF

  IF llRpOrdNot .AND. NEWDOC
    *-- Print the note pad.(Begin)
    SELECT NotePad
    IF SEEK('B' + XORDER)
      lnOldMemW = SET("MEMOWIDTH")
      SET MEMOWIDTH TO 75
      lnMemLins = MEMLINES(NOTEPAD.MNOTES)
      @ Row,02 SAY '* -- N O T E P A D -- *' 
      Row = Row + 1
      DO WHILE lnNotLine <= lnMemLins
        IF Row >= 53
          Row = 23
        ENDIF
        @ ROW,00 SAY MLINE(MNOTES,lnNotLine)
        ROW = ROW + 1
        lnNotLine = lnNotLine + 1
      ENDDO
      SET MEMOWIDTH TO lnOldMemW
    ENDIF
  ENDIF  
  lnNotLine = 1
  SELECT (lcOrdTmp)
  IF XSTORE = STORE .AND. !NEWDOC
     @ 57,28 SAY '** CONTINUED NEXT PAGE **'
     @ 58,00 SAY A
  ELSE
     @ 52,01 SAY ORDHDR.NOTE1
     @ 52,41 SAY ORDHDR.NOTE2
     @ 53,00 SAY A
     @ 54,00 SAY E
     @ 55,00 SAY '|                 |           |       |           |           |'
     @ 55,64 SAY SUBSTR(XSHIPVIA,1,10)
     @ 55,78 SAY '|'
     @ 56,00 SAY A
     @ 57,00 SAY G
     @ 58,00 SAY '|  $'
     @ 58,04 SAY XVALUE     PICTURE '9999999.99'
     @ 58,18 SAY '| $         | $     | $         |'
     XVALUE   = 0.00
     @ 58,52 SAY SUBSTR(XTERMS,1,10)
     @ 58,54 SAY '|               |' 
     @ 59,00 SAY A
  ENDIF
  @ 60,00 SAY lcRpMsg1
  @ 61,00 SAY lcRpMsg2
  @ 62,00 SAY lcRpMsg3
  IF !NEWDOC
    LOOP
  ENDIF
  SELECT PIKTKT
  = SEEK (&lcTmpOrdL..PIKTKT)

  *-- Function To REPLACE  PRTFLAG WITH 'P' If Device For Printer Only.
  =lfEndGroup()
  
  SELECT(lcTmpOrdL)
  SKIP
ENDSCAN  
SET ORDER TO &lcOldOrL IN ORDLINE
SET DEVICE TO SCREEN
RETURN
*!**************************************************************************
