*:***************************************************************************
*: Program file  : ALPKTKNA.PRG
*: Program desc. : CUSTOMIZED PICK TICKET FOR NAP.
*:                 PRINT PICK TICKET - 66 LINE PAGE, 8 1/2" x 11"
*: Date          : 06/28/2001
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Ahmed Abdel Naby (AAN)
*: Tracking Job Number: C102342
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKNA
*:***************************************************************************
*: Modifications :
*: B605777,1 BWA 04/01/2002 Modify the sorting to be sort with the style.
***************************************************************************

*B605777,1 BWA 04/01/2002 Modify the sorting to be sort with the style.[START]
SELECT (lcTmpOrdL)
INDEX ON PikTkt + Order + cGrupDetal + Style + STR(LineNo , 6) TAG (lcTmpOrdL)
*B605777,1 BWA 04/01/2002.[END]

DECLARE laSoldTo[5,1] , laShipTo[5,1]
PUBLIC lcDistCntr
PRIVATE XPIECES , XTERMS
STORE 0 TO XPIECES , XTERMS
llNoRec = .F.
lcDistCntr =''
laSoldTo = ''           && Array to hold the Sold To address
laShipTo = ''           && Array to hold the Ship To address

XDYELOT_S = IIF(ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y', .T. , .F.)
llWareHous = IIF(ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)) = 'Y', .T. , .F.)

SELECT (lcTmpOrdL)
GO TOP
IF EOF()
  llNoRec = .T.
  =gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcWareCode = SPACE(28)
lcWName    = SPACE(28)
lcWaddr1   = SPACE(28)
lcWaddr2   = SPACE(28)
lcWCity    = SPACE(28)
lcWState   = SPACE(28)
lcWZip     = SPACE(28)

ROW     = 0   
*new
llSkipVe = .F.
*new

lcStore = ' ' 
llCont  = .F. 

lcCstPO  = ' '   && Variable to hold cust po.
lcStrln1 = ' '   && Variable to hold string of line1.

SELECT ORDLINE

   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
A='-------------------------------------------------------------------------------'
M='---------------------------------------------------------------'
B='  SHIPVIA       | SEASON |SPCL INSTRUCTIONS | STORE#   | DEPT# | PURCH ORDER  |'
C='       G STYLE        COLOR      DESCRIPTION                  '     && ARH 09/09/93
D='   _______ ___ ____________________ ___  ___  ___  ___  ___  ___ '

E='|  BILL OF LADING | # CARTONS | WEIGHT| PICKED BY | PACKED BY |'

   *....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....

*0....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....
*   G STYLE   CLR     DESCRIPTION              DYELOT #         PRICE      AMOUNT
*12 X 1234567 123 12345678901234567890                        1234.99  123,456.99
*     SIZE: 12345 12345 12345 12345 12345 12345 12345 12345
*     ORDR: 12345 12345 12345 12345 12345 12345 12345 12345
*     ALLO: 12345 12345 12345 12345 12345 12345 12345 12345
*     PICK: 12345 12345 12345 12345 12345 12345 12345 12345


*------------------------------
* SECTION: MAIN LOOP
*------------------------------
NEWDOC = .T.

*WAIT WINDOW 'PRINT PICK TICKETS - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

XTIME = TIME()
SELECT (lcTmpOrdL)
DO WHILE !EOF() .AND. INKEY() <>32
  WAIT WINDOW 'Printing Pick Ticket ...' + PIKTKT NOWAIT
  SELECT (lcTmpOrdL)
  *----------------------------
  * GET ORDERHDR & LINE ITEMS
  *----------------------------
  IF NEWDOC
    SELECT ORDHDR
    lcWare  = CWARECODE
    SELECT (lcTmpOrdL)
    XPIKTKT  = PIKTKT
    XORDER   = ORDER
    XDATE    = PIKTKT.DATE
    XSTORE   = PIKTKT.STORE
    IF llWareHous .AND. SEEK(lcWare,'WareHous')
      lcWDesc = 'WARHSE: '+ WareHous.cWareCode+ ' '+SUBSTR(WareHous.cDesc,1,18)
    ELSE
      lcWDesc = SPACE(1)
    ENDIF

    SELECT SycComp
    SEEK gcAct_Comp
    lcCompPhon = cCom_Phon
    lcPhonPict = gfPhoneTem()
    *Hline5 = WareHous.cPhone
    Hline5 = TRANSFORM(lcCompPhon,lcPhonPict)
    SELECT ORDHDR
    XMULTI   = MULTI
    XACCOUNT = ACCOUNT

    SELECT CUSTOMER
    STORE 0.00 TO XORDTOT, XVALUE , SVALUE

    SELECT ORDLINE
    SET ORDER TO OrdLinst

    =SEEK('O'+XORDER+XSTORE)
    IF ORDHDR.MultiPO   
      lcCustPO=CustPO
    ENDIF 
    IF PIKTKT <> XPIKTKT
      LOCATE REST FOR PIKTKT=XPIKTKT;
                  WHILE ORDER+STORE=XORDER+XSTORE   
    ENDIF

    *IF ORDER+STORE <> XORDER+XSTORE   
      *SELECT &lcTmpOrdL
      *SKIP
      *LOOP
    *ENDIF
  ENDIF
********


  =lfPrntHdr()
  SELECT (lcTmpOrdL)

  *new
  NEWDOC =.T.
  *new

  *B605777,1 BWA 04/01/2002 Modify the sorting to be sort with the style.[START]
  *SCAN REST WHILE PikTkt + Order + cGrupDetal + STR(LineNo , 6) = ;
                  XPIKTKT + XORDER FOR  LINENO <> 0

  SCAN REST WHILE PikTkt + Order + cGrupDetal + Style + STR(LineNo , 6) = ;
                  XPIKTKT + XORDER FOR  LINENO <> 0
  *B605777,1 BWA 04/01/2002.[END]

    SELECT (lcTmpOrdL)
    ***NEWDOC  = .T.
    XSTORE  = STORE
    XTOTQTY = 0
    * TEST PAGE OVERFLOW
    KEY = STYLE
    SELECT STYLE
    =SEEK(KEY)
    XSTYDESC =  DESC
    SELECT (lcTmpOrdL)
    @ ROW,00 SAY LINENO 
    @ ROW,07 SAY GROUP
    @ ROW,09 SAY STYLE
    @ ROW,29 SAY SUBSTR(XSTYDESC,1,19)
    IF XDYELOT_S
      @ ROW,49 SAY DYELOT
    ENDIF
    ROW = ROW+1
    XSCALE = GETSCALE(SCALE,SPACE(1) )
    @ ROW,12 SAY XSCALE
    ROW =ROW+1
    @ ROW,05 SAY 'PICK:'
    @ ROW,12 SAY QTY1     PICTURE '99999'
    @ ROW,18 SAY QTY2     PICTURE '99999'
    @ ROW,24 SAY QTY3     PICTURE '99999'
    @ ROW,30 SAY QTY4     PICTURE '99999'
    @ ROW,36 SAY QTY5     PICTURE '99999'
    @ ROW,42 SAY QTY6     PICTURE '99999'
    @ ROW,48 SAY QTY7     PICTURE '99999'
    @ ROW,54 SAY QTY8     PICTURE '99999'
    @ ROW,61 SAY TOTQTY   PICTURE '999999'
    * SUM ORDER TOTALS
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
    ROW=ROW+2
 
    * GET NEXT LINE ITEM
    *------------------------ END PRINT LINE LOOP ----------------------

    *IF XPIECES >0
      *ROW = ROW + 1
      *@ ROW,30 SAY "TOTAL PIECES TO PICK ............"  
      *@ ROW,61 SAY XPIECES      PICTURE '999999'
      *ROW = ROW + 1
    *ENDIF
 
    IF !NEWDOC
      LOOP
    ENDIF
    
    *B605777,1 BWA 04/01/2002 Comment the update of the prtflag field because it updated in the main program.[START]
    *SELECT PIKTKT
    *=SEEK(&lcTmpOrdL..PIKTKT)
    *REPLACE NEXT 1 PRTFLAG WITH 'P'
    *B605777,1 BWA 04/01/2002.[END]

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
      CASE ROW>=47
        NEWDOC=.F.
        EXIT
    ENDCASE
  ENDSCAN
  *new
  IF XPIECES >0
    ROW = ROW + 1
    @ ROW,30 SAY "TOTAL PIECES TO PICK ............"  
    @ ROW,61 SAY XPIECES      PICTURE '999999'
    ROW = ROW + 1
  ENDIF

  IF ROW>=47 AND NEWDOC=.F.
    @ 57,28 SAY '** CONTINUED NEXT PAGE **'
    @ 58,00 SAY M
  ENDIF

  llSkipVe = .T.

  IF NEWDOC
    IF llSkipVe
      llSkipVe = .F.
      SELECT (lcTmpOrdL)
      SKIP -1
    ENDIF
    =lfFooter()
  ENDIF
  SKIP
ENDDO
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
*!*************************************************************
*! Name : PROCEDURE lpHeader
*! DATE : 06/28/2001
*! Developer: Ahmed Abdel Naby (AAN)
*!*************************************************************
*! Synopsis : To print the header before each page.  
*!*************************************************************
*! Calls : 
*!         Procedures : None.
*!         Functions  : None.
*!*************************************************************

PROCEDURE lpHeader
SELECT ORDHDR

* LINE 1
@ 00,01 SAY 'P A C K I N G   L I S T '

@ 00,70 SAY gdSysDate


@ 02,01 SAY 'TICKET: '+XPIKTKT
@ 02,16 SAY XDATE

@ 03,01 SAY 'ORDER : '+ORDER
@ 03,16 SAY SUBSTR(DTOC(START),1,5)
@ 03,24 SAY SUBSTR(DTOC(COMPLETE),1,5)



@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'

@ 07,04 SAY ACCOUNT

@ 07,10 SAY PHONE

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

SELECT ORDHDR

@ 16,01 SAY XSHIPVIA
@ 16,18 SAY SUBSTR(XSEASON,1,7)
                                      
@ 16,26 SAY XSPCINST
*aan[new]
*@ 16,47 SAY &lcOrdLTmp..STORE
@ 16,47 SAY &lcTmpOrdL..STORE
*aan[new]
@ 16,58 SAY DEPT
@ 16,66 SAY IIF(MultiPO,lcCustPO,CUSTPO)

@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A
ROW=20

RETURN
*--------------------------------
*    END of program
*--------------------------------

*!*************************************************************
*! Name      : lfPrntHdr
*! DATE      : 06/28/2001
*! Developer : Ahmed Abdel Naby (AAN)
*! Purpose   : Print report header.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None 
*!*************************************************************
*! Example            : 
*!*************************************************************
FUNCTION lfPrntHdr
PRIVATE lcAlias
lcAlias = SELECT(0)

SET DEVICE TO PRINT
=lfGetAddr()
XBTNAME  = lcSolTName
XBTADDR1 = laSoldTo[1]
XBTADDR2 = laSoldTo[2]
XBTADDR3 = ALLTRIM(laSoldTo[3]) + ' ' +ALLTRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
IF LEN(TRIM(XBTADDR2)) =0
  XBTADDR2 = XBTADDR3
  XBTADDR3 = ''
ENDIF
*------------------------------------
* GET DESCRIPTIONS FOR CODED FIELDS
*------------------------------------
XSEASON  = gfCodDes(ORDHDR.Season , 'SEASON')
XTERMS   = gfCodDes(ORDHDR.ctermcode,'cTermCode')
XSHIPVIA = IIF(ORDHDR.ShipVia ='*',gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'),gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'))
XSPCINST = gfCodDes(ORDHDR.SPCINST , 'SPCINST   ')
HLINE1   = gfCodDes(ORDHDR.CDivision , 'CDIVISION ')
*******************************************************************

STORE "" TO XSTNAME,XSTADDR1,XSTADDR2,XSTADDR3,XSTADDR4
XPIECES = 0
*-------------------- END PICK TKT/ORDER SELECTION ---------------


*--------------------------------------------
* SHIP-TO ADDRESS FOR THIS STORE
*--------------------------------------------
XSTNAME  = lcShpTName
XSTADDR1 = laShipTo[1]
XSTADDR2 = laShipTo[2]
XSTADDR3 = ALLTRIM(laShipTo[3]) + ' ' +ALLTRIM(laShipTo[4]) + ' ' + laShipTo[5]
     
IF LEN(TRIM(XSTADDR2)) =0
  XSTADDR2 = XSTADDR3
  XSTADDR3 = ''
ENDIF


SELECT OrdHdr
* LINE 1

@ 00,01 SAY 'P I C K I N G   T I C K E T'

lcWareCode = lcware
=lfGetWareh()
@ 00,35 SAY lcWName
@ 00,70 SAY gdSysDate
@ 01,01 SAY 'TICKET: '+XPIKTKT
@ 01,16 SAY XDATE
@ 01,35 SAY lcWaddr1
@ 02,01 SAY 'ORDER : '+ORDER
@ 02,16 SAY SUBSTR(DTOC(START),1,5)
@ 02,24 SAY SUBSTR(DTOC(COMPLETE),1,5)
@ 02,35 SAY  lcWaddr2
@ 02,70 SAY XTIME

@ 03,01 SAY IIF(!EMPTY(APPROVAL),'APPRVL: '+APPROVAL,'')
@ 03,35 SAY lcWCity +SPACE(02)+lcWState+SPACE(02)+lcWZip
IF llWareHous .AND. !EMPTY(lcWDesc)
  @ 04,01 SAY lcWDesc
ENDIF

@ 04,35 SAY HLINE5
@ 06,04 SAY '.....SOLD TO .....'
@ 06,46 SAY '.....SHIP TO .....'

@ 07,04 SAY ACCOUNT

@ 07,10 SAY PHONE
   
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

*If store changed get new store
IF llCont
  llCont = .F.
ELSE
*aan[new]
  *lcStore = &lcOrdLTmp..STORE
  lcStore = &lcTmpOrdL..STORE
*aan[new]  
ENDIF  

SELECT ORDHDR
* Concatinate variables with vertical lines (Begin)
lcCstPO  = IIF(MultiPO,lcCustPO,CUSTPO)
lcStrln1 = PADR(XSHIPVIA,16)+"|"+PADR(SUBSTR(XSEASON,1,7),8)+"|"+;
           PADR(XSPCINST,18)+"|"+PADR(lcStore,10)+"|"+;
           PADR(DEPT,7)+"|"+PADR(lcCstPO,14)+"|"
@ 16,00 SAY lcStrln1
             
@ 17,00 SAY A
@ 18,00 SAY C
@ 19,00 SAY A
ROW=20

SELECT (lcAlias)
*:*************************************************************
*: Name      : lfGetWareh
*: DATE      : 06/28/2001
*: Developer : Ahmed Abdel Naby (AAN)
*: Purpose   : To get the warehouse information.
*:*************************************************************
*: Example   :  lfGetWareh()
*:*************************************************************
FUNCTION lfGetWareh
*-- To get the warehouse address.

*IF llWareHous
  IF SEEK(lcWareCode,'WareHous')
    lcWName    = ALLTRIM(WareHous.cDesc)
    lcWaddr1   = ALLTRIM(WareHous.cAddress1)
    lcWaddr2   = ALLTRIM(WareHous.cAddress2)
    lcWCity    = ALLTRIM(WareHous.cAddress3)
    lcWState   = ALLTRIM(WareHous.cAddress4)
    lcWZip     = ALLTRIM(WareHous.cAddress5)
  ELSE
    lcWName    = SPACE(28)
    lcWaddr1   = SPACE(28)
    lcWaddr2   = SPACE(28)
    lcWCity    = SPACE(28)
    lcWState   = SPACE(28)
    lcWZip     = SPACE(28)
  ENDIF 
*ELSE
*  lcWName    = ALLTRIM(xWHCompany)
*  lcWaddr1   = ALLTRIM(XWhAddr1)
*  lcWaddr2   = ALLTRIM(XWhAddr2)
*  lcWCity    = ALLTRIM(XWhCity)
*  lcWState   = ALLTRIM(XWhState)
*  lcWZip     = ALLTRIM(XWhZip)
*ENDIF

*!**************************************************************************
*! Name      : lfGetAddr
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 01/07/2001
*! Purpose   : get the address of the customer
*!**************************************************************************
*! Example   : = lfGetAddr()
*!**************************************************************************
*
FUNCTION lfGetAddr
PARAMETER llDummy

lcCurrAlis = ALIAS()
laSoldTo = ''
laShipTo = ''


=SEEK(IIF(EMPTY(PIKTKT.Store) , 'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store),'Customer')

lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)), 'SHIPVIA')

lcSolTName = CUSTOMER.BTName


= SEEK(IIF(EMPTY(PIKTKT.Store) ,'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store) ,'Customer')

IF Customer.Type = 'S' AND Customer.billto = 'M'
  GO TOP IN 'Customer'
  = SEEK('M'+ PIKTKT.Account ,'Customer')
  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , '2')
ELSE
  = gfGetAdr('Customer', '' , '' , '' , @laSoldTo , '2')
ENDIF  


IF ORDHDR.Alt_ShpTo
  
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else
  = SEEK(IIF(EMPTY(PIKTKT.Store) ,'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store) ,'Customer')
  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    =SEEK('S' + PIKTKT.Account + lcDistCntr)
  ELSE
    =SEEK(IIF(EMPTY(PIKTKT.Store) , 'M'+ PIKTKT.Account,'S' + PIKTKT.Account + PIKTKT.Store))
  ENDIF
  
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  *laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(Customer.Phone1 , lcPhonPict)
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5))
ENDIF    && End of IF
SELECT (lcCurrAlis)
RETURN ''
*-- End of lfGetAddr

*!**************************************************************************
*! Name      : lfFooter
*! Developer : Ahmed Abdel Naby (AAN)
*! Date      : 01/07/2001
*! Purpose   : Print the footer
*!**************************************************************************
*! Example   : = lfFooter()
*!**************************************************************************
*
FUNCTION lfFooter

*------------------------
* [NOTEPAD] COMMENT LINES
*------------------------
IF llRpOrdNot .AND. NEWDOC
  SELECT NOTEPAD
  IF SEEK('B'+XORDER)
    lnMemoWdth = SET('MEMOWIDTH')
    SET MEMOWIDTH TO 75
    lnMemoLnNo = 0      
    lnMemolins = 0
    ROW = ROW +1
    @ ROW,02 SAY '*-- N O T E P A D --*'
    ROW = ROW +1
    lnMemolins = MEMLINES(mNotes)
    IF lnMemolins > 0
      DO WHILE lnMemoLnNo <> lnMemolins .AND. INKEY() <> 32
        IF ROW > 47
          ROW = ROW + 1
          @ ROW,2 SAY 'CONTINUED NEXT PAGE ...'
          llCont  = .T.
          SELECT ORDHDR
          =lfPrntHdr()
          SELECT NotePad
        ENDIF
        lnMemoLnNo = lnMemoLnNo + 1
        @ ROW,02 SAY MLINE(mNotes,lnMemoLnNo)
        ROW = ROW + 1
      ENDDO
    ENDIF
    @ ROW,02 SAY '*-- END OF NOTEPAD --*'
    lnMemoLnNo = 0      
    lnMemolins = 0
    SET MEMOWIDTH TO (lnMemoWdth)
  ENDIF
ENDIF

SELECT (lcTmpOrdL)
IF XSTORE = STORE .AND. !NEWDOC
  @ 57,28 SAY '** CONTINUED NEXT PAGE **'
  @ 58,00 SAY M
ELSE
  @ 52,01 SAY ORDHDR.NOTE1
  @ 52,41 SAY ORDHDR.NOTE2
  @ 53,00 SAY M
  @ 54,00 SAY E
  F="|                 |           |       |           |           |"
  @ 55,00 SAY F
  @ 56,00 SAY M

  IF llRpStyPrc
    H="|  $"+PADL(XVALUE,12)+"  | $         | $     | $         |"+;
       PADR(SUBSTR(XTERMS,1,10),11)+"|               |"
  ELSE
    H="|  $              | $         | $     | $         |"+;
       PADR(SUBSTR(XTERMS,1,10),11)+"|               |"
  ENDIF
  XVALUE   = 0.00
ENDIF
IF LEN(ALLTRIM(lcRpMsg1+lcRpMsg2+lcRpMsg3)) > 0
  @ 60,00 SAY lcRpMsg1
  @ 61,00 SAY lcRpMsg2
  @ 62,00 SAY lcRpMsg3
ENDIF
