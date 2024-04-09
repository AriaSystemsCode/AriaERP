*:***************************************************************************
*: Program file  : ARPINVOG.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR OGIO.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO C100824)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" 
*! Date          : 05/16/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C101855
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVOG
*:***************************************************************************
*: Modifications :
*: B604622,1 BWA 07/02/2001 Fix the bug of the total merchandise and the ship
*: B604622,1                to address for the first invoice is dublicated.
*****************************************************************************

           *--Section of Initializing the variables --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
lcInv      = " "      && variable used in Reset the page # after eash invoice.
llNoRec    = .F.      && Variable used to make the endreport function in the main program.

XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
NEWDOC = .T.
MAXROW = 47

           *--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE ''     TO HLINE4
ENDIF
               *--Section to get the FromZone --*
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED(lcWarHous)
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous ),&lcWarHous..UPS,lcFromZone)
ENDIF
IF !USED(lcSysZone)
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'
                  *--End Section to get the FromZone --*

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT
SCAN FOR &lcASExp
  WAIT WINDOW 'SELECTING RECORDS FOR THE REPORT ...' NOWAIT
  IF lcInv <> Invoice
    lcInv = Invoice
  ENDIF
  XINVOICE = INVOICE

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF NEWDOC
    STORE 0.00 TO XPIECES, XSUBTOTAL
    NEWDOC    = .F.
    XORDER    = ORDER
    XPHONE    = INVHDR.PHONE
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XORDER    = ORDER
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION

    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)

    =lfSolSpAdr()        && function in the main program (ARPINV) to get the adrress.
    XBTNAME  = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = TRIM(laSoldTo[3])
    IF LEN(TRIM(laSoldTo[2])) =0
       XBTADDR2 = laSoldTo[3]
       XBTADDR3 = ''
    ENDIF
    XSTNAME  = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3])
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      XSTADDR3 = ''
    ENDIF

    *** GET THE DESCRIPTION ABOUT THE CODES
    SELECT CODES
    SET ORDER TO CODES IN CODES
    PTERMS     = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA   = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST   = gfCodDes(INVHDR.SPCINST,'SPCINST')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
    
    DO CASE
      CASE 'G' $ lcUpsType
        XZN = gfGetZone(lcUpsType,lcFromZone,SUBSTR(CUSTOMER.cAddress5,1,3))
        XZN = IIF(!EMPTY(XZN),'('+ XZN +')' ,XZN)
      CASE '2' $ lcUpsType
        XZN  = '(12)'
      CASE 'N' $ lcUpsType
        XZN  = '(22)'
      OTHERWISE
        XZN  = ''
    ENDCASE

    PSHIPVIA = IIF(TRIM(PSHIPVIA) = "N/A" , "" ,TRIM(PSHIPVIA)) +XZN
    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------
    IF llPrnFact
      XFNAME  = lcFacName           && Factor company name.
      XFADDR1 = laFactor[1]         && Factor address#1.
      XFADDR2 = laFactor[2]
      XFADDR3 = laFactor[3]
      IF LEN(TRIM(laFactor[2])) =0  && If address#2 is empty fill it with address#3.
        XFADDR2 = laFactor[3]
        XFADDR3 = ''
      ENDIF
    ENDIF  
    CURLOOP = '1'                 && Variable to check if it is the first time for the new document.
  ENDIF

  *-----------------
  * START PRINT
  *-----------------
  =lfGetInHdr()
  
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT INVLINE
  XSTORE = STORE
  XSCALE =  ' '
  ROW    = 24
  DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
     SELECT INVLINE
     IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
       IF ROW >= MAXROW
         =lfGetcont()
         =lfGetInHdr()
         LOOP
       ENDIF
       EXIT
     ENDIF
     IF TOTQTY = 0
        SKIP
        LOOP
     ENDIF
     SELECT INVLINE
     @ ROW,00 SAY STYLE
     @ ROW,22 SAY STYLE.SCALE
     @ ROW,23 SAY QTY1   PICTURE '@Z 9999'
     @ ROW,27 SAY QTY2   PICTURE '@Z 9999'
     @ ROW,31 SAY QTY3   PICTURE '@Z 9999'
     @ ROW,35 SAY QTY4   PICTURE '@Z 9999'
     @ ROW,39 SAY QTY5   PICTURE '@Z 9999'
     @ ROW,43 SAY QTY6   PICTURE '@Z 9999'
     @ ROW,47 SAY QTY7   PICTURE '@Z 9999'
     @ ROW,51 SAY QTY8   PICTURE '@Z 9999'
     @ ROW,55 SAY TOTQTY PICTURE '99999'

     * INVOICE TOTALS
     XLINETOT   = PRICE * TOTQTY
     XPIECES    = XPIECES + TOTQTY
     XSUBTOTAL  = XSUBTOTAL+XLINETOT
     @ ROW,63  SAY PRICE     PICTURE '9999.99'
     @ ROW,71  SAY XLINETOT  PICTURE '999999.99'
     ROW = ROW + 1
     @ ROW,00 SAY 'STYLE DESC : '+STYLE.Desc
     ROW = ROW + 1

     * GET NEXT LINE ITEM
     SELECT INVLINE
     SKIP
  ENDDO
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')
  IF ENDPAGE = '1' .AND. !XNOTE_LOOP
    =lfGetcont()
     LOOP
  ENDIF
  *** Print invoice Notepad.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75

    IF TYPE + KEY <> 'C' + XINVOICE
      SEEK 'C' + XINVOICE
    ENDIF
    lnMemLins = MEMLINES(NOTEPAD.MNOTES)

    IF TYPE + KEY = 'C' + XINVOICE
      @ ROW,02 SAY '* -- N O T E S -- *' 
      ROW = ROW + 1 
      DO WHILE lnNotLine <= lnMemLins
        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
          ROW = ROW + 1
        ENDIF
        IF ROW >= MAXROW
          =lfGetcont()
          =lfGetInHdr()
        ENDIF
        lnNotLine = lnNotLine + 1
      ENDDO
      IF !XNOTE_LOOP
        @ ROW,02 SAY '* -- END OF NOTES -- *'
        lnNotLine = 1
        ROW = ROW + 1 
      ELSE
        =lfGetcont()
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  SELECT INVHDR
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
  @ ROW,54 SAY XPIECES              PICTURE '999999'
  @ ROW,70 SAY XSUBTOTAL            PICTURE '9999999.99'
  ROW=ROW+1

  IF DISCOUNT<>0
     @ ROW,12 SAY 'TOTAL - D I S C O U N T'
     @ ROW,69 SAY DISCOUNT         PICTURE '99999999.99'
     ROW=ROW+1
  ENDIF

  *** Print the tax rate and tax amount 
  IF XTAX .AND. XTAX_METH = 'M' 
    IF INVHDR.TAX_RATE <> 0          && Condition Sales tax <> 0
      @ ROW,12 SAY XTAX_DESC
      @ Row,33 SAY lcTaxRefr
      @ ROW,64 SAY STR(INVHDR.TAX_RATE,5,2)
      @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
    ENDIF
    ROW = ROW + 1
    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'        && Modify %
      @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      Row = Row + 1
    ENDIF 
  ENDIF
  IF FREIGHT + INSUR + COD <> 0
     @ ROW,12 SAY 'TOTAL - F R E I G H T'
     @ ROW,70 SAY FREIGHT + INSUR + COD  PICTURE '9999999.99'
     ROW=ROW+1
  ENDIF

  *** Print the tax rate and tax amount 
  IF XTAX .AND. XTAX_METH = 'A'
    IF INVHDR.TAX_RATE <> 0        && Condition Sales tax <> 0
      @ ROW,12 SAY XTAX_DESC
      @ Row,33 SAY lcTaxRefr
      @ ROW,64 SAY STR(INVHDR.TAX_RATE,5,2) 
      @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
    ENDIF  
    ROW = ROW + 1
    IF InvHdr.nPSTAmt <> 0
      @ Row,12 SAY 'P S T   T A X'
      @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
      @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
      Row = Row + 1
    ENDIF
  ENDIF

   @ 54,01 SAY XPHONE SIZE 1,16
   @ 54,18 SAY XNOTE1 + ' ' + XNOTE2

   IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
      @ 56,10 SAY lcRpMsg1
      @ 57,10 SAY lcRpMsg2
   ENDIF

   IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ 59,56 SAY 'APPROVAL: ' + INVHDR.APPROVAL
   ENDIF
   @ 60,00 SAY INVHDR.PIKTKT
   @ 60,08 SAY ShipDate
   @ 60,17 SAY CARTONS               PICTURE '@Z 999'
   @ 60,25 SAY WEIGHT                PICTURE '@Z 99999'
   @ 60,35 SAY INVHDR.INVOICE
   @ 60,70 SAY INVHDR.TOTALCHG      PICTURE '9999999.99'

   *B604622,1 BWA 07/02/2001 Fix the bug of the total merchandise and the ship
   *B604622,1                to address for the first invoice is dublicated.[START]
   SELECT INVHDR
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN
   ELSE
      NEWDOC = .T.
   ENDIF

  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
  *B604622,1 BWA 07/02/2001. [END]

ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 05/16/2000
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVOG.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,lcSyszone),'',&lcSyszone..ZONE)

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 05/16/2000
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetInHdr()
*!*************************************************************
FUNCTION lfGetInHdr

* LINE 2
SELECT INVHDR
@ 02,40 SAY INVDATE
@ 02,50 SAY 'INVOICE: '+XINVOICE
@ 02,68 SAY 'PO: '+LEFT(CUSTPO,10)

* LINE 3
IF llPrntComp
  @ 03,05 SAY HLINE1
ENDIF

IF SEEK('M'+INVHDR.ACCOUNT , 'CUSTOMER') AND !EMPTY(CUSTOMER.DUNS)
  @ 03,50 SAY 'DUNS: '+ ALLTRIM(CUSTOMER.DUNS)
ENDIF

* LINE 4
IF llPrntComp
  @ 04,05 SAY HLINE2
ENDIF
  
IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
   @ 04,50 SAY 'R E M I T   T O:'
ENDIF

* LINE 5
IF llPrntComp
   @ 05,05 SAY HLINE3
ENDIF
IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
   @ 05,50 SAY XFNAME
ENDIF

* LINE 6
IF llPrntComp
   @ 06,05 SAY HLINE4
ENDIF
IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
   @ 06,50 SAY XFADDR1
ENDIF

* LINE 7
IF llPrntComp
  @ 07,05 SAY XPHONE SIZE 1,16
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
   @ 07,50 SAY XFADDR2
ENDIF

* LINE 8
IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
   @ 08,50 SAY XFADDR3
ENDIF

* LINE 10
@ 10,10 SAY XBTNAME
IF CURLOOP = '1'
   @ 10,47 SAY XSTNAME
ENDIF

* LINE 11
@ 11,10 SAY XBTADDR1
IF CURLOOP = '1'
   @ 11,47 SAY XSTADDR1
ENDIF

* LINE 12
@ 12,10 SAY XBTADDR2
IF CURLOOP = '1'
   @ 12,47 SAY XSTADDR2
ENDIF

* LINE 13
@ 13,10 SAY XBTADDR3
IF CURLOOP = '1'
   @ 13,47 SAY XSTADDR3
ENDIF
@ 17,01 SAY ACCOUNT
@ 17,09 SAY LEFT(CUSTPO,10)
@ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
@ 17,30 SAY DEPT
@ 17,36 SAY ORDER
@ 17,44 SAY LEFT(PTERMS,14)
@ 17,60 SAY REP1
@ 17,64 SAY REP2
@ 17,68 SAY PSHIPVIA
   
*------------------
* PRINT SCALES
*------------------

lcScalStr = SPACE(0)
lcInvNo   = INVHDR.INVOICE
DO lpGtScl WITH lcInvNo
SELECT Scale
Row = 19
FOR lnI = 1 TO LEN( lcScalStr )
  lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  IF !EMPTY(lcSizeScal)
    SEEK 'S' + lcSizeScal
    @ Row,22 SAY lcSizeScal
    @ Row,24 SAY SUBSTR(SCALE.SZ1,1,3)
    @ Row,28 SAY SUBSTR(SCALE.SZ2,1,3)
    @ Row,32 SAY SUBSTR(SCALE.SZ3,1,3)
    @ Row,36 SAY SUBSTR(SCALE.SZ4,1,3)
    @ Row,40 SAY SUBSTR(SCALE.SZ5,1,3)
    @ Row,44 SAY SUBSTR(SCALE.SZ6,1,3)
    @ Row,48 SAY SUBSTR(SCALE.SZ7,1,3)
    @ Row,52 SAY SUBSTR(SCALE.SZ8,1,3)
     
    Row = Row + 1
  ENDIF  
ENDFOR

Row = 24
*--End of function lfGetInHdr --*

*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 08/02/2000
*! Purpose   : PRINT THE FOOTER OF THE INVOICE
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetcont()
*!*************************************************************
FUNCTION lfGetcont

@ ROW+1,12 SAY 'C O N T I N U E D ...'
SELECT INVHDR
@ 54,01 SAY XPHONE SIZE 1,16
@ 54,18 SAY INVHDR.NOTE1 + ' ' + INVHDR.NOTE2

IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
  @ 56,10 SAY lcRpMsg1
  @ 57,10 SAY lcRpMsg2
ENDIF
@ 60,02 SAY INVHDR.PIKTKT
@ 60,09 SAY ShipDate
@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY XINVOICE
@ 60,71 SAY '******.**'

*!*************************************************************
*! Name      : lpGtScl
*! Developer : BASSEM RAFAAT 
*! Date      : 05/16/2000
*! Purpose   : Build a string holding the first uniqe 5 scales
*!           : from the invoice line scal.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpGtScl()
*!*************************************************************
PROCEDURE lpGtScl
PARAMETERS lcInvNo
PRIVATE lcAlias, lnCount

lcAlias = ALIAS()
SELECT InvLine
lcExp   = Invoice + STR(LineNo,6)   && To hold the invocie before the scan.
lnCount = 1
SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 5
  IF !( Scale $ lcScalStr )
    lcScalStr = lcScalStr + Scale
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN
= SEEK(lcExp ,"InvLine")
SELECT (lcAlias)
*-- EOF( lpGtScl )