****************************************************************************
*: Program file      : ARPINVKT.PRG        (C#101665)
*: Program desc.     : PRINT INVOICES      (For KAKTUS)
*: System            : Aria Apparel System (A27)
*: Module            : Accounts Receivable (AR)
*: Developer         : ABDOU ELGENDI       (ABD)
*: Date              : 09/22/1999
*:**************************************************************************
*: Calls : FUNCTIONS : lfGetZone , lfClearRep
*:         PROCEDURE : 
*:**************************************************************************
*: Passed Parameters : None
*:**************************************************************************
*: Modifications     : ........
*:**************************************************************************
*B803869,1 AAN 30/11/2000 Shift SALE AMT label and it's value 5 spaces to the left.
*B804056,1 BWA 05/08/2001 Adj. some fields in the invoice and correcT the flag of the print.
*B804337,1 BWA 07/29/2001 Remove decimals from weight.
*B804350,1 BWA 08/07/2001 Reposition fields on custom invoice.
*C102539,1 SSE 01/31/2001 Custom adding hard coded lines to the special instructions.
*B123728,1 ASH 07/22/2004 Change the special instructions text.
*:**************************************************************************

*--  Variable Declaration.
llNoRec     = .F.
llNote_Loop = .F.
llNewDec    = .T.
lnMaxRow    = 47
DIMENSION laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcZone'
*-- End Of Variable Declaration.

*-- Get the style major and color [Begin.]
STORE 0 TO lnColorLen,lnNonMajSt
lcMajPict  = gfItemMask("PM")
lnMajorLen = LEN(lcMajPict)

*--Get the color & Get the No. of major segments.
lnMajSeg = gfItemMask('SM')
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)
*-- lnMajor Var. is declared in main prg (ARPINV) 
*-- Get the Non Major elements. [Begin.]
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)
  IF laMajSeg[lnI,1] = 'C'
    lnNonMajSt = laMajSeg[lnI,4]
    lnColorLen = LEN(laMajSeg[lnI,3])
    EXIT
  ENDIF
ENDFOR 
*-- Get the Non Major elements. [End.]
*--- If user change filter criteria then you must collect data again [Begin]
*IF llOGFltCh 
  SELECT INVHDR
  USE IN (IIF(USED(INVHTEMP),INVHTEMP,0))
  COPY ALL FOR &lcRpExp TO &gcWorkDir.&INVHTEMP
  = gfOpenFile('&gcWorkDir.&INVHTEMP',' ','EX')
*ENDIF

SELECT (INVHTEMP)
GOTO TOP
IF EOF()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  IF USED(INVHTEMP)
    USE IN (INVHTEMP)
    ERASE &gcWorkDir.&INVHTEMP+'.DBF'
  ENDIF
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ELSE
  *-- Message : --LINE UP <YES>  <NO>  <QUIT>--
  LineUp = gfModalGen('QRM40145B40012','DIALOG' )
  DO CASE
    CASE  LineUp = 3
      IF USED(INVHTEMP)
        USE IN (INVHTEMP)
      ENDIF
      ERASE &gcWorkDir.&INVHTEMP+'.DBF'
      llNoRec = .T.
      SET DEVICE TO SCREEN
      RETURN
    CASE  LineUp = 2
      LineUp = .F.
    CASE  LineUp = 1
      LineUp = .T.
  ENDCASE
ENDIF
SELECT (INVHTEMP)
WAIT WINDOW  'Printing invoice ... <Space Bar> to abort ' NOWAIT

*--  MAIN LOOP --*
lnNotLine = 1
SELECT (INVHTEMP)
SET DEVICE TO PRINT
DO WHILE INKEY() <>32
  SELECT (INVHTEMP)
  IF EOF()
    EXIT
  ENDIF
  lcInvoice = INVOICE
  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records. *
  * If any records are not found, skip to next invoice.            *
  * Initialize document totals.                                    *
  *-----------------------------------------------------------------
  IF llNewDec
    lnPrTerr = 0
    STORE 0.00 TO lnPieces, lnSubTotal
    SELECT INVHDR
    SEEK lcInvoice
    IF EOF()
      SKIP IN (INVHTEMP)
      LOOP
    ENDIF
    llNewDec   = .F.
    lnPhone    = IIF(EMPTY(PHONE),'',PHONE)
    lcOrder    = Order
    lcPikTkt   = Piktkt
    lcAccount  = Account
    lcStore    = Store
    lcSeason   = Season
    xDivision = Cdivision
    *-- GET THE BILL TO AND SHIP TO ADDRESS
    =lfSolSpAdr()

    lcBtName  = lcSolTName
    lcBtAddr1  = laSoldTo[1]
    lcBtAddr2 = laSoldTo[2]
    lcBtAddr3 = laSoldTo[3]

    lcStName  = lcShpTName
    lcStAddr1 = laShipTo[1]
    lcStAddr2 = laShipTo[2]
    lcStAddr3 = laShipTo[3] 
  
    SELECT INVLINE
    SEEK lcInvoice
    IF EOF()
      lnPrTerr = 2
    ENDIF
    *- Print Term Description
    lcTerms = SUBSTR(gfCodDes(INVHDR.CTERMCODE,'CTERMCODE',.T.),1,15)
    *- Print ShipVia Description  
    lcShipVia   = SUBSTR(gfCodDes(INVHDR.SHIPVIA,'SHIPVIA',.T.),1,15)
    lcZone = ''
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
    DO CASE
      CASE 'G' $ lcZone
        XUPSFOM =gfGetmemvar('XUPSFROM') 
        lcZn = lfGetZone(lcZone,XUPSFOM,SUBSTR(CUSTOMER.CADDRESS5,1,3))
        lcZn = IIF(!EMPTY(lcZn),'('+lcZn+')',lcZn)
      CASE '2' $ lcZone
        lcZn  = '(12)'
      CASE 'N' $ lcZone
        lcZn  = '(22)'
      OTHERWISE
        lcZn  = ''
    ENDCASE
    lcShipVia  = TRIM(lcShipVia) + lcZn
    PSPCINST   = SUBSTR(gfCodDes(INVHDR.SPCINST,'SPCINST'),1,15)
    lcDivision = gfCodDes(XDIVISION,'CDIVISION')

    *-- FACTOR NAME & ADDRESS
    lcFName  = lcFacName 
    lcFAddr1 = laFactor[1]
    lcFAddr2 = laFactor[2]
    lcFAddr3 = TRIM(laFactor[3])+' '+laFactor[4]+' '+laFactor[5]
    lnCurLoop = '1'
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
  ENDIF
  *-- END llNewDec
  IF lnPrTerr >0
    SELECT (INVHTEMP)
    SKIP
    LOOP
  ENDIF
  *-----------------
  * START PRINT
  *-----------------
  SELECT INVHDR
  IF gcDevice='PRINTER'
    @ 02,64 SAY CHR(27)+CHR(15)+DTOC(INVDATE)
    @ 02,79 SAY lcInvoice
    @ 02,88 SAY DUEDATE
    @ 03,1  SAY CHR(18)+CHR(18) 
  ELSE
    @ 02,63 SAY INVDATE
    @ 02,71 SAY lcInvoice
    @ 02,77 SAY DUEDATE
  ENDIF
  
  SELECT INVHDR
  IF gcDevice = "PRINTER"
    @ 06,71 SAY CHR(27)+CHR(15)+ "ORDER#   " + ORDER + CHR(18)
    @ 07,71 SAY CHR(27)+CHR(15)+ "PICK#   " + PIKTKT 
    @ 08,01 SAY CHR(18)+CHR(18)
  ELSE
    @ 06,71 SAY "ORDER#" + ORDER
    @ 07,71 SAY "PICK# " + PIKTKT
  ENDIF
  
  @ 09,12 SAY lcBtName
  @ 09,49 SAY lcStName
  @ 10,12 SAY lcBtAddr1
  @ 10,49 SAY lcStAddr1
  @ 11,12 SAY lcBtAddr2
  @ 11,49 SAY lcStAddr2
  @ 12,12 SAY lcBtAddr3
  @ 12,49 SAY lcStAddr3
  
  IF gcDevice='PRINTER'
    @ 16,03  SAY CHR(27)+CHR(15)+CUSTPO
    @ 16,27  SAY DEPT               
    @ 16,38  SAY STORE
    @ 16,59  SAY lcTerms
    @ 16,75  SAY lcShipVia
    @ 16,94  SAY SHIPDATE
    @ 16,107 SAY ACCOUNT
    @ 16,122 SAY REP1
    @ 16,131 SAY REP2
    @ 17,01  SAY CHR(18)+CHR(18)
  ELSE
    @ 16,03 SAY CUSTPO
    @ 16,16 SAY DEPT             
    @ 16,21 SAY STORE
    @ 16,36 SAY lcTerms
    @ 16,44 SAY SUBSTR(lcShipVia,1,10)
    @ 16,55 SAY SHIPDATE
    @ 16,63 SAY ACCOUNT
    @ 16,70 SAY REP1
    @ 16,76 SAY REP2
  ENDIF
  CNT = 1
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT INVLINE
  lcStore = STORE
  ROW     = 20
  DO WHILE lnCurLoop = '1' .AND. !llNote_Loop           
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> lcInvoice .OR. ROW >= lnMaxRow
      EXIT
    ENDIF
    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF
    KEY = INVLINE.STYLE
    =SEEK(KEY,'STYLE')
    XSTYDESC = IIF(FOUND() , SUBSTR(STYLE.DESC,1,17) ,'')
    SELECT INVLINE
    =SEEK("S"+SCALE,"SCALE")
    @ ROW,03 SAY SUBSTR(STYLE,1,lnMajorLen)
    @ ROW,22 SAY SUBSTR(STYLE,lnMajorLen+2,lnColorLen)
    @ ROW,30 SAY XSTYDESC
    @ ROW,58 SAY TOTQTY PICTURE '999999'
    @ ROW,66 SAY PRICE PICTURE '9999.99'
    XLINETOT = PRICE * TOTQTY
    @ ROW,74 SAY XLINETOT  PICTURE '999999.99'
    ROW = ROW + 1     
    lnCol= 30
    FOR lnCount = 1 TO 5
      lcCount = STR(lnCount,1)
      IF QTY&lcCount = 0
        LOOP
      ENDIF
      @ ROW,lnCol SAY   ALLTRIM(SCALE.SZ&lcCount)+"/"+PADR(QTY&lcCount,5)
      lnCol = lnCol+LEN(ALLTRIM(SCALE.SZ&lcCount)+"/"+PADR(QTY&lcCount,5))+1
    ENDFOR
    *-- SUM INVOICE TOTALS
    lnPieces    = lnPieces + TOTQTY
    lnSubTotal  = lnSubTotal+XLINETOT
    ROW = ROW+1
    SELECT INVLINE
    IF LINEUP
      EXIT
    ENDIF
    *-- GET NEXT LINE ITEM
    SKIP
  ENDDO
  *-- END LINE PROCESSING
  SET DEVICE TO PRINT
  IF LINEUP .AND. !llNote_Loop
    EJECT
    lnChoice = gfModalGen('QRM40143B40012','DIALOG' )
    DO CASE
      CASE lnChoice = 3
        RETURN(.F.)
      CASE lnChoice = 2
        LINEUP =.F.
    ENDCASE
    WAIT WINDOW 'Printing invoice ... <Space Bar> to abort ' NOWAIT
    SET DEVICE TO PRINT
    STORE 0.00 TO lnPieces, lnSubTotal
    LOOP
  ENDIF
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE.INVOICE = lcInvoice ,'1','0')
  IF ENDPAGE = '1' .AND. !llNote_Loop                 
    @ ROW+1,12 SAY 'C O N T I N U E D ...'
    
    *B804350,1 BWA 08/07/2001 Reposition fields on custom invoice.[START]
    *IF !EMPTY (INVHDR.PHONE)
    *  @ 50,7 SAY INVHDR.PHONE  PICTURE "@R "+ gfPhoneTem()  SIZE 1,16
    *ENDIF
    *B804350,1 BWA 08/07/2001 [END]
    
    SELECT INVHDR
    *B803869,1 AAN 11/30/2000 Shift SALE AMT label and it's value 5 spaces to the left[Start].
    *@ 51,66 SAY 'SALES AMT'
    *@ 51,76 SAY '******'
    @ 51,61 SAY 'SALES AMT'
    @ 51,71 SAY '******'
    *B803869,1 AAN 11/30/2000 [End].
    ROW=ROW+1

    *B804350,1 BWA 08/07/2001 Reposition fields on custom invoice.[START]
    *@ 54,03 SAY "CARTONS: " + PADR(INVHDR.CARTONS,5) PICTURE '@Z 999'
    @ 54,04 SAY "CARTONS: " + PADR(INVHDR.CARTONS,4) PICTURE '@Z 999'
    *B804350,1 BWA 08/07/2001 [END]

    *B804337,1 BWA 07/29/2001 Remove decimals from  weight.[START]
    *@ 55,03 SAY "WEIGHT : " + PADR(INVHDR.WEIGHT,5)

    *B804350,1 BWA 08/07/2001 Reposition fields on custom invoice.[START]
    *@ 55,03 SAY "WEIGHT : " + PADL(INT(INVHDR.WEIGHT),5)
    @ 55,04 SAY "WEIGHT : " + PADL(INT(INVHDR.WEIGHT),4)
    *B804350,1 BWA 08/07/2001 [END]

    *B804337,1 BWA 07/29/2001 [END]

    LOOP
  ENDIF
  SELECT NOTEPAD
  lnOldMemW = SET("MEMOWIDTH")
  SET MEMOWIDTH TO 75
  IF TYPE + KEY <> 'C' + lcInvoice
    SEEK 'C' + lcInvoice
    
    *C102539,1 Fix bug of variable lnMemLins not found. [Begin]
    *lnMemLins = MEMLINES(NOTEPAD.MNOTES)
    *C102539,1 Fix bug of variable lnMemLins not found. [End]

  ENDIF

  *C102539,1 Fix bug of variable lnMemLins not found. [Begin]
  lnMemLins = MEMLINES(NOTEPAD.MNOTES)
  *C102539,1 Fix bug of variable lnMemLins not found. [End]
  
  IF TYPE + KEY = 'C' + lcInvoice
    @ ROW,02 SAY '* -- N O T E S -- *' 
    ROW = ROW + 1 
    DO WHILE lnNotLine <= lnMemLins
      IF ROW >= lnMaxRow
        llNote_Loop = .T.
        EXIT
      ELSE
        llNote_Loop = .F.
        @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
        ROW = ROW + 1
      ENDIF
      lnNotLine = lnNotLine + 1
    ENDDO
    IF !llNote_Loop
      @ ROW,02 SAY '* -- END OF NOTES -- *'
      lnNotLine = 1
      ROW = ROW + 1 
    ELSE
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      
      *B804350,1 BWA 08/07/2001 Reposition fields on custom invoice.[START]
      *IF !EMPTY (INVHDR.PHONE)
      *  @ 50,07 SAY INVHDR.PHONE PICTURE "@R "+gfPhoneTem()  SIZE 1,16
      *ENDIF
      *B804350,1 BWA 08/07/2001 [END]

      SELECT INVHDR
      *B803869,1 AAN 11/30/2000 Shift SALE AMT label and it's value 5 spaces to the left[Start].
      *@ 51,66 SAY 'SALE AMT'
      *@ 51,76 SAY '******'
      @ 51,61 SAY 'SALE AMT'
      @ 51,71 SAY '******'
      *B803869,1 AAN 11/30/2000 [End].      
      ROW = ROW + 1
      @ 54,04 SAY "CARTONS: " + PADR(INVHDR.CARTONS,4) PICTURE '@Z 999'

      *B804337,1 BWA 07/29/2001 Remove decimals from  weight.[START]
      *@ 55,03 SAY "WEIGHT : " + PADR(INVHDR.WEIGHT,5)
      @ 55,04 SAY "WEIGHT : " + PADL(INT(INVHDR.WEIGHT),4)
      *B804337,1 BWA 07/29/2001 [END]

      LOOP
    ENDIF
  ENDIF
  SET MEMOWIDTH TO lnOldMemW

  *B804350,1 BWA 08/07/2001 Reposition fields on custom invoice.[START]
  *IF !EMPTY (INVHDR.PHONE)
  * @ 50,7 SAY INVHDR.PHONE  PICTURE "@R "+ gfPhoneTem()  SIZE 1,16
  *ENDIF
  *B804350,1 BWA 08/07/2001 [END]

  SELECT INVHDR
  WKAMT = FREIGHT + INSUR + COD

  IF gcDevice='PRINTER'
    IF !EMPTY(INVHDR.CFACCODE)
    
      *C102539,1 Add case of factor = "HSBC". [Begin]
      *@ 52,18 SAY CHR(27)+CHR(15) +IIF(INVHDR.CFACCODE = "NBANK",;
      *               "THIS ACCOUNT HAS BEEN ASSIGNED TO AND OWNED BY IN PAR              ",;
      *               "PAYABLE TO HELLER FINANCIAL, INC., P.O.BOX 7247-8292, PHILADELPHIA,")
      IF InvHdr.cFacCode = "HSBC"
        *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
        *@ 51,18 SAY CHR(27)+CHR(15) + "This Receivable is assigned to, owned by and payable only to:      "
        *@ 52,31 SAY "HSBC Business Credit (USA) Inc. PO Box 7777-W8720,                     "
        @ 51,18 SAY CHR(27)+CHR(15) + "THIS INVOICE IS ASSIGNED TO, OWNED BY, AND PAYABLE ONLY TO:"
        @ 52,31 SAY                   "THE CIT GROUP/COMMERCIAL SERVICES, PO BOX 1036, CHARLOTTE, "
        *B123728,1 ASH 07/22/2004 (End)
      ELSE
        @ 52,18 SAY CHR(27)+CHR(15) +IIF(INVHDR.CFACCODE = "NBANK",;
                       "THIS ACCOUNT HAS BEEN ASSIGNED TO AND OWNED BY IN PAR              ",;
                       "PAYABLE TO HELLER FINANCIAL, INC., P.O.BOX 7247-8292, PHILADELPHIA,")
      ENDIF
      *C102539,1 Add case of factor = "HSBC". [End]
                     
    ENDIF                  
    IF EMPTY(INVHDR.CFACCODE)
      *B803869,1 AAN Shift SALE AMT label and it's value 5 spaces to the left[Start].
      *@ 52,70  SAY CHR(27)+CHR(15) +'SALE AMT'
      *@ 52,90 SAY lnSubTotal  PICTURE '99999.99'  

      *B804056,1 BWA 05/08/2001 Adj. some fields in the invoice.[START]
      *@ 52,65  SAY CHR(27)+CHR(15) +'SALE AMT'
      *@ 52,85 SAY lnSubTotal  PICTURE '99999.99'

      @ 52,65 SAY 'SALE AMT'
      @ 52,74 SAY lnSubTotal  PICTURE '99999.99'  
      *B804056,1 [END]

      *B803869,1 AAN Shift SALE AMT label and it's value 5 spaces to the left[End].      
    ELSE
      *B803869,1 AAN Shift SALE AMT label and it's value 5 spaces to the left[Start].
      *@ 52,104 SAY 'SALE AMT'
      *@ 52,117 SAY lnSubTotal  PICTURE '99999.99'  

      *B804056,1 BWA 05/08/2001 Adj. some fields in the invoice.[START]
      *@ 52,98 SAY 'SALE AMT'
      *@ 52,111 SAY lnSubTotal  PICTURE '99999.99'  

      *C102539,1 Change the Sales Amount position. [Begin] 
      *@ 52,100 SAY 'SALE AMT'
      *@ 52,120 SAY lnSubTotal  PICTURE '99999.99'
      IF InvHdr.cFacCode = "HSBC"
        @ 52,112 SAY 'SALE AMT'
        @ 52,131 SAY lnSubTotal  PICTURE '99999.99'
      ELSE
        @ 52,100 SAY 'SALE AMT'
        @ 52,120 SAY lnSubTotal  PICTURE '99999.99'      
      ENDIF  
      *C102539,1 Change the Sales Amount position. [End] 
      
      *B804056,1 [END]

      *B803869,1 AAN Shift SALE AMT label and it's value 5 spaces to the left[End].
    ENDIF

    *B804056,1 BWA 05/08/2001 Adj. some fields in the invoice.[START]
    *@ 53,07 SAY "CARTONS:    " + PADR(INVHDR.CARTONS,5)
    IF EMPTY(INVHDR.CFACCODE)
    
    *B804350,1 BWA 08/07/2001 Reposition fields on custom invoice.[START]
     * @ 53,03 SAY "CARTONS:" + PADR(INVHDR.CARTONS,4)
      @ 53,04 SAY "CARTONS:" + PADR(INVHDR.CARTONS,4)
    *B804350,1 BWA 08/07/2001 [END]

    ELSE
      @ 53,06 SAY "CARTONS: " + PADR(INVHDR.CARTONS,5)      
    ENDIF
    *B804056,1 [END]

    IF !EMPTY(INVHDR.CFACCODE)

      *C102539,1 Add case of factor = "HSBC". [Begin]
      *@ 53,30 SAY IIF(INVHDR.CFACCODE = "NBANK",;
      *            "FUNDS ONLY TO: NATIONSBANC COMMERCIAL CORP.                       ",;
      *            "PA  19170-8292, TO WHOM THIS INVOICE IS ASSIGNED. REMITTANCE IS TO")
      IF InvHdr.cFacCode = "HSBC"
        *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
        *@ 53,31 SAY "Philadelphia, PA 19175 OR Dept.49941, Los Angles, CA 90088 Whichever is"
        @ 53,31 SAY  "NC 28201-1036, TO WHOM NOTICE MUST BE GIVEN OF ANY MERCHANDISE"
        *B123728,1 ASH 07/22/2004 (End)
      ELSE
        @ 53,31 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                    "FUNDS ONLY TO: NATIONSBANC COMMERCIAL CORP.                       ",;
                    "PA  19170-8292, TO WHOM THIS INVOICE IS ASSIGNED. REMITTANCE IS TO")
      ENDIF
      *C102539,1 Add case of factor = "HSBC". [End]
                        
    ENDIF

    *B804056,1 BWA 05/08/2001 Adj. some fields in the invoice.[START]
    *@ 53,105 SAY INVHDR.SHIP PICTURE "999999"
    IF EMPTY(INVHDR.CFACCODE)
      @ 53,55 SAY INVHDR.SHIP PICTURE "999999"
    ELSE
*      @ 53,70  SAY INVHDR.SHIP PICTURE "999999"

      @ 53,100  SAY INVHDR.SHIP PICTURE "999999"
    ENDIF
    *B804056,1 [END]

    *B804056,1 BWA 05/08/2001 Adj. some fields in the invoice.[START]
    *IF WKAMT <> 0
    *  @ 53,118 SAY 'FREIGHT'
    *  @ 53,130 SAY WKAMT       PICTURE '99999.99'  
    *ENDIF
    *IF DISCOUNT<>0
    *  @ 53,106 SAY 'DISCOUNT'
    *  @ 53,117 SAY DISCOUNT    PICTURE '99999.99'  
    *ENDIF

    IF EMPTY(INVHDR.CFACCODE)
      IF WKAMT <> 0
        @ 53,65 SAY 'FREIGHT'
        @ 53,74 SAY WKAMT       PICTURE '99999.99'  
      ENDIF
      IF DISCOUNT<>0
        @ 54,65 SAY 'DISCOUNT'
        @ 54,74 SAY DISCOUNT    PICTURE '99999.99'  
      ENDIF
    ENDIF

    IF !EMPTY(INVHDR.CFACCODE)
      IF WKAMT <> 0
        @ 53,112 SAY 'FREIGHT'
        @ 53,131 SAY WKAMT       PICTURE '99999.99'  
      ENDIF
    ENDIF
    *B804056,1 [END]

    IF !EMPTY(INVHDR.CFACCODE)

      *C102539,1 Add case of factor = "HSBC". [Begin]
      *@ 54,30 SAY IIF(INVHDR.CFACCODE = "NBANK",;
      *                "P.O.BOX 105657, ATLANTA, GA 30348-5657                     ",;
      *                "BE MADE ONLY TO THEM. ANY OBJECTIONS TO THIS INVOICE OR ITS")
      IF InvHdr.cFacCode = "HSBC"      
        *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
        *@ 54,31 SAY "nearer. Any objection to this invoice must be reported to"      
        @ 54,31 SAY  "RETURNS OR CLAIMS. PAYMENT MADE TO ANY OTHER PARTY DOES NOT "
        *B123728,1 ASH 07/22/2004 (End)
      ELSE
        @ 54,31 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                        "P.O.BOX 105657, ATLANTA, GA 30348-5657                     ",;
                        "BE MADE ONLY TO THEM. ANY OBJECTIONS TO THIS INVOICE OR ITS")
      ENDIF                  
      *C102539,1 Add case of factor = "HSBC". [End]

    ENDIF

    IF !EMPTY(INVHDR.CFACCODE)
      IF DISCOUNT<>0
        @ 54,112 SAY 'DISCOUNT'
        @ 54,131 SAY DISCOUNT    PICTURE '99999.99'  
      ENDIF
    ENDIF

    *B804056,1 BWA 05/08/2001 Adj. some fields in the invoice.[START]
    *@ 55,07 SAY "WEIGHT :    " + PADR(INVHDR.WEIGHT,5)
    IF EMPTY(INVHDR.CFACCODE)

      *B804337,1 BWA 07/29/2001 Remove decimals from  weight.[START]
      *@ 55,01 SAY "WEIGHT :" + PADR(INVHDR.WEIGHT,5)    
      @ 55,04 SAY "WEIGHT:" + PADR(INT(INVHDR.WEIGHT),4)
    ELSE
      *@ 55,07 SAY "WEIGHT :    " + PADR(INVHDR.WEIGHT,5)
      @ 55,06 SAY "WEIGHT:" + PADR(INT(INVHDR.WEIGHT),4)
      *B804337,1 BWA 07/29/2001 [END]

    ENDIF
    *B804056,1 [END]

    IF !EMPTY(INVHDR.CFACCODE)

      *C102539,1 Add case of factor = "HSBC". [Begin]
      *@ 55,30 SAY IIF(INVHDR.CFACCODE = "NBANK",;
      *                "TO WHOM NOTICE MUST BE GIVEN OF ANY MERCHANDISE RETURN  ",;
      *                "TERMS MUST BE REPORTED WITHIN 10 DAYS AFTER ITS RECEIPT.")
      IF InvHdr.cFacCode = "HSBC"      
        *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
        *@ 55,31 SAY "HSBC, Business Credit (USA) Inc. at 452 Fifth Avenue 4th FL."
        @ 55,31 SAY "CONSTITUTE VALID PAYMENT OF THIS INVOICE."
        *B123728,1 ASH 07/22/2004 (End)
      ELSE
        @ 55,31 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                        "TO WHOM NOTICE MUST BE GIVEN OF ANY MERCHANDISE RETURN  ",;
                        "TERMS MUST BE REPORTED WITHIN 10 DAYS AFTER ITS RECEIPT.")
      ENDIF                  
      *C102539,1 Add case of factor = "HSBC". [End]
    ENDIF

    *B804056,1 BWA 05/08/2001 Adj. some fields in the invoice.[START]
    *@ 55,130 SAY INVHDR.TOTALCHG  PICTURE '999999.99'
    IF EMPTY(INVHDR.CFACCODE)
      @ 55,73 SAY INVHDR.TOTALCHG  PICTURE '999999.99'
    ELSE
      @ 55,130 SAY INVHDR.TOTALCHG  PICTURE '999999.99'
    ENDIF
    *B804056,1 [END]

    IF !EMPTY(INVHDR.CFACCODE)

      *C102539,1 Add case of factor = "HSBC". [Begin]
      *@ 56,30 SAY IIF(INVHDR.CFACCODE = "NBANK",;
      *            "OR CLAIMS FOR SHORTAGE, NON DELIVERY OR FOR OTHER GROUNDS.",;
      *            "                                                           ")
      IF InvHdr.cFacCode = "HSBC"
        *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
        *@ 56,31 SAY "New York, NY 10018 , RF 0009(rev.3/2000)"
        *B123728,1 ASH 07/22/2004 (End)
      ELSE
        @ 56,31 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                    "OR CLAIMS FOR SHORTAGE, NON DELIVERY OR FOR OTHER GROUNDS.",;
                    "                                                           ")
      ENDIF                  
      *C102539,1 Add case of factor = "HSBC". [End]

    ENDIF 
    
      *C102539,1 Add case of factor = "HSBC". [Begin]
      *@ 57,1 SAY CHR(18)+CHR(18) 
      IF InvHdr.cFacCode = "HSBC"
        @ 57,1 SAY CHR(18)+CHR(18) 
      ELSE
        @ 57,1 SAY CHR(18)+CHR(18) 
      ENDIF                  
      *C102539,1 Add case of factor = "HSBC". [End]

  ELSE

    *B803869,1 AAN Shift SALE AMT label and it's value 5 spaces to the left[Start].
    *@ 51,66 SAY 'SALE AMT'
    *@ 51,75 SAY lnSubTotal  PICTURE '99999.99'  
    @ 51,61 SAY 'SALE AMT'
    @ 51,70 SAY lnSubTotal  PICTURE '99999.99'  
    *B803869,1 AAN Shift SALE AMT label and it's value 5 spaces to the left[End].    

    *C102539,1 Add case of factor = "HSBC". [Begin]
    *@ 52,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
    *                "THIS ACCOUNT HAS BEEN ASSIGNED TO AND OWNED BY IN PAR",;
    *                "PAYABLE TO HELLER FINANCIAL, INC., P.O.BOX 7247-8292, PHILADELPHIA,")
    IF InvHdr.cFacCode = "HSBC"
      *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
      *@ 52,18 SAY "This Receivable is assigned to, owned by and payable only to:"
      @ 52,18 SAY  "THIS INVOICE IS ASSIGNED TO, OWNED BY, AND PAYABLE ONLY TO: "
      *B123728,1 ASH 07/22/2004 (End)
    ELSE
      @ 52,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                     "THIS ACCOUNT HAS BEEN ASSIGNED TO AND OWNED BY IN PAR              ",;
                     "PAYABLE TO HELLER FINANCIAL, INC., P.O.BOX 7247-8292, PHILADELPHIA,")
    ENDIF
    *C102539,1 Add case of factor = "HSBC". [End]

    IF WKAMT <> 0
      @ 52,86 SAY 'FREIGHT'
      @ 52,95 SAY WKAMT       PICTURE '99999.99'  
    ENDIF

    *B804350,1 BWA 08/07/2001 Reposition fields on custom invoice.[START]
    *@ 53,03 SAY "CARTONS:    " + PADR(INVHDR.CARTONS,5)
    @ 53,04 SAY "CARTONS: " + PADR(INVHDR.CARTONS,5)
    *B804350,1 BWA 08/07/2001 [END]

    *C102539,1 Add case of factor = "HSBC". [Begin]
    *@ 53,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
    *            "FUNDS ONLY TO: NATIONSBANC COMMERCIAL CORP.",;
    *            "PA  19170-8292, TO WHOM THIS INVOICE IS ASSIGNED. REMITTANCE IS TO")
    IF InvHdr.cFacCode = "HSBC"
      *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
      *@ 53,18 SAY "HSBC Business Credit (USA) Inc. PO Box 7777-W8720,"
      @ 53,18 SAY  "THE CIT GROUP/COMMERCIAL SERVICES, PO BOX 1036, CHARLOTTE,"
      *B123728,1 ASH 07/22/2004 (End)
    ELSE
      @ 53,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                  "FUNDS ONLY TO: NATIONSBANC COMMERCIAL CORP.",;
                  "PA  19170-8292, TO WHOM THIS INVOICE IS ASSIGNED. REMITTANCE IS TO")
    ENDIF
    *C102539,1 Add case of factor = "HSBC". [End]

    IF DISCOUNT<>0
      @ 53,86 SAY 'DISCOUNT'
      @ 53,95 SAY DISCOUNT    PICTURE '99999.99'  
    ENDIF

    *C102539,1 Add case of factor = "HSBC". [Begin]
    *@ 54,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
    *                "P.O.BOX 105657, ATLANTA, GA 30348-5657",;
    *                "BE MADE ONLY TO THEM. ANY OBJECTIONS TO THIS INVOICE OR ITS")
    IF InvHdr.cFacCode = "HSBC"
      *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
      *@ 54,18 SAY "Philadelphia, PA 19175 OR Dept.49941, Los Angles, CA 90088 Whichever is"
      @ 54,18 SAY  "NC 28201-1036, TO WHOM NOTICE MUST BE GIVEN OF ANY MERCHANDISE"
      *B123728,1 ASH 07/22/2004 (End)
    ELSE
      @ 54,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                      "P.O.BOX 105657, ATLANTA, GA 30348-5657",;
                      "BE MADE ONLY TO THEM. ANY OBJECTIONS TO THIS INVOICE OR ITS")
    ENDIF
    *C102539,1 Add case of factor = "HSBC". [End]

    *B804337,1 BWA 07/29/2001 Remove decimals from  weight.[START]
    *@ 55,03 SAY "WEIGHT : " + PADR(INVHDR.WEIGHT,5)
    @ 55,04 SAY "WEIGHT:" + PADR(INT(INVHDR.WEIGHT),4)
    *B804337,1 BWA 07/29/2001 [END]

    *C102539,1 Add case of factor = "HSBC". [Begin]
    *@ 55,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
    *                "TO WHOM NOTICE MUST BE GIVEN OF ANY MERCHANDISE RETURN",;
    *                "TERMS MUST BE REPORTED WITHIN 10 DAYS AFTER ITS RECEIPT.")
    IF InvHdr.cFacCode = "HSBC"
      *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
      *@ 55,18 SAY "nearer. Any objection to this invoice must be reported to"
      @ 55,18 SAY  "RETURNS OR CLAIMS. PAYMENT MADE TO ANY OTHER PARTY DOES NOT"
      *B123728,1 ASH 07/22/2004 (End)
    ELSE
      @ 55,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                      "TO WHOM NOTICE MUST BE GIVEN OF ANY MERCHANDISE RETURN",;
                      "TERMS MUST BE REPORTED WITHIN 10 DAYS AFTER ITS RECEIPT.")
    ENDIF
    *C102539,1 Add case of factor = "HSBC". [End]


    @ 55,94 SAY INVHDR.TOTALCHG  PICTURE '999999.99'

    *C102539,1 Add case of factor = "HSBC". [Begin]
    *@ 56,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
    *                "OR CLAIMS FOR SHORTAGE, NON DELIVERY OR FOR OTHER GROUNDS.","")
    IF InvHdr.cFacCode = "HSBC"
      *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
      *@ 56,18 SAY "HSBC, Business Credit (USA) Inc. at 452 Fifth Avenue 4th FL."
      @ 56,18 SAY "CONSTITUTE VALID PAYMENT OF THIS INVOICE."
      *B123728,1 ASH 07/22/2004 (End)
    ELSE
      @ 56,18 SAY IIF(INVHDR.CFACCODE = "NBANK",;
                      "OR CLAIMS FOR SHORTAGE, NON DELIVERY OR FOR OTHER GROUNDS.","")
    ENDIF
    *C102539,1 Add case of factor = "HSBC". [End]

    @ 56,78 SAY INVHDR.SHIP PICTURE "999999"

    *C102539,1 Add case of factor = "HSBC". [Begin]
    IF InvHdr.cFacCode = "HSBC"      
      *B123728,1 ASH 07/22/2004 (Begin) Change the special instructions text.
      *@ 57,18 SAY "New York, NY 10018 , RF 0009(rev.3/2000)"
      *B123728,1 ASH 07/22/2004 (End)
    ENDIF                  
    *C102539,1 Add case of factor = "HSBC". [End]
    
  ENDIF

  *B804056,1 BWA 05/08/2001 correct the flag of the print.[START]
  *SELECT INVHDR
  *SEEK lcInvoice
  *REPLACE PRTFLAG WITH 'P'

  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
  *B804056,1 [END]

  SELECT (INVHTEMP)
  SKIP
  llNewDec = .T.
ENDDO
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfGetZone
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/23/1999 
*! Purpose   : Get the zone to be printed in the invoice format
*!*************************************************************
*! Calls     : ........
*!*************************************************************
*! Returns   : ........
*!*************************************************************
*! Example   :  =lfGetZone()
*!*************************************************************
*
FUNCTION lfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip
PRIVATE lnOldWrk
  lnOldWrk = SELECT(0)
IF !USED('SYSZONES')
  SELECT 0
  =gfOpenFile(gcSysHome+'SYSZONES',gcSysHome+'Frtzones','SH')
ENDIF 

IF SEEK(lcUpsType+lcUpsFrom+lcToZip,'SYSZONES')
  lcZones = SYSZONES.ZONE
ELSE
 lcZones = ''
ENDIF
SELECT (lnOldWrk)

RETURN lcZones

*-- END OF lfGetZone.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : ABDOU ELGENDI - (ABD) 
*! Date      : 09/23/1999 
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfClearRep()
*!*************************************************************
*
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.

*--- Erase the temp file
USE IN IIF(USED(INVHTEMP),INVHTEMP,0)
ERASE &gcWorkDir.&INVHTEMP+'.DBF'

*-- END OF lfClearRep.

*!*************************************************************
