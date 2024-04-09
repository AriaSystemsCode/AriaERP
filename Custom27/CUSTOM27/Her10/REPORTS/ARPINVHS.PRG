*:***************************************************************************
*: Program file  : ARPINVHS.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR HER STYLE.
*: Date          : 09/15/2002
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102708
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVHS
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*: C102734,1 ADEL 10/13/02 Made needed modifications.
*: B606585,1 RAE  10/30/2002 Move the SHIP TO and the SOLD TO lines 2 lines up.
*: B121267,1 NNA  02/08/2004 Fix bug of that the invoice printing in 2 pages and in the second
*: B121267,1 NNA			 page it prints only one line of total amount column
*:***************************************************************************

           *--Section of Opening the files doesn't found in the OG --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
lnNotLine  = 1        && For the notepad
llNoRec    = .F.      && Variable used to make the endreport function in the main program.
XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)
XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
NEWDOC     = .T.
MAXROW     = 47

                  *--Section to get the style and color length --*
*--The Color length
DECLARE laItemSeg[1]
STORE 0 TO lcClrLen,lnClrPos
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lcClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

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

           *--Section of getting the address from the main program --*
HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
HLINE2 = TRIM(laCompAdd[1])
HLINE3 = TRIM(laCompAdd[2])
HLINE4 = TRIM(laCompAdd[3])
HLINE5 = IIF(EMPTY(lcCompPhon) , "" , TRANSFORM(lcCompPhon , lcPhonPict))
IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF

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
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  XINVOICE = INVOICE

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF NEWDOC
    STORE 0.00 TO XPIECES, XSUBTOTAL
    NEWDOC    = .F.
    XORDER    = ORDER
    XPHONE    = TRANSFORM(lcCompPhon , lcPhonPict)
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
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
    IF LEN(TRIM(laShipTo[2])) = 0
      XSTADDR2 = laShipTo[3]
      XSTADDR3 = ''
    ENDIF

    *--Get the Descreption about the codes.
    SELECT CODES
    SET ORDER TO CODES IN CODES
    PTERMS   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST = gfCodDes(INVHDR.SPCINST,'SPCINST')
    lcDvsion = gfCodDes(INVHDR.CDIVISION,'CDIVISION')
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
    XFNAME  = lcFacName           && Factor company name.
    XFADDR1 = laFactor[1]         && Factor address#1.
    XFADDR2 = laFactor[2]
    XFADDR3 = laFactor[3]
    IF LEN(TRIM(laFactor[2])) =0  && If address#2 is empty fill it with address#3.
      XFADDR2 = laFactor[3]
      XFADDR3 = ''
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
  ROW    = 22

  DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
      IF ROW >= MAXROW
        IF INVOICE <> XINVOICE
          EXIT
        ENDIF
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
    @ ROW,00 SAY SUBSTR(STYLE,1,lnMajor)
    @ ROW,13 SAY SUBSTR(STYLE,lnClrPos,lcClrLen)
    *C102734,1 (Begin) Print sizes Qtys
    *@ ROW,21 SAY STYLE.DESC
    *@ ROW,58 SAY TOTQTY PICTURE '9999'    
    @ ROW,22 SAY LEFT(STYLE.SCALE,1)
    @ ROW,23 SAY QTY1   PICTURE '@Z 9999'
    @ ROW,27 SAY QTY2   PICTURE '@Z 9999'
    @ ROW,31 SAY QTY3   PICTURE '@Z 9999'
    @ ROW,35 SAY QTY4   PICTURE '@Z 9999'
    @ ROW,39 SAY QTY5   PICTURE '@Z 9999'
    @ ROW,43 SAY QTY6   PICTURE '@Z 9999'
    @ ROW,47 SAY QTY7   PICTURE '@Z 9999'
    @ ROW,51 SAY QTY8   PICTURE '@Z 9999'
    @ ROW,55 SAY TOTQTY PICTURE '99999'
    *C102734,1 (End)

    *--SUM INVOICE TOTALS.
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL + XLINETOT
    *C102734,1 (Begin) Print Price 2 cols to the left.
    *@ ROW,64  SAY PRICE     PICTURE '9999.99'
    @ ROW,62  SAY PRICE     PICTURE '9999.99'
    *C102734,1 (End)
    
    @ ROW,71  SAY XLINETOT  PICTURE '999999.99'
    ROW = ROW + 1

    *C102734,1 (Begin) Print Style desc.
    @ ROW,00 SAY 'STYLE DESC : '+STYLE.Desc
    ROW = ROW + 1
    *C102734,1 (End)
    IF llRpPrtSku
      IF SEEK('S'+INVLINE.ACCOUNT+INVLINE.STYLE,"SPCK_HDR") OR  ;
         SEEK('P'+INVLINE.ACCOUNT+INVLINE.STYLE,"SPCK_HDR")
        @ ROW,00 SAY 'SKU# ' + SPCK_HDR.Pack_id
        ROW = ROW + 1
      ENDIF
    ENDIF

    *--GET NEXT LINE ITEM
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
  *adel
  row = row +1

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
          lnNotLine = lnNotLine - 1
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
  *C102734,1 (Begin) Print Total Merch. 2 lines down not jusrt 1.
  ROW = ROW + 1
  *C102734,1 (End)
  
  @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
  *C102734,1 (Begin) Print Total 2 cols left.
  *@ ROW,56 SAY XPIECES   PICTURE '999999'
  *C102734,1 (End)
  @ ROW,54 SAY XPIECES   PICTURE '999999'
  
  @ ROW,70 SAY XSUBTOTAL PICTURE '9999999.99'
  ROW = ROW + 1

  IF DISCOUNT<>0
    @ ROW,12 SAY 'TOTAL - D I S C O U N T'
    @ ROW,69 SAY DISCOUNT PICTURE '99999999.99'
    ROW = ROW + 1
  ENDIF

   *--Print the tax rate and tax amount .
   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
     IF INVHDR.TAX_RATE <> 0          && Condition Sales tax <> 0
       @ ROW,12 SAY XTAX_DESC
       @ Row,33 SAY lcTaxRefr
       @ ROW,64 SAY XSTRING_RATE + '%'
       @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
     ENDIF
     ROW = ROW + 1
     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF

     IF UPPER(ALLTRIM(gcContCode)) = 'CANADA' AND InvHdr.nHSTAmt <> 0
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF
   ENDIF

   IF FREIGHT + INSUR + COD <> 0
     @ ROW,12 SAY 'TOTAL - F R E I G H T'
     @ ROW,70 SAY FREIGHT + INSUR + COD  PICTURE '9999999.99'
     ROW = ROW + 1
   ENDIF

   *** Print the tax rate and tax amount.
   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
     IF INVHDR.TAX_RATE <> 0        && Condition Sales tax <> 0
       @ ROW,12 SAY XTAX_DESC
       @ Row,33 SAY lcTaxRefr
       @ ROW,64 SAY XSTRING_RATE + '%'
       @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
     ENDIF
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF

     IF UPPER(ALLTRIM(gcContCode)) = 'CANADA' AND InvHdr.nHSTAmt <> 0
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF
   ENDIF
      *B121267,1 NNA 02/08/2004 (Begin) Correct the Row No. to Print from (Row=54) if There is notes
	  IF EMPTY(ALLTRIM(XNOTE1+XNOTE2))
	  *B121267,1 NNA  (End)
        *@ 52,18 SAY XNOTE1 + ' ' + XNOTE2
        IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
          *C102734,1 (Begin) Print Mesg lines donw.
          *@ 53,10 SAY lcRpMsg1
          *@ 54,10 SAY lcRpMsg2
          @ 54,10 SAY lcRpMsg1
          @ 55,10 SAY lcRpMsg2
          *C102734,1 (End)
        ENDIF
        *C102734,1 (Begin) Print customer phone.
        @ 56,01 SAY CUSTOMER.PHONE1
        *C102734,1 (End)
      *B121267,1 NNA 02/08/2004 (Begin) Correct the Row No. to Print from (Row=54) if There is notes
      ELSE
        @ 54,18 SAY XNOTE1 + ' ' + XNOTE2
        IF LEN(TRIM(lcRpMsg1)) >0
          @ 55,10 SAY lcRpMsg1
        ENDIF 
        @ 56,01 SAY CUSTOMER.PHONE1
        IF LEN(TRIM(lcRpMsg2)) >0
          @ 56,10 SAY lcRpMsg2
        ENDIF
	  ENDIF  
	  *B121267,1 NNA  (End)
      IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
        *C102734,1 (Begin) Print approval one line down
        *@ 56,56 SAY 'APPROVAL: ' + INVHDR.APPROVAL
        @ 57,56 SAY 'APPROVAL: ' + INVHDR.APPROVAL
        *C102734,1 (End)
      ENDIF
      @ 58,02 SAY INVHDR.PIKTKT
      @ 58,09 SAY INVHDR.SHIPDATE
      @ 58,19 SAY CARTONS PICTURE '@Z 999'
      @ 58,25 SAY WEIGHT  PICTURE '@Z 99999'
      @ 58,35 SAY INVHDR.INVOICE
      @ 58,70 SAY INVHDR.TOTALCHG PICTURE '9999999.99'
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

ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT ERNEST (BWA) 
*! Date      : 09/15/2002
*! Purpose   : PRINT THE FOOTER OF THE INVOICE.
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

@ 52,18 SAY XNOTE1 + ' ' + INVHDR.NOTE2
IF LEN(TRIM(lcRpMsg1 + lcRpMsg2)) >0
  *C102734,1 (Begin) Print Mesg lines donw.
  *@ 53,10 SAY lcRpMsg1
  *@ 54,10 SAY lcRpMsg2
  @ 54,10 SAY lcRpMsg1
  @ 55,10 SAY lcRpMsg2
  *C102734,1 (End)
ENDIF
*C102734,1 (Begin) Print Mesg lines donw.
@ 56,01 SAY CUSTOMER.PHONE1
*C102734,1 (End)
@ 58,02 SAY INVHDR.PIKTKT
@ 58,09 SAY INVHDR.SHIPDATE
@ 58,19 SAY '***'
@ 58,25 SAY '*****'
@ 58,35 SAY XINVOICE
@ 58,72 SAY '******.**'

*--End of lfGetcont.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 09/15/2002
*! Purpose   : PRINT THE HEADER OF THE INVOICE.
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

SELECT INVHDR
IF llPrntComp
  @ 00,05 SAY ALLTRIM(HLINE1)
  SELECT INVHDR
ENDIF

*C102734,1 (Begin) Print Invoice date, no and Cust po.
*@ 00,53 SAY XINVOICE
*@ 00,70 SAY INVDATE
@ 00,40 SAY INVDATE
@ 00,50 SAY 'INVOICE: '+XINVOICE
@ 00,68 SAY 'PO: '+LEFT(CUSTPO,11)
*C102734,1 (End)

* LINE 1
IF llPrntComp
  @ 01,05 SAY HLINE2
ENDIF

* LINE 2
IF llPrntComp
  @ 02,05 SAY HLINE3
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 02,50 SAY 'R E M I T   T O:'
ENDIF

* LINE 3
IF llPrntComp
  @ 03,05 SAY HLINE4
ENDIF
IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 03,50 SAY XFNAME
ENDIF

* LINE 4
IF llPrntComp
  @ 04,05 SAY XPHONE SIZE 1,16
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 04,50 SAY XFADDR1
ENDIF

* LINE 5
IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 05,50 SAY XFADDR2
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 06,50 SAY XFADDR3
ENDIF

*B606585,1 RAE Move the SHIP TO and the SOLD TO lines 2 lines up. [start]
*@ 09,10 SAY XBTNAME
*IF CURLOOP = '1'
*  @ 09,47 SAY XSTNAME
*ENDIF

*@ 10,10 SAY XBTADDR1
*IF CURLOOP = '1'
*  @ 10,47 SAY XSTADDR1
*ENDIF

*@ 11,10 SAY XBTADDR2
*IF CURLOOP = '1'
*  @ 11,47 SAY XSTADDR2
*ENDIF

*@ 12,10 SAY XBTADDR3
*IF CURLOOP = '1'
*  @ 12,47 SAY XSTADDR3
*ENDIF

*B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]
*@ 07,10 SAY XBTNAME
@ 08,10 SAY XBTNAME
*B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [End]

IF CURLOOP = '1'
  
  *B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]
  *@ 07,47 SAY XSTNAME
  @ 08,47 SAY XSTNAME
  *B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [End]

ENDIF

*B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]
*@ 08,10 SAY XBTADDR1
@ 09,10 SAY XBTADDR1
*B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]

IF CURLOOP = '1'

  *B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]
  *@ 08,47 SAY XSTADDR1
  @ 09,47 SAY XSTADDR1
  *B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [End]

ENDIF

*B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]
*@ 09,10 SAY XBTADDR2
@ 10,10 SAY XBTADDR2
*B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]

IF CURLOOP = '1'

  *B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]
  *@ 09,47 SAY XSTADDR2
  @ 10,47 SAY XSTADDR2
  *B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [End]

ENDIF

*B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]
*@ 10,10 SAY XBTADDR3
@ 11,10 SAY XBTADDR3
*B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]

IF CURLOOP = '1'

  *B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [Begin]
  *@ 10,47 SAY XSTADDR3
  @ 11,47 SAY XSTADDR3
  *B606585,1 SSE Move the SHIP TO & SOLD TO 1 line down. [End]

ENDIF
*B606585,1 RAE [end]

SELECT INVHDR
@ 15,01 SAY ACCOUNT
@ 15,09 SAY LEFT(CUSTPO,11)
@ 15,21 SAY IIF(CURLOOP = '1', XSTORE ,'')
@ 15,30 SAY DEPT
@ 15,36 SAY ORDER
@ 15,45 SAY LEFT(PTERMS,15)
@ 15,59 SAY REP1
@ 15,63 SAY REP2
@ 15,68 SAY LEFT(PSHIPVIA,14)

*C102734,1 (Begin) Print styles scales.
lcScalStr = SPACE(0)
lcInvNo   = INVHDR.INVOICE
DO lpGtScl WITH lcInvNo
SELECT Scale
Row = 17
FOR lnI = 1 TO MIN(LEN( lcScalStr ),5)
  lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  SEEK 'S' + lcSizeScal
  *--Changed by TAK 11/29/94
  @ Row,22 SAY lcSizeScal
  @ Row,24 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ1),1,3),3,' ')
  @ Row,28 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ2),1,3),3,' ')
  @ Row,32 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ3),1,3),3,' ')
  @ Row,36 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ4),1,3),3,' ')
  @ Row,40 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ5),1,3),3,' ')
  @ Row,44 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ6),1,3),3,' ')
  @ Row,48 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ7),1,3),3,' ')
  @ Row,52 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ8),1,3),3,' ')
  Row = Row + 1
ENDFOR
*C102734,1 (End)

Row = 22          && NEW LINE in the function

*--End of lfGetInHdr.
*!*************************************************************
*! Name      : gfGetZone
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 09/15/2002
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVHS.PRG
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

*-- End of gfGetZone.


*!*************************************************************
*! Name : lpGtScl.
*! Auth : Adel Mohammed El Gazzar (ADEL)
*! Date : 10/13/02
*!*************************************************************
*! Synopsis : Build a string holding the first uniqe 5 scales
*!            from the invoice line scal.
*!*************************************************************
*! Passed :
*!        Parameters : 
*!           lcInvNo : The Current invoice number.
*!*************************************************************
*! Returned : 
*!        Variables  : lcScalStr.
*!*************************************************************
*! C102734

PROCEDURE lpGtScl
PARAMETERS lcInvNo

PRIVATE lnRecNo, lcAlias, lnCount
*-- Save the current alias.
lcAlias = ALIAS()
SELECT InvLine
lcExp   = Invoice + STR(LineNo,6)
lnCount = 1
=SEEK(lcInvNo)
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 5
  IF !( ALLT(Scale) $ lcScalStr )
    lcScalStr = lcScalStr + ALLTR(Scale)
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN
= SEEK(lcExp,"InvLine")
SELECT (lcAlias)
*-- EOF( lpGtScl )
