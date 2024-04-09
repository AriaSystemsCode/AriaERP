*:***************************************************************************
*: Program file  : ARPINVTR.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR TRIO KNITTINGS INC.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO ?)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 8 1/2"
*! Date          : 06/28/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102375
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVTR
*:***************************************************************************

           *--Section of Initializing the variables --*
llNoRec = .F.      && Variable used to make the endreport function in the main program.
NEWDOC  = .T.
ROW     = 0
XCOUNT  = 0
TOPROW  = 0

               *--Section to get the FromZone --*
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED(lcWarHous1)
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous1, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous1 ),&lcWarHous1..UPS,lcFromZone)
ENDIF
IF !USED(lcSysZone)
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'
                  *--End Section to get the FromZone --*

*--THE COLOR LENGTH
STORE 0 TO lnClrLnTr , lnClrPosTr
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnTr  = LEN(laItemSeg[lnCount,3])
    lnClrPosTr = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenthTr
lnLenthTr = LEN(gfItemMask('PM'))

           *--Section of getting the address from the main program --*
HLINE1  = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1]) TO HLINE2
STORE TRIM(laCompAdd[2]) TO HLINE3
STORE TRIM(laCompAdd[3]) TO HLINE4
IF EMPTY(HLINE3)
  STORE HLINE4 TO HLINE3
  STORE ''     TO HLINE4
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
    STORE 0 TO XPAGENO
    NEWDOC   = .F.
    XACCOUNT = ACCOUNT
    XSTORE   = STORE
    XREP     = REP1

    *-- GET THE BILL TO AND SHIP ADDRESS
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

    *--Get the Descreption about the codes.
    SELECT CODES
    SET ORDER TO CODES IN CODES
    PTERMS   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
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
  STORE SPACE(1) TO lcScale

  DO WHILE CURLOOP = '1'
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= TOPROW + 39
      IF ROW >= TOPROW + 39
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

    XSTYLE = LEFT(INVLINE.STYLE,lnLenthTr)
    SELECT INVLINE
    XTOTQTY = 0                 && ADD UP ALL COLORS TO BE PRINTED ON ONE LINE
    XPRICE  = PRICE             && DEFAULTING TO THE PRICE OF FIRST COLOR. AND
                                && ASSUMING THAT IT IS SAME FOR ALL COLORS.
     IF !llPrnSize
       SCAN WHILE (XSTYLE = LEFT(INVLINE.STYLE,lnLenthTr)) .AND. (INVOICE=XINVOICE)
         XTOTQTY=XTOTQTY+TOTQTY
       ENDSCAN
     ENDIF

     IF llPrnSize AND Scale <> lcScale AND ;
       SEEK("S"+Scale,"SCALE")
       STORE Scale TO lcScale
       STORE 0 TO lnCol
       FOR lnCount = 1 TO 5
         lcCount = STR(lnCount,1)
         @ ROW,36+lnCol SAY PADL(ALLTRIM(SCALE.SZ&lcCount),4," ")
         lncol = lnCol + 5
       ENDFOR
       ROW =ROW + 1
     ENDIF

     @ ROW,09 SAY IIF(llPrnSize,TOTQTY,XTOTQTY) PICTURE '999999'
     @ ROW,16 SAY IIF(llPrnSize , SUBSTR(INVLINE.STYLE,lnClrPosTr,lnClrLnTr) , "")
     @ ROW,28 SAY SUBSTR(XSTYLE,1,8)

     lnCol = 0
     IF llPrnSize
       FOR lnCount = 1 TO 5
         lcCount = STR(lnCount,1)
         @ ROW,36+lnCol SAY Qty&lcCount PICTURE '@Z 9999'
         lncol = lnCol + 5
       ENDFOR
     ENDIF

     *-- SUM INVOICE TOTALS
     XLINETOT  = IIF(llPrnSize, PRICE * TOTQTY , XPRICE * XTOTQTY)
     XPIECES   = XPIECES + IIF(llPrnSize,TOTQTY,XTOTQTY)
     XSUBTOTAL = XSUBTOTAL+XLINETOT
     @ ROW,62  SAY IIF(llPrnSize,PRICE,XPRICE)   PICTURE '9999.99'
     @ ROW,77  SAY XLINETOT  PICTURE '999999.99'
     ROW = ROW+1

     *-- GET NEXT LINE ITEM      && LOOK AT THE SCAN ABOVE. WE SHOULD BE POINTING
     SELECT INVLINE              && AT THE NEXT LINE.
     IF llPrnSize
       SKIP
     ENDIF
  ENDDO
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')

  IF ENDPAGE = '1'
    =lfGetcont()
     LOOP
  ENDIF

  SELECT INVHDR
  WKAMT = FREIGHT + INSUR + COD
  XTRADE_DIS = 0                       && TO BE MODIFIED AFTER CHECKING WITH THEM.
  ROW = TOPROW + 42
  @ ROW,77 SAY XSUBTOTAL                PICTURE '999999.99'
  ROW = ROW+2
  @ ROW,77 SAY XTRADE_DIS               PICTURE '999999.99'
  ROW=ROW+2
  @ ROW,77 SAY WKAMT                    PICTURE '999999.99'
  ROW=ROW+2
  @ ROW,77 SAY INVHDR.TOTALCHG          PICTURE '999999.99'

  ROW = TOPROW + 51

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
*! Name      : gfGetZone
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/28/2001
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINTR.PRG
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
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/28/2001
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
ROW = TOPROW + 43
@ ROW,77 SAY '******.**'
ROW = TOPROW + 51

*--End of lfGetcont.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/28/2001
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

SELECT INVHDR
IF XCOUNT = 22
  ROW     = 0
  XCOUNT  = 0
  TOPROW  = ROW
ELSE
  XCOUNT  = XCOUNT + 1
  TOPROW  = ROW
ENDIF

XPAGENO = XPAGENO+1
ROW = TOPROW + 3

IF llPrntComp
  @ ROW,05 SAY HLINE1
ENDIF
@ ROW,76 SAY XINVOICE

* LINE 4
ROW = TOPROW + 4
IF llPrntComp
  @ ROW,05 SAY HLINE2
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,50 SAY 'R E M I T   T O:'
ENDIF
@ ROW,70 SAY 'PAGE : '
@ ROW,77 SAY XPAGENO PICTURE '9999'

* LINE 5
ROW = TOPROW + 5
IF llPrntComp
  @ ROW,05 SAY HLINE3
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,50 SAY XFNAME
ENDIF

* LINE 6
ROW = TOPROW + 6
IF llPrntComp
  @ ROW,05 SAY HLINE4
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,50 SAY XFADDR1
ENDIF

* LINE 7
ROW = TOPROW + 7
IF llPrntComp
  @ ROW,05 SAY IIF(EMPTY(lcCompPhon) , "" , TRANSFORM(lcCompPhon , lcPhonPict))
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,50 SAY XFADDR2
ENDIF

* LINE 8
ROW = TOPROW + 8
IF  !EMPTY(lcDunsNo)
  @ ROW,05 SAY 'DUNNS NUMBER -->  '+ ALLTRIM(lcDunsNo)
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,50 SAY XFADDR3
ENDIF

ROW = TOPROW + 14
@ ROW,13 SAY XBTNAME
IF CURLOOP = '1'
  @ ROW,53 SAY XSTNAME
ENDIF

ROW = TOPROW + 15
@ ROW,13 SAY XBTADDR1
IF CURLOOP = '1'
  @ ROW,53 SAY XSTADDR1
ENDIF

ROW = TOPROW + 16
@ ROW,13 SAY XBTADDR2
IF CURLOOP = '1'
  @ ROW,53 SAY XSTADDR2
ENDIF

ROW = TOPROW + 17
@ ROW,13 SAY XBTADDR3
IF CURLOOP = '1'
  @ ROW,53 SAY XSTADDR3
ENDIF

ROW = TOPROW + 21
@ ROW,01 SAY ACCOUNT
@ ROW,11 SAY XREP
@ ROW,18 SAY LEFT(CUSTPO,10)
@ ROW,29 SAY LEFT(PSHIPVIA,15)
@ ROW,50 SAY DEPT
@ ROW,59 SAY LEFT(PTERMS,14)
@ ROW,75 SAY INVDATE

ROW = TOPROW + 24
STORE SPACE(1) TO lcScale

*--End of lfGetInHdr.