*:***************************************************************************
*: Program file  : ARPINVSS.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR STUNT SPORTWEAR(1).
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG (REFERE TO ?)
*:                 PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" - 5 SIZE SCALES
*! Date          : 06/17/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102288
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVSS
*:***************************************************************************

           *--Section of Initializing the variables --*
NEWDOC  = .T.
MAXROW  = 47
llNoRec = .F.      && Variable used to make the endreport function in the main program.

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

                  *--Section to get the style and color length --*
*--THE COLOR LENGTH
STORE 0 TO lnClrLnSS , lnClrPosSS
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnSS  = LEN(laItemSeg[lnCount,3])
    lnClrPosSS = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenth
lnLenthSS = LEN(gfItemMask('PM'))
                  *--End Section to get the style and color length --*

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
    XPHONE    = INVHDR.PHONE
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE

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

    *--Get the Descreption about the codes.
    SELECT CODES
    SET ORDER TO CODES IN CODES
    PTERMS   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST = gfCodDes(INVHDR.SPCINST,'SPCINST')
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
   XSCALE =  ' '
   ROW    = 24
   DO WHILE CURLOOP = '1'
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
     @ ROW,00 SAY LEFT(STYLE,lnLenthSS)
     @ ROW,08 SAY SUBSTR(STYLE,lnClrPosSS,lnClrLnSS)
     @ ROW,12 SAY SUBSTR(STYLE.DESC,1,12)
     @ ROW,25 SAY LEFT(STYLE.SCALE,1)
     @ ROW,26 SAY QTY1   PICTURE '@Z 9999'
     @ ROW,30 SAY QTY2   PICTURE '@Z 9999'
     @ ROW,34 SAY QTY3   PICTURE '@Z 9999'
     @ ROW,38 SAY QTY4   PICTURE '@Z 9999'
     @ ROW,42 SAY QTY5   PICTURE '@Z 9999'
     @ ROW,46 SAY QTY6   PICTURE '@Z 9999'
     @ ROW,50 SAY QTY7   PICTURE '@Z 9999'
     @ ROW,54 SAY QTY8   PICTURE '@Z 9999'
     @ ROW,58 SAY TOTQTY PICTURE '99999'

     * SUM INVOICE TOTALS
     XLINETOT   = PRICE * TOTQTY
     XPIECES    = XPIECES + TOTQTY
     XSUBTOTAL  = XSUBTOTAL+XLINETOT

     @ ROW,64  SAY PRICE     PICTURE '9999.99'
     @ ROW,73  SAY XLINETOT  PICTURE '999999.99'

     ROW = ROW+1

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
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
   @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
   @ ROW,57 SAY XPIECES              PICTURE '999999'
   @ ROW,73 SAY XSUBTOTAL            PICTURE '999999.99'
   ROW=ROW+1

   IF DISCOUNT<>0
      @ ROW,12 SAY 'TOTAL - D I S C O U N T'
      @ ROW,73 SAY DISCOUNT             PICTURE '999999.99'
      ROW=ROW+1
   ENDIF

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,12 SAY 'TOTAL - F R E I G H T'
      @ ROW,73 SAY WKAMT                PICTURE '999999.99'
      ROW=ROW+1
   ENDIF

  
   @ 56,28 SAY 'NationsBanc'
   @ 56,40 SAY 'xxxxxx'
   @ 60,02 SAY INVHDR.PIKTKT
   @ 60,09 SAY INVDATE
   @ 60,19 SAY CARTONS                  PICTURE '@Z 999'
   @ 60,25 SAY WEIGHT                   PICTURE '@Z 99999'
   @ 60,35 SAY INVHDR.INVOICE
   @ 60,73 SAY INVHDR.TOTALCHG          PICTURE '999999.99'

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
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/17/2001
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

* LINE 2
SELECT INVHDR
@ 02,40 SAY INVDATE
@ 02,50 SAY 'INVOICE: '+XINVOICE
@ 02,68 SAY 'PO: '+INVHDR.CUSTPO

IF llPrntComp
  @ 03,05 SAY HLINE1
ENDIF

* LINE 4
IF llPrntComp
  @ 04,05 SAY HLINE2
ENDIF

IF !EMPTY(lcDunsNo)
  @ 04,32 SAY 'DUNS: '+ ALLTRIM(lcDunsNo)
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
  @ 07,05 SAY IIF(EMPTY(lcCompPhon) , "" , TRANSFORM(lcCompPhon , lcPhonPict))
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 07,50 SAY XFADDR2
ENDIF

* LINE 8
IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ 08,50 SAY XFADDR3
ENDIF

@ 10,10 SAY XBTNAME

IF CURLOOP = '1'
  @ 10,47 SAY XSTNAME
ENDIF

@ 11,10 SAY XBTADDR1
IF CURLOOP = '1'
  @ 11,47 SAY XSTADDR1
ENDIF

@ 12,10 SAY XBTADDR2
IF CURLOOP = '1'
  @ 12,47 SAY XSTADDR2
ENDIF

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
@ 17,68 SAY LEFT(PshipVia,15)

*------------------
* PRINT SCALES
*------------------
SELE SCALE
SEEK 'S'
ROW = 19
X   = 1
Y   = ' '
DO WHILE X <= 5 .AND. !EOF() .AND. TYPE='S'
  @ ROW,25 SAY LEFT(SCALE,1)+Y+SZ1+Y+SZ2+Y+SZ3+Y+SZ4+Y+SZ5+Y+SZ6+Y+SZ7+Y+SZ8
  ROW = ROW + 1
  X  = X + 1
  SKIP
ENDDO

Row = 24

*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/17/2001
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
@ 54,01 SAY XPHONE + ' ' + INVHDR.NOTE1 + ' ' + INVHDR.NOTE2

IF LEN(ALLTRIM(lcRpMsg1 + lcRpMsg2)) >0
  @ 56,10 SAY lcRpMsg1
  @ 57,10 SAY lcRpMsg2
ENDIF

@ 60,02 SAY INVHDR.PIKTKT
@ 60,09 SAY INVDATE
@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY XINVOICE
@ 60,73 SAY '******.**'

*--End of lfGetcont.
*!*************************************************************
*! Name      : gfGetZone
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 06/17/2001
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINSS.PRG
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
