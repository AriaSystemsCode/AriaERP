*:************************************************************************
*: Program file  : ARPINVD.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:Modifications  :
*:HDM B801884,1 Incorrect Bill to and ship to addresses
*:B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
*:                         it does not print the 2nd page and also does not print any totals.
*:                         And the phone.
*:B603443,1 BWA 10/02/2000 Fix the bug of shifting the first line of the style on printing.
*:B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.
*:B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.
*:C102212,1 ADEL 03/25/20001 Add HST tax to all Invoice formats.
*:B607103,1 KHM 05/04/2003 Fix the bug of not printing the decimals in the weight field
*:*****************************************************************************************

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*
NEWDOC = .T.
MAXROW = 47
llNote = llRpInvNot
*B801604,1 (Bebin) Initilize the distribution center var.
lcDist = ' '
LINEUP = .F.
HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
lnNotLine = 1
STORE .F. TO llNoRec
STORE lcCompPhon                         TO HLINE5

IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF

XNOTE_LOOP = .F.  && Flag to indicate whether we have finished
                  && printing the Notepad or not.

lcZone = ''
DECLARE laZone[1,2]

laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'

MSG1 = lcRpMsg1 && = 'lcRpMsg1'        && 1st. line Variable
MSG2 = lcRpMsg2
MSG3 = lcRpMsg1
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

XINVNAME = lcPrnComp
gcPhnFrmt = lcPhonPict
SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

SELECT INVHDR
  SCAN FOR &lcASExp
  *   IF XMODE  = 'M' .OR. XMODE = 'Y'
  *HDM      SELECT &lcDumFile
  *-- Get Customer DUNS[start]
  lnPrevAl = SELECT()
  SELECT CUSTOMER
  XDUNS = DUNS
  SELECT (lnPrevAl)
  *-- Get Customer DUNS[end]
  SELECT INVHDR
  XINVOICE = INVOICE
  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------

  IF NEWDOC
    PRTERR = 0
    STORE 0.00 TO XPIECES, XSUBTOTAL
    SELECT INVHDR
    IF EOF()
      NEWDOC    = .F.
    ENDIF
    XORDER    = ORDER
    
    *XPHONE    = laCompAdd[5]           && HDM.. IIF(EMPTY(PHONE),'',PHONE)
    
    *B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
    *                         it does not print the 2nd page and also does not print any totals.[START]
    XPHONE    = INVHDR.PHONE    
    *B803023,1 BWA 08/02/2000 [END]
    
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
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[start]
    =lfSolSpAdr()

    XBTNAME = lcSolTName
    XBTADDR1 = laSoldTo[1]
    XBTADDR2 = laSoldTo[2]
    XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
    IF LEN(TRIM(laSoldTo[2])) =0
      XBTADDR2 = laSoldTo[3]
      XBTADDR3 = ''
    ENDIF

    XSTNAME = lcShpTName
    XSTADDR1 = laShipTo[1]
    XSTADDR2 = laShipTo[2]
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      XSTADDR3 = ''
    ENDIF

    *--HDM B801884,1 Incorrect Bill to and ship to addresses[end]
    *** END TAK ***
    *-- Added by TAK 05/16/94
    IF INVHDR->CONSOL = 'Y'
      SELECT CONSINVH
      SEEK XINVOICE
      SELECT CUSTOMER
      SEEK 'S'+XACCOUNT+CONSINVH->STORE
    ENDIF
    *-- End TAK 05/16/94
    SELE CUSTOMER
    *B801604,1 ADEL (Begin) Seek the DC  address if there is one.
    IF !EMPTY(DIST_CTR)
      lcDist = DIST_CTR
      = SEEK('S'+XACCOUNT+lcDist)
    ENDIF
    *B801604,1 (End)

    ***
    ****** FIND THE INVOICE LINES
    SELECT INVLINE
    SEEK XINVOICE
    IF EOF()
      PRTERR = 2
    ENDIF
     
      *** GET THE DESCRIPTION ABOUT THE CODES
      
      SELECT CODES
      SET ORDER TO CODES IN CODES 
  
      *TERMS
      PTERMS = gfCodDes(INVHDR->CTERMCODE,'CTERMCODE')

      *SHIPVIA
      PSHIPVIA = gfCodDes(INVHDR->SHIPVIA,'SHIPVIA')

      *SPC INST.
      PSPCINST= gfCodDes(INVHDR->SPCINST,'SPCINST')
      *ahmed
      *= gfRltFld(INVHDR.UPSZONE , @laZone , 'SHIPVIA')
      = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
      
      XZN = laZone[1,2]

*--HDM The following was handeled by gfRltFld()[START]
*      DO CASE
*        CASE 'G' $ CUPS
*          *B500628,1 MFM 04/05/95 (Begin).
*          XZN = gfGetZone(Codes.cUps,XUPSFROM,SUBSTR(CUSTOMER->STZIP,1,3))
*          XZN = IIF(!EMPTY(XZN),'('+XZN+')',XZN)
*          *XZN  = '('+INVHDR->UPSZONE+')'
*          *B500628,1 MFM 04/05/95 (End).
*        CASE '2' $ CUPS
*          XZN  = '(12)'
*        CASE 'N' $ CUPS
*          XZN  = '(22)'
*        OTHERWISE
*          XZN  = ''
*      ENDCASE
*--HDM[END]
      PSHIPVIA = TRIM(PSHIPVIA) + XZN

      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      PRINTFCTR = llPrnFact                  && (XINVFACTOR = 'Y')
      STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
      SET DEVICE TO PRINTER
      DO WHILE PRINTFCTR
         SELECT SYCFACT
         SEEK INVHDR->CFACCODE
        DO CASE
          CASE EOF()
            PRINTFCTR = .F.
            STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3

          CASE FOUND()
            XFNAME  = CFACCOMP
            XFADDR1 = CADDRESS1
            XFADDR2 = CADDRESS2
            XFADDR3 = TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5
            IF CADDRESS2 = ' '
              CADDRESS2 = CADDRESS3
              CADDRESS3 = ' '
            ENDIF
         ENDCASE
         EXIT
      ENDDO
      CURLOOP = '1'
      STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
   ENDIF
   ****** END NEWDOC

*-- Lineup option goes here
*   IF PRTERR > 0
*      IF XMODE   = 'M' .OR. XMODE = 'Y'
*         SELE &INVHTEMP
*         SKIP
*         LOOP
*      ELSE
*         ? CHR(7)
*         DO MSG22 WITH 'INV800: AN ERROR HAS OCCURRED TRYING TO PRINT!','$'
*         WAIT WINDOW 'INV800: AN ERROR HAS OCCURRED TRYING TO PRINT!' NOWAIT
*         SET DEVICE TO SCREEN
*         RETURN
*      ENDIF
*   ENDIF

   *-----------------
   * START PRINT
   *-----------------
   * LINE 2
   SELECT INVHDR
   @ 02,40 SAY INVDATE
   @ 02,50 SAY 'INVOICE: '+XINVOICE
   @ 02,68 SAY 'PO: '+INVHDR->CUSTPO

   * LINE 3
   IF XINVNAME = 'Y'
*     SELECT CODES 
*     SEEK 'D' + INVHDR->CDIVISION
* HDM         
*     HLINE1 = IIF(!EMPTY(lcDivLName), gfCodDes(INVHDR->CDIVISION,'CDIVISION') , QCOMPANY)
     @ 03,05 SAY HLINE1
     SELE INVHDR
   ENDIF

   * LINE 4
   IF XINVNAME='Y'
     @ 04,05 SAY HLINE2
   ENDIF
   IF !EMPTY(XDUNS)
     *B500670,1 MFM 04/26/95 (Begin).
     *@ 04,32 SAY 'DUNS: '+ TRIM(XDUNS)
     @ 04,37 SAY 'DUNS: '+ TRIM(XDUNS)
     *B500670,1 MFM 04/26/95 (End).
   ENDIF
   IF PRINTFCTR
      @ 04,50 SAY 'R E M I T   T O:'
   ENDIF

   * LINE 5
   IF XINVNAME='Y'
      @ 05,05 SAY HLINE3
   ENDIF
   IF PRINTFCTR
      @ 05,50 SAY XFNAME
   ENDIF

   * LINE 6
   IF XINVNAME='Y'
      @ 06,05 SAY HLINE4
   ENDIF
   IF PRINTFCTR
      @ 06,50 SAY XFADDR1
   ENDIF

   * LINE 7
   IF XINVNAME='Y'
     *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
     @ 07,05 SAY HLINE5  PICTURE gcPhnFrmt SIZE 1,16
     *@ 07,05 SAY HLINE5
     *B500772,1 HISH 06/29/95.  ( End )
   ENDIF
   IF PRINTFCTR
      @ 07,50 SAY XFADDR2
   ENDIF

   * LINE 8
   IF PRINTFCTR
      @ 08,50 SAY XFADDR3
   ENDIF

   * LINE 10
   *--HDM B801884,1[START]
    SELECT CUSTOMER
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)
    =lfSolSpAdr()
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[start]
    SELECT CUSTOMER
    XBTNAME  = BTNAME
    XBTADDR1 = laSoldTo[1] &&CADDRESS1
    XBTADDR2 = laSoldTo[2] &&CADDRESS2
    XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
    IF LEN(TRIM(XBTADDR2)) =0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF
    *** TAK 04/06/94
    SELE ORDHDR
    SEEK XORDER
    XSTADDR1 = laShipTo[1] &&CADDRESS12
    XSTADDR2 = laShipTo[2] &&laShipTo22
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[END]

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
   @ 17,09 SAY CUSTPO
   *B801604,1 (Bebin) Say the DC if there is one.
   *@ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
   @ 17,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
   *B801604,1 (End)
   @ 17,30 SAY DEPT
   @ 17,36 SAY ORDER
   @ 17,44 SAY PTERMS
   @ 17,60 SAY REP1
   @ 17,64 SAY REP2
   @ 17,68 SAY PSHIPVIA
   *------------------
   * PRINT SCALES
   *------------------

   *-- MFM 05/11/94.
   lcScalStr = SPACE(0)
   lcInvNo   = INVHDR.INVOICE                         && &InvHtemp..Invoice
* Should check for user celection criteria  DHM
   DO lpGtScl WITH lcInvNo
   SELECT Scale
   Row = 19
   FOR lnI = 1 TO LEN( lcScalStr )
     lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
     SEEK 'S' + lcSizeScal
     *--Changed by TAK 11/29/94
*    @ Row,25 SAY lcSizeScal+' '+GetScale( lcSizeScal, SPACE(1) )
     @ Row,25 SAY lcSizeScal
     
     *B500670,1 MFM 04/26/95 (Begin) Added the 'ALLTRIM'.
     @ Row,27 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ1),1,3),3,' ')
     @ Row,31 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ2),1,3),3,' ')
     @ Row,35 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ3),1,3),3,' ')
     @ Row,39 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ4),1,3),3,' ')
     @ Row,43 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ5),1,3),3,' ')
     @ Row,47 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ6),1,3),3,' ')
     @ Row,51 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ7),1,3),3,' ')
     @ Row,55 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ8),1,3),3,' ')
     *B500670,1 MFM 04/26/95 (End).

     Row = Row + 1
   ENDFOR
   *--END MFM 05/11/94.

   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   
   *B603443,1 BWA 10/02/2000 Fix the bug of shifting the first line of the style on printing.[START]
   *ROW    = 24
   ROW    = 25
   *B603443,1 BWA 10/02/2000 [END]

   SET DEVICE TO PRINTER
   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
      
        *B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
        *                         it does not print the 2nd page and also does not print any totals.[START]
        IF ROW >= MAXROW
           =lfGetcont()
           =lfGetInHdr()
           LOOP
         ENDIF
        *B803023,1 BWA 08/02/2000 [END]         
      
         EXIT
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF
      KEY = INVLINE->STYLE && HDM+ INVLINE->COLOR
      SELECT STYLE
      SEEK KEY
      *-- TMI 06/16/94 
      SELECT INVLINE
      @ ROW,00 SAY SUBSTR(STYLE,1,7)
*HDM      @ ROW,08 SAY SUBSTR(COLOR,1,3)              &&TMI 06/16/94
      @ ROW,12 SAY SUBSTR(STYLE->DESC,1,12)
      @ ROW,25 SAY STYLE->SCALE
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
      ROW = ROW + 1
      IF LINEUP
         EXIT
      ENDIF

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING

   SET DEVICE TO PRINT
*   IF LINEUP .AND. !XNOTE_LOOP
*      EJECT
*      DO MSG22 WITH ' ANOTHER LINEUP? Y/N/Q => ','YNQ'

*      DO CASE
*         CASE CHOICE ='Q'
*           RETURN
*         CASE CHOICE ='N'
*           LINEUP =.F.
*      ENDCASE
*      DO MSG22 WITH 'PRINTING INVOICES - <Space Bar> TO ABORT','@'
*      WAIT WINDOW "PRINTING INVOICES - <Space Bar> TO ABORT" NOWAIT
*      SET DEVICE TO PRINT
*      STORE 0.00 TO XPIECES, XSUBTOTAL
*      LOOP
*   ENDIF

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')

   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR
      *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
*      @ 54,01 SAY XPHONE  PICTURE gcPhnFrmt SIZE 1,16
       @ 54,01 SAY XPHONE  SIZE 1,16
      @ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *@ 54,01 SAY XPHONE + ' ' + INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *B500772,1 HISH 06/29/95.  ( End )

      IF LEN(TRIM(MSG1 + MSG2)) >0
         @ 56,10 SAY MSG1
         @ 57,10 SAY MSG2
      ENDIF
      @ 60,02 SAY INVHDR->PIKTKT
      *-- MFM 06/27/94.
      @ 60,09 SAY ShipDate
      @ 60,19 SAY '***'
      @ 60,25 SAY '*****'
      @ 60,35 SAY XINVOICE
      @ 60,73 SAY '******.**'
      LOOP
   ENDIF

   *** Print invoice Notepad.
  IF llNote
   SELECT NOTEPAD
   *E100207,1 YMA 03/30/95 (Begin).
   lnOldMemW = SET("MEMOWIDTH")
   SET MEMOWIDTH TO 75

   IF TYPE + KEY <> 'C' + XINVOICE
     SEEK 'C' + XINVOICE
     
     *B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.[START]
     *lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   *ELSE
   
    * lnMemLins = 0
   ENDIF
   
     lnMemLins = MEMLINES(NOTEPAD.MNOTES)
     *B603444,1 BWA 10/02/2000 [END]
   
   IF TYPE + KEY = 'C' + XINVOICE
     @ ROW,02 SAY '* -- N O T E S -- *' 
     ROW = ROW + 1 

     *SCAN WHILE TYPE + KEY = 'C' + XINVOICE
     SET DEVICE TO PRINTER
     DO WHILE lnNotLine <= lnMemLins
       IF ROW >= MAXROW
         XNOTE_LOOP = .T.
         
        *B803023,1 BWA 08/02/2000 [START]         
         *EXIT        && I comment this line of code.
        *B803023,1 BWA 08/02/2000 [END]
        
       ELSE
         XNOTE_LOOP = .F.
         *B803023,1 BWA 08/02/2000 [START]
         *@ ROW,02 SAY MLINE(MNOTES,lnNotLine)
         @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
         *B803023,1 BWA 08/02/2000 [END]
         ROW = ROW + 1
       ENDIF
       
        *B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
        *                         it does not print the 2nd page and also does not print any totals.[START]
        IF ROW >= MAXROW      && if there is a line will de delete from 
          =lfGetcont()        && the memo field
          =lfGetInHdr()
        ENDIF
        *B803023,1 BWA 08/02/2000 [END]
        
       lnNotLine = lnNotLine + 1
     ENDDO

     IF !XNOTE_LOOP
       @ ROW,02 SAY '* -- END OF NOTES -- *'
       lnNotLine = 1
       ROW = ROW + 1 
     ELSE
       @ ROW+1,12 SAY 'C O N T I N U E D ...'
       SELECT INVHDR
       
       @ 54,01 SAY XPHONE  SIZE 1,16
       @ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2

       IF LEN(TRIM(MSG1 + MSG2)) >0
          @ 56,10 SAY MSG1
          @ 57,10 SAY MSG2
       ENDIF
       @ 60,02 SAY INVHDR->PIKTKT
       *-- MFM 06/27/94.
       @ 60,09 SAY InvHdr.ShipDate
       @ 60,19 SAY '***'
       @ 60,25 SAY '*****'
       @ 60,35 SAY XINVOICE
       @ 60,73 SAY '******.**'
       LOOP
     ENDIF
   ENDIF

   SET MEMOWIDTH TO lnOldMemW
   *E100207,1 YMA 03/30/95 (End).
ENDIF &&HDM
   SELECT INVHDR
   @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
   @ ROW,57 SAY XPIECES              PICTURE '999999'
   @ ROW,72 SAY XSUBTOTAL            PICTURE '9999999.99'
   ROW=ROW+1

   IF DISCOUNT<>0
      @ ROW,12 SAY 'TOTAL - D I S C O U N T'
      @ ROW,71 SAY DISCOUNT         PICTURE '99999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,72 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1
     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,72 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,72 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)

   ENDIF  
   
   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,12 SAY 'TOTAL - F R E I G H T'
      @ ROW,72 SAY WKAMT            PICTURE '9999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,72 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1

     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,72 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,72 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  

   @ 54,01 SAY XPHONE SIZE 1,16
   @ 54,18 SAY XNOTE1 + ' ' + XNOTE2

   IF LEN(TRIM(MSG1 + MSG2)) >0
      @ 56,10 SAY MSG1
      @ 57,10 SAY MSG2
   ENDIF

   IF INVHDR->APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ 59,56 SAY 'APPROVAL: ' + INVHDR->APPROVAL
   ENDIF

   @ 60,02 SAY INVHDR->PIKTKT
   *-- MFM 06/27/94.
   @ 60,09 SAY ShipDate
   @ 60,19 SAY CARTONS               PICTURE '@Z 999'
   
   *B607103,1 KHM 05/04/2003 (Begin) Allow decimal point in the weight field.
   *@ 60,25 SAY WEIGHT                PICTURE '@Z 99999'
   @ 60,25 SAY WEIGHT                PICTURE '@Z 99.99'   
   *B607103,1 KHM 05/04/2003 (End)
   
   @ 60,35 SAY INVHDR->INVOICE
   @ 60,72 SAY INVHDR->TOTALCHG      PICTURE '9999999.99'
   SELECT INVHDR
   
   *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.[START]
   *REPLACE PRTFLAG WITH 'P'
   *B603487,1 BWA 03/08/2000 [END]
   
   *-- GET NEXT HEADER RECORD
   SELECT INVHDR
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN
   ELSE
     NEWDOC = .T.
   ENDIF
  *--HDM
  SELECT INVHDR
  
  *B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.[START]
  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
  *B803585,1 [END]

ENDSCAN
SET DEVICE TO SCREEN
RETURN
*--------------------------------
*    END INV810A.PRG
*--------------------------------

*!*************************************************************
*! Name : lpGtScl.
*! Auth : Mohamed Fahmy Mohamed (MFM).
*! Date : 05/11/94.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Build a string holding the first uniqe 5 scales
*!            from the invoice line scal.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Passed :
*!        Parameters : 
*!           lcInvNo : The Current invoice number.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Returned : 
*!        Variables  : lcScalStr.
*!*************************************************************
PROCEDURE lpGtScl

PARAMETERS lcInvNo

PRIVATE lnRecNo, lcAlias, lnCount

*-- Save the current alias.
lcAlias = ALIAS()

SELECT InvLine
*B600987,1 MFM 02/28/96 (Begin) Build the current record expr. string,
*B600987,1 MFM          instead of saving the record number.
lcExp   = Invoice + STR(LineNo,6)
*lnRecNo = RECNO()
*B600987,1 MFM 02/28/96 (End).

lnCount = 1

SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 5
  IF !( Scale $ lcScalStr )
    lcScalStr = lcScalStr + Scale
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN

*B600987,1 MFM 02/28/96 (Begin) Seek for the saved expr. instead of going to 
*B600987,1 MFM          the record number.
= SEEK(lcExp,"InvLine")
*GOTO lnRecNo
*B600987,1 MFM 02/28/96 (End).

*-- Select the old alias.
SELECT (lcAlias)

*-- EOF( lpGtScl )

*!*************************************************************
*! Name : gfGetZone.
*! Auth : Mohamed Fahmy Mohamed (MFM).
*! Date : 04/05/95.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�
*! Synopsis : Get the zone to be printed in the invoice format.
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip
PRIVATE lnOldWrk

IF !USED('FRTZONES')
  lnOldWrk = SELECT()
  SELECT 0
  DO NETUSE WITH '&QLB.FRTZONES','&QLB.FRTZONES','SH'
  SELECT (lnOldWrk)
ENDIF
SET DEVICE TO SCREEN

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,'FRTZONES'),'',FRTZONES.ZONE)

*B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
*                         it does not print the 2nd page and also does not print any totals.[START]
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
@ 54,01 SAY XPHONE  SIZE 1,16
@ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 56,10 SAY MSG1
  @ 57,10 SAY MSG2
ENDIF
@ 60,02 SAY INVHDR->PIKTKT
@ 60,09 SAY InvHdr.ShipDate
@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY XINVOICE
@ 60,73 SAY '******.**'

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 08/02/2000
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
@ 02,40 SAY INVDATE
@ 02,50 SAY 'INVOICE: '+XINVOICE
@ 02,68 SAY 'PO: '+INVHDR->CUSTPO

* LINE 3
IF XINVNAME = 'Y'
  @ 03,05 SAY HLINE1
  SELE INVHDR
ENDIF

* LINE 4
IF XINVNAME='Y'
  @ 04,05 SAY HLINE2
ENDIF
IF !EMPTY(XDUNS)
  @ 04,37 SAY 'DUNS: '+ TRIM(XDUNS)
ENDIF
IF PRINTFCTR
  @ 04,50 SAY 'R E M I T   T O:'
ENDIF

* LINE 5
IF XINVNAME='Y'
  @ 05,05 SAY HLINE3
ENDIF
IF PRINTFCTR
  @ 05,50 SAY XFNAME
ENDIF

* LINE 6
IF XINVNAME='Y'
  @ 06,05 SAY HLINE4
ENDIF
IF PRINTFCTR
  @ 06,50 SAY XFADDR1
ENDIF

* LINE 7
IF XINVNAME='Y'
  @ 07,05 SAY HLINE5  PICTURE gcPhnFrmt SIZE 1,16
ENDIF
IF PRINTFCTR
  @ 07,50 SAY XFADDR2
ENDIF

* LINE 8
IF PRINTFCTR
  @ 08,50 SAY XFADDR3
ENDIF

* LINE 10
SELECT CUSTOMER
SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)
=lfSolSpAdr()
SELECT CUSTOMER
XBTNAME  = BTNAME
XBTADDR1 = laSoldTo[1] &&CADDRESS1
XBTADDR2 = laSoldTo[2] &&CADDRESS2
XBTADDR3 = TRIM(laSoldTo[3]) + ' ' +TRIM(laSoldTo[4]) + ' ' + laSoldTo[5]
IF LEN(TRIM(XBTADDR2)) =0
  XBTADDR2 = XBTADDR3
  XBTADDR3 = ''
ENDIF

SELE ORDHDR
SEEK XORDER
XSTADDR1 = laShipTo[1] &&CADDRESS12
XSTADDR2 = laShipTo[2] &&laShipTo22
XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
IF LEN(TRIM(XSTADDR2)) =0
  XSTADDR2 = XSTADDR3
  XSTADDR3 = ''
ENDIF

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
@ 17,09 SAY CUSTPO
@ 17,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
@ 17,30 SAY DEPT
@ 17,36 SAY ORDER
@ 17,44 SAY PTERMS
@ 17,60 SAY REP1
@ 17,64 SAY REP2
@ 17,68 SAY PSHIPVIA
*------------------
* PRINT SCALES
*------------------
lcScalStr = SPACE(0)
lcInvNo   = INVHDR.INVOICE                         && &InvHtemp..Invoice
DO lpGtScl WITH lcInvNo
SELECT Scale
Row = 19
FOR lnI = 1 TO LEN( lcScalStr )
  lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  SEEK 'S' + lcSizeScal
  @ Row,25 SAY lcSizeScal
  @ Row,27 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ1),1,3),3,' ')
  @ Row,31 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ2),1,3),3,' ')
  @ Row,35 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ3),1,3),3,' ')
  @ Row,39 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ4),1,3),3,' ')
  @ Row,43 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ5),1,3),3,' ')
  @ Row,47 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ6),1,3),3,' ')
  @ Row,51 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ7),1,3),3,' ')
  @ Row,55 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ8),1,3),3,' ')
  Row = Row + 1
ENDFOR

Row = 25          && NEW LINE in the function

*B803023,1 BWA 08/02/2000 [END]