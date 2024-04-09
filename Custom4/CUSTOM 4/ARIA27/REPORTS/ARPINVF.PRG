*:************************************************************************
*: Program file  : ARPINVD.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*: Modifications :
*:               HDM B801884,1 Incorrect Bill to and ship to addresses
*:B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
*:                         it does not print the 2nd page and also does not print any totals.
*:                         And the phone.
*:B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*:B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.
*:C102212,1 ADEL 03/25/20001 Add HST tax to all Invoice formats.
*:B607103,1 KHM 05/04/2003 Fix the bug of not printing the decimals in the weight field
*:************************************************************************

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*
NEWDOC = .T.
MAXROW = 47
llNote = llRpInvNot

IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

*B801604,1 (Bebin) Initilize the distribution center var.
lcDist = ' '
LINEUP = .F.
STORE TRIM(QCOMPANY)                                       TO HLINE1
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
MSG3 = lcRpMsg3
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

XINVNAME = lcPrnComp

gcPhnFrmt = lcPhonPict
SELECT INVHDR
*SET FILTER TO (lcRpExp)

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

SCAN FOR &lcASExp
*   IF XMODE  = 'M' .OR. XMODE = 'Y'
*-- Get Customer DUNS[start]
lnPrevAl = SELECT()
SELECT CUSTOMER
XDUNS = DUNS
SELECT (lnPrevAl)
*-- Get Customer DUNS[end]

SELECT INVHDR
XINVOICE = INVOICE
*   ENDIF

   *-----------------------------------------------------------------
   * Get invoice header, line items, and financial history records.
   * If any records are not found, skip to next invoice.
   * Initialize document totals.
   *-----------------------------------------------------------------
   IF NEWDOC
      XNOLINES = .F.
      STORE 0.00 TO XPIECES, XSUBTOTAL
      SELECT INVHDR
      SEEK XINVOICE
      NEWDOC    = .F.
      XORDER    = ORDER
      *XPHONE    = laCompAdd[5]
      
      *B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
      *                         it does not print the 2nd page and also does not print any totals.[START]
      XPHONE    = INVHDR.PHONE    
      *B803023,1 BWA 08/02/2000 [END]

      XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
      XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
      XCODTAG   = CODTAG
      XORDER    = ORDER
      XPIKTKT   = PIKTKT
      XACCOUNT  = ACCOUNT
      XSTORE    = STORE
      XSEASON   = SEASON
      XDIVISION = CDIVISION
      ***
      *** GET THE BILL TO AND SHIP ADDRESS
      ***
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
      ***
      SELECT INVLINE
*      SEEK XINVOICE
*      IF EOF()
*        XNOLINES=.T.
*      ENDIF
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

      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      PRINTFCTR = .T.        && IIF(XINVFACTOR='Y', .T., .F.)
      STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3

      DO WHILE PRINTFCTR
         SELECT SYCFACT
         SEEK INVHDR->CFACCODE
         DO CASE
           CASE EOF()
             *PRINTFCTR = .F.
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
   *-----------------
   * START PRINT
   *-----------------
   * LINE 2
   SELECT INVHDR
   IF XINVNAME='Y'
*     SELE CODE 
*     SEEK 'D' + INVHDR->DIVISION
*     HLINE1 = IIF(FOUND().AND. .NOT. EMPTY(DIVLNAME), DIVLNAME , QCOMPANY)
     @ 03,05 SAY HLINE1
     SELE INVHDR
   ENDIF

   * LINE 3
   @ 03,66 SAY 'INVOICE: '+XINVOICE

   * LINE 4
   IF XINVNAME='Y'
     @ 04,05 SAY HLINE2
   ENDIF
   IF .NOT. EMPTY(XDUNS)
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
*      @ 07,05 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
      @ 07,05 SAY XPHONE SIZE 1,16
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
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[END]

   @ 11,10 SAY XBTNAME
   IF CURLOOP = '1'
      @ 11,56 SAY XSTNAME
   ENDIF

   @ 12,10 SAY XBTADDR1
   IF CURLOOP = '1'
      @ 12,56 SAY XSTADDR1
   ENDIF

   @ 13,10 SAY XBTADDR2
   IF CURLOOP = '1'
      @ 13,56 SAY XSTADDR2
   ENDIF

   @ 14,10 SAY XBTADDR3
   IF CURLOOP = '1'
      @ 14,56 SAY XSTADDR3
   ENDIF

   @ 17,01 SAY ACCOUNT
   @ 17,11 SAY ORDER
   @ 17,21 SAY PTERMS
   @ 17,38 SAY PSHIPVIA
   *B801604,1 (Bebin) Say the DC if there is one.
   *@ 17,53 SAY IIF(CURLOOP='1',XSTORE,'')
   @ 17,53 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
   *B801604,1 (End)
   @ 17,59 SAY DEPT
   @ 17,66 SAY REP1
   @ 17,70 SAY REP2
   @ 17,76 SAY IIF(INVHDR->STATUS='V','** VOID **',CUSTPO)

   *------------------
   * PRINT SCALES
   *------------------
   SELE SCALE
   SEEK 'S'
   ROW = 19
   X   = 1
   Y   = ' '
   DO WHILE X <= 3 .AND. .NOT. EOF() .AND. TYPE='S'
     *B500670,1 MFM 04/26/95 (Begin) Added the 'ALLTRIM'.
     @ ROW,30 SAY SCALE+Y+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                          PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
     *B500670,1 MFM 04/26/95 (End).
     ROW = ROW + 1
     X  = X + 1
     SKIP
   ENDDO

   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 22
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
      KEY = INVLINE->STYLE &&+ INVLINE->COLOR
      SELECT STYLE
      SEEK KEY

      SELECT INVLINE
*      @ ROW,01 SAY SUBSTR(STYLE,1,7)
      @ ROW,01 SAY SUBSTR(STYLE,1,10)
*      @ ROW,08 SAY SUBSTR(COLOR,1,3)                &&TMI 06/16/94
      @ ROW,12 SAY SUBSTR(STYLE->DESC,1,15)
      @ ROW,30 SAY STYLE->SCALE
      @ ROW,32 SAY QTY1   PICTURE '@Z 999'
      @ ROW,36 SAY QTY2   PICTURE '@Z 999'
      @ ROW,40 SAY QTY3   PICTURE '@Z 999'
      @ ROW,44 SAY QTY4   PICTURE '@Z 999'
      @ ROW,48 SAY QTY5   PICTURE '@Z 999'
      @ ROW,52 SAY QTY6   PICTURE '@Z 999'
      @ ROW,56 SAY QTY7   PICTURE '@Z 999'
      @ ROW,60 SAY QTY8   PICTURE '@Z 999'
      @ ROW,63 SAY TOTQTY PICTURE '9999'

      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      @ ROW,69  SAY PRICE     PICTURE '9999.99'
      @ ROW,77  SAY XLINETOT  PICTURE '999,999.99'
      ROW = ROW+1
      IF LINEUP
         EXIT
      ENDIF

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING

   SET DEVICE TO PRINT
   IF LINEUP .AND. !XNOTE_LOOP
      EJECT
      DO MSG22 WITH ' ANOTHER LINEUP? Y/N/Q => ','YNQ'
      DO CASE
         CASE CHOICE ='Q'
           SET DEVICE TO SCREEN
           RETURN
         CASE CHOICE ='N'
           LINEUP =.F.
      ENDCASE
      DO MSG22 WITH 'PRINTING INVOICES - <Space Bar> TO ABORT','@'
      SET DEVICE TO PRINT
      STORE 0.00 TO XPIECES, XSUBTOTAL
      LOOP
   ENDIF

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR

      *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
      @ 54,01 SAY XPHONE SIZE 1,16
      @ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *@ 54,01 SAY XPHONE + ' ' + INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *B500772,1 HISH 06/29/95.  ( End )
      
      IF LEN(TRIM(MSG1 + MSG2)) >0
         @ 56,10 SAY MSG1
         @ 57,10 SAY MSG2
      ENDIF
      @ 59,60 SAY 'CODTAG# '+XCODTAG
      @ 60,02 SAY INVHDR->PIKTKT
      @ 60,09 SAY INVDATE
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
     *lnMemLins = 0
   ENDIF
   
   lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   *B603444,1 BWA 10/02/2000 [END]   
   
   IF TYPE + KEY = 'C' + XINVOICE
     @ ROW,02 SAY '* -- N O T E S -- *' 
     ROW = ROW + 1 
 
     *SCAN WHILE TYPE + KEY = 'C' + XINVOICE
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
     
     IF .NOT. XNOTE_LOOP
       @ ROW,02 SAY '* -- END OF NOTES -- *'
       lnNotLine = 1
       ROW = ROW + 1 
     ELSE
       @ ROW+1,12 SAY 'C O N T I N U E D ...'
       SELECT INVHDR

       *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
       @ 54,01 SAY XPHONE SIZE 1,16
       @ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
       *@ 54,01 SAY XPHONE + ' ' + INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
       *B500772,1 HISH 06/29/95.  ( End )

       IF LEN(TRIM(MSG1 + MSG2)) >0
          @ 56,10 SAY MSG1
          @ 57,10 SAY MSG2
       ENDIF
       @ 59,60 SAY 'CODTAG# '+XCODTAG
       @ 60,02 SAY INVHDR->PIKTKT
       @ 60,09 SAY INVHDR->INVDATE
       @ 60,19 SAY '***'
       @ 60,25 SAY '*****'
       @ 60,35 SAY XINVOICE
       @ 60,73 SAY '******.**'
       LOOP
     ENDIF
   ENDIF

   SET MEMOWIDTH TO lnOldMemW
   *E100207,1 YMA 03/30/95 (End).
ENDIF
   SELECT INVHDR
   @ ROW,11 SAY '** MERCHANDISE **'
   @ ROW,50 SAY 'TOTAL PIECES'
   @ ROW,63 SAY XPIECES   PICTURE '999999'
   @ ROW,74 SAY XSUBTOTAL PICTURE '9999999.99'
   ROW=ROW+1

   IF DISCOUNT<>0
      @ ROW,11 SAY 'LESS : DISCOUNT'
      @ ROW,73 SAY DISCOUNT PICTURE '99999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,11 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,32 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,74 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1
     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,11 SAY 'PST TAX'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,74 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,11 SAY 'HST TAX'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,74 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  
   
   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,11 SAY 'FREIGHT'
      @ ROW,74 SAY WKAMT PICTURE '9999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,11 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,32 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,74 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1

     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,11 SAY 'PST TAX'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,74 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,11 SAY 'HST TAX'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,74 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  

   @ 41,08 SAY XNOTE1
   @ 42,08 SAY XNOTE2
   IF INVHDR->APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ 43,01 SAY 'APPROVAL: ' + INVHDR->APPROVAL
   ENDIF
   @ 45,01 SAY INVHDR->PIKTKT
   @ 45,08 SAY INVDATE
   @ 45,21 SAY SUBSTR(PSHIPVIA,1,12)
   @ 45,35 SAY INVHDR->INVOICE
   @ 45,43 SAY CARTONS          PICTURE '@Z 999'

   *B607103,1 KHM 05/04/2003 (Begin) Allow decimal point in the weight field.
   *@ 45,51 SAY WEIGHT           PICTURE '@Z 99999'
   @ 45,51 SAY WEIGHT           PICTURE '@Z 99.99'
   *B607103,1 KHM 05/04/2003 (End)

   @ 45,73 SAY INVHDR->TOTALCHG PICTURE '9999999.99'

   SELECT INVHDR
   *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.[START]
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

   *B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.[START]
   IF gcDevice <> 'SCREEN'
     INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
   ENDIF
   *B803585,1 [END]

ENDSCAN
SET DEVICE TO SCREEN
RETURN
*--------------------------------
*    END INV810E.PRG
*--------------------------------

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

@ 54,01 SAY XPHONE SIZE 1,16
@ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 56,10 SAY MSG1
  @ 57,10 SAY MSG2
ENDIF
@ 59,60 SAY 'CODTAG# '+XCODTAG
@ 60,02 SAY INVHDR->PIKTKT
@ 60,09 SAY INVHDR->INVDATE
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

* LINE 2
SELECT INVHDR
IF XINVNAME='Y'
  @ 03,05 SAY HLINE1
  SELE INVHDR
ENDIF

* LINE 3
@ 03,66 SAY 'INVOICE: '+XINVOICE

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
  @ 07,05 SAY XPHONE SIZE 1,16
ENDIF
IF PRINTFCTR
  @ 07,50 SAY XFADDR2
ENDIF

* LINE 8
IF PRINTFCTR
  @ 08,50 SAY XFADDR3
ENDIF
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

@ 11,10 SAY XBTNAME
IF CURLOOP = '1'
  @ 11,56 SAY XSTNAME
ENDIF

@ 12,10 SAY XBTADDR1
IF CURLOOP = '1'
  @ 12,56 SAY XSTADDR1
ENDIF

@ 13,10 SAY XBTADDR2
IF CURLOOP = '1'
  @ 13,56 SAY XSTADDR2
ENDIF

@ 14,10 SAY XBTADDR3
IF CURLOOP = '1'
  @ 14,56 SAY XSTADDR3
ENDIF

@ 17,01 SAY ACCOUNT
@ 17,11 SAY ORDER
@ 17,21 SAY PTERMS
@ 17,38 SAY PSHIPVIA
@ 17,53 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
@ 17,59 SAY DEPT
@ 17,66 SAY REP1
@ 17,70 SAY REP2
@ 17,76 SAY IIF(INVHDR->STATUS='V','** VOID **',CUSTPO)

*------------------
* PRINT SCALES
*------------------
SELE SCALE
SEEK 'S'
ROW = 19
X   = 1
Y   = ' '
DO WHILE X <= 3 .AND. .NOT. EOF() .AND. TYPE='S'
  @ ROW,30 SAY SCALE+Y+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                       PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                       PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                       PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
  ROW = ROW + 1
  X  = X + 1
  SKIP
ENDDO

Row = 22          && NEW LINE in the function

*B803023,1 BWA 08/02/2000 [END]