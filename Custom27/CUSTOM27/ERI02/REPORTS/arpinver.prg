*:************************************************************************
*: Program file  : ARPINVD.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:  Modifications:
*:         HDM B801884,1 Incorrect Bill to and ship to addresses
*:B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
*:                         it does not print the 2nd page and also does not print any totals.
*:                         And the phone.
*:B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*:E500340,1 BWA 05/09/2000 1)Modify option for printing the SKU# in the option grid
*:E500340,1                2)Fix the printing of some fields doesn't print.
*:E500340,1                3)Fix the wrong printing of the company address.
*:B803286,1 BWA 06/20/2000 1)Color field has to be moved to the left 1 column.
*:B803286,1                2)Description field has to be moved 4 columns to the left.
*:B803286,1                3)Remove telephone # printout from optional message box on bottom of invoice .
*:B803573,1 BWA 08/07/2000 Fix the bug of multi store order prints DC # in header, not store # and increase the custpo 2 character.
*:B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.
*:C102212,1 ADEL 03/25/20001 Add HST tax to all Invoice formats.
*:************************************************************************

XNOTE_LOOP = .F.  && Flag to indicate whether we have finished
                  && printing the Notepad or not.

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*

*E500340,1 Get color position and length. [START]
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
*E500340,1 [END]

NEWDOC = .T.
MAXROW = 47
*B801604,1 (Bebin) Initilize the distribution center var.
lcDist = ' '
*B801604,1 (End)

IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF
  
lnNotLine = 1
llNote = llRpInvNot
SELECT INVHDR

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
CLEAR TYPEAHEAD
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

LINEUP = .F.
STORE TRIM(QCOMPANY)                                       TO HLINE1
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3

*E500340,1 Fix the wrong printing of the company address. [START]
*STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
STORE TRIM(laCompAdd[3])                                   TO HLINE4
*E500340,1 [END]

lnNotLine = 1
STORE .F. TO llNoRec
STORE lcCompPhon                         TO HLINE5

IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF


SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
*--Begin Main Loop{HDM}
SCAN FOR &lcASExp

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
*      SEEK XINVOICE
*      IF EOF()
*         SELECT &INVHTEMP
*         SKIP
*         LOOP
*      ENDIF
      NEWDOC   = .F.
      XORDER    = ORDER
      *XPHONE    = laCompAdd[5]           && HDM.. IIF(EMPTY(PHONE),'',PHONE)
      XPHONE    = TRANSFORM(lcCompPhon , lcPhonPict)
      XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
      XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
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
      *** TAK 04/06/94
      SELE ORDHDR
      SEEK XORDER

      *** END TAK ***
        *-- Added by TAK 05/16/94
        IF INVHDR->CONSOL = 'Y'       
          SELECT CONSINVH
          SEEK XINVOICE
          SELECT CUSTOMER
          SEEK 'S'+XACCOUNT+CONSINVH->STORE
        ENDIF
        *-- End TAK 05/16/94
        SELECT CUSTOMER
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
      IF EOF()
        PRTERR = 2
      ENDIF

      *** GET THE DESCRIPTION ABOUT THE CODES [HDM]
      SELECT CODES
      SET ORDER TO CODES IN CODES 
  
      *TERMS
      PTERMS = gfCodDes(INVHDR->CTERMCODE,'CTERMCODE')

      *SHIPVIA
      PSHIPVIA = gfCodDes(INVHDR->SHIPVIA,'SHIPVIA')

      *SPC INST.
      PSPCINST= gfCodDes(INVHDR->SPCINST,'SPCINST')
      *= gfRltFld(INVHDR.UPSZONE , @laZone , 'SHIPVIA')
      = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
      
      XZN = laZone[1,2]

      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
      DO WHILE PRINTFCTR
         SELECT SYCFACT
         SEEK INVHDR->CFACCODE
         DO CASE
           CASE EOF()
             STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
           CASE FOUND()
             XFNAME  = CFACCOMP
             XFADDR1 = CADDRESS1
             XFADDR2 = CADDRESS2
             
             *E500340,1 Adjust the postion of this fields and print part of some fields. [START]   
             *XFADDR3 = TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5
             XFADDR3 = LEFT(TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5,30)
             *E500340,1 [END]
             
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
   ***
   ****** END NEWDOC
   ***
   *-----------------
   * START PRINT
   *-----------------
   SELECT INVHDR
   IF XINVNAME='Y'
     *SELE CODE 
     *SEEK 'D' + INVHDR->DIVISION
     *PTERMS = gfCodDes(INVHDR->CTERMCODE,'CTERMCODE')
     *HLINE1 = IIF(FOUND().AND. .NOT. EMPTY(DIVLNAME), DIVLNAME , QCOMPANY)
     
     HLINE1 = IIF(!EMPTY(lcDivLName), lcDivLName , QCOMPANY)
     
     @ 00,05 SAY ALLTRIM(HLINE1)
     SELECT INVHDR
   ENDIF
   
   *sha
   *@ 00,53 SAY XINVOICE
   @ 01,51 SAY XINVOICE
   *sha
   @ 01,70 SAY INVDATE

   @ 03,40 say "DUNS : "  + gfGetMemVar('XDUNS') 
 
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[start]
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

   @ 07,10 SAY XBTNAME
   IF CURLOOP = '1'
      @ 07,47 SAY XSTNAME
   ENDIF

   @ 08,10 SAY XBTADDR1
   IF CURLOOP = '1'
      @ 08,47 SAY XSTADDR1
   ENDIF

   @ 09,10 SAY XBTADDR2
   IF CURLOOP = '1'
      @ 09,47 SAY XSTADDR2
   ENDIF

   @ 10,10 SAY XBTADDR3
   IF CURLOOP = '1'
      @ 10,47 SAY XSTADDR3
   ENDIF
   
   *E500340,1 Fix the wrong printing of the company address. [START]
   @ 11,10 SAY 'DUNS: '+ TRIM(lcDunsNo)
   SELECT INVHDR
   *E500340,1 [END] 
   
   @ 15,01 SAY ACCOUNT
   
   *E500340,1 Adjust the postion of this fields and print part of some fields. [START]   
   *@ 15,09 SAY CUSTPO
   
   *B803573,1 Increase the custpo 2 character.[START]
   *@ 15,08 SAY LEFT(CUSTPO,10)
   *sha 
   *@ 15,08 SAY LEFT(CUSTPO,12)
   @ 15,09 SAY LEFT(CUSTPO,12)
   *sha
   *B803573,1 [END]
   
   *E500340,1 [END]
   
   *B801604,1 (Bebin) Say the DC if there is one.
   *@ 15,21 SAY IIF(CURLOOP='1',XSTORE,'')
   
   *B803573,1 BWA 08/07/2000 Fix the bug of multi store order prints DC # in header, not store #.[START]
   *@ 15,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
   @ 15,21 SAY IIF(CURLOOP='1',XSTORE,'')
   *B803573,1 [END]
   
   *B801604,1 (End)

   @ 15,30 SAY DEPT
   *E500340,1 Adjust the postion of this fields and print part of some fields. [START]
   
   *@ 15,36 SAY ORDER
   *@ 15,45 SAY PTERMS
   *@ 15,60 SAY REP1
   *@ 15,64 SAY REP2
   
   @ 15,36 SAY ORDER
   @ 15,45 SAY LEFT(PTERMS,15)
   *sha
   *@ 15,59 SAY REP1
   @ 15,55 SAY REP1
   *sha
   @ 15,63 SAY REP2
   *E500340,1 [END]
   
   *E500340,1 Adjust the postion of this fields and print part of some fields. [START]   
   *@ 15,68 SAY PSHIPVIA
   @ 15,68 SAY LEFT(PSHIPVIA,14)
   *E500340,1 [END]
   
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
      KEY = INVLINE->STYLE        && + INVLINE->COLOR
      SELECT STYLE
      SEEK KEY

      SELECT INVLINE
      
      *E500340,1 Adjust the style field and print the color. [START]
      *@ ROW,00 SAY SUBSTR(STYLE,1,7)
      *@ ROW,08 SAY SUBSTR(COLOR,1,3)                &&TMI 06/16/94      
      @ ROW,00 SAY SUBSTR(STYLE,1,lnMajor)

      *B803286,1 BWA 06/20/2000 1)Color field has to be moved to the left 1 column [START]
      *@ ROW,14 SAY SUBSTR(STYLE,lnClrPos,lcClrLen)
      @ ROW,13 SAY SUBSTR(STYLE,lnClrPos,lcClrLen)
      *B803286,1 [END]
      
      *E500340,1 [END]

      *E500340,1 Adjust the postion of this fields and print part of some fields. [START]
      *@ ROW,12 SAY STYLE->DESC

      *B803286,1 BWA 06/20/2000 2)Description field has to be moved 4 columns to the left.[START]
      *@ ROW,25 SAY STYLE.DESC
      @ ROW,21 SAY STYLE.DESC
      *B803286,1 [END]
      
      *E500340,1 [END]
      
      *sha 
      *@ ROW,58 SAY TOTQTY PICTURE '9999'
      @ ROW,57 SAY TOTQTY PICTURE '9999'
      *sha 
      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      
      *E500340,1 Adjust the postion of this fields and print part of some fields. [START]      
      *@ ROW,63  SAY PRICE     PICTURE '9999.99'
      *sha
      *@ ROW,64  SAY PRICE     PICTURE '9999.99'
      @ ROW,61  SAY PRICE     PICTURE '9999.99'
      *sha
      *E500340,1 [END]
      
      
      @ ROW,71  SAY XLINETOT  PICTURE '999999.99'
      
      ROW = ROW+1
      
      *E500340,1 Modify option for printing the SKU# in the option grid.[START]
      IF llRpPrtSku
        IF SEEK('S'+INVLINE.ACCOUNT+INVLINE.STYLE,"SPCK_HDR") OR  ;
           SEEK('P'+INVLINE.ACCOUNT+INVLINE.STYLE,"SPCK_HDR")
          @ ROW,00 SAY 'SKU# ' + SPCK_HDR.Pack_id
          ROW = ROW+1
        ENDIF
      ENDIF  
      *E500340,1 [END]
      
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
      *DO MSG22 WITH ' ANOTHER LINEUP? Y/N/Q => ','YNQ'
      DO CASE
         CASE CHOICE ='Q'
           SET DEVICE TO SCREEN
           RETURN
         CASE CHOICE ='N'
           LINEUP =.F.
      ENDCASE
      *DO MSG22 WITH 'PRINTING INVOICES - <Space Bar> TO ABORT','@'
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
      
      *B803286,1 BWA 06/20/2000 3)Remove telephone # printout from optional message box on bottom of invoice .[START]
      *@ 52,01 SAY ALLTRIM(XPHONE) SIZE 1,16
      *B803286,1 [END]
      
      @ 52,18 SAY XNOTE1 + ' ' + INVHDR->NOTE2
      *@ 52,01 SAY XPHONE + ' ' + XNOTE1 + ' ' + INVHDR->NOTE2
      *B500772,1 HISH 06/29/95.  ( End )

      IF LEN(TRIM(MSG1 + MSG2)) >0
         @ 53,10 SAY MSG1
         @ 54,10 SAY MSG2
      ENDIF
      *SHA
      @ 56,02 SAY INVHDR->PIKTKT  &&@ 58,02 SAY INVHDR->PIKTKT
      @ 56,09 SAY SHIPDATE  &&@ 58,09 SAY SHIPDATE
      @ 56,18 SAY '***'   &&@ 58,19 SAY '***'
      @ 56,25 SAY '*****'  &&@ 58,25 SAY '*****'
      @ 56,35 SAY XINVOICE  &&@ 58,35 SAY XINVOICE
      @ 56,72 SAY '******.**'  &&@ 58,72 SAY '******.**'
      *SHA
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
       
       *B803286,1 BWA 06/20/2000 3)Remove telephone # printout from optional message box on bottom of invoice .[START]
       *@ 52,01 SAY ALLTRIM(XPHONE) SIZE 1,16
       *B803286,1 [END]
       
       @ 52,18 SAY XNOTE1 + ' ' + INVHDR->NOTE2
       *@ 52,01 SAY XPHONE + ' ' + XNOTE1 + ' ' + INVHDR->NOTE2
       *B500772,1 HISH 06/29/95.  ( End )

       IF LEN(TRIM(MSG1 + MSG2)) >0
         @ 53,10 SAY MSG1
         @ 54,10 SAY MSG2
       ENDIF
       @ 56,02 SAY INVHDR->PIKTKT
       @ 56,09 SAY INVHDR->SHIPDATE
       @ 56,18 SAY '***'
       @ 56,25 SAY '*****'
       @ 56,35 SAY XINVOICE
       @ 56,72 SAY '******.**'
       LOOP
     ENDIF
   ENDIF
   SET MEMOWIDTH TO lnOldMemW
   *E100207,1 YMA 03/30/95 (End).
ENDIF

   SELECT INVHDR
   @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
    *sha
    *@ ROW,56 SAY XPIECES   PICTURE '999999'
     @ ROW,55 SAY XPIECES   PICTURE '999999'
    *sha
    @ ROW,70 SAY XSUBTOTAL PICTURE '9999999.99'
   *sha
   ROW=ROW+1
   IF DISCOUNT<>0
      @ ROW,12 SAY 'TOTAL - D I S C O U N T'
      @ ROW,69 SAY DISCOUNT PICTURE '99999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   *SHA
   *IF XTAX .AND. XTAX_METH = 'M' 
   IF XTAX .AND. XTAX_METH = 'M' AND INVHDR->TAX_AMT <> 0 
   *SHA
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     *-- End TMI 01/17/95
     @ ROW,64 SAY XSTRING_RATE + '%'

     @ ROW,70 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1
     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)

   ENDIF  
   
   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,12 SAY 'TOTAL - F R E I G H T'
      @ ROW,70 SAY WKAMT PICTURE '9999999.99'
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
     @ ROW,70 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1

     *B800080,1 TAK 08/16/95 Start, Added PST tax.
     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *B800080,1 TAK 08/16/95 End. 
     *C102212,1 (Begin) Add HST Tax.
     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C102212,1 (End)
   ENDIF  
   
   *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
   
   *B803286,1 BWA 06/20/2000 3)Remove telephone # printout from optional message box on bottom of invoice .[START]
   *@ 52,01 SAY ALLTRIM(XPHONE) SIZE 1,16
   *B803286,1 [END]
      
   @ 50,18 SAY XNOTE1 + ' ' + XNOTE2
   *@ 52,01 SAY XPHONE + ' ' + XNOTE1 + ' ' + XNOTE2
   *B500772,1 HISH 06/29/95.  ( End )

   IF LEN(TRIM(MSG1 + MSG2)) >0
      @ 51,10 SAY MSG1
      @ 52,10 SAY MSG2
   ENDIF
   IF INVHDR->APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ 54,56 SAY 'APPROVAL: ' + INVHDR->APPROVAL
   ENDIF
   @ 56,02 SAY INVHDR->PIKTKT
   @ 56,09 SAY INVHDR->SHIPDATE
   @ 56,18 SAY CARTONS PICTURE '@Z 999'
   @ 56,25 SAY WEIGHT  PICTURE '@Z 99999'
   @ 56,35 SAY INVHDR->INVOICE
   @ 56,70 SAY INVHDR->TOTALCHG PICTURE '9999999.99'
   SELECT INVHDR 
   
   *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.[START]
   *REPLACE PRTFLAG WITH 'P'
   *B603487,1 BWA 03/08/2000 [END]
   
   SELECT INVHDR
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN
   ELSE
     NEWDOC = .T.
   ENDIF

   *B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.[START]
   IF gcDevice <> 'SCREEN'
     INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
   ENDIF
   *B803585,1 [END]

ENDSCAN
SET DEVICE TO SCREEN
RETURN
*--------------------------------
*    END INV810C.PRG
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

*B803286,1 BWA 06/20/2000 3)Remove telephone # printout from optional message box on bottom of invoice .[START]
*@ 52,01 SAY ALLTRIM(XPHONE) SIZE 1,16
*B803286,1 [END]

@ 52,18 SAY XNOTE1 + ' ' + INVHDR->NOTE2
IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 53,10 SAY MSG1
  @ 54,10 SAY MSG2
ENDIF
@ 58,02 SAY INVHDR->PIKTKT
@ 58,09 SAY INVHDR->SHIPDATE
@ 58,19 SAY '***'
@ 58,25 SAY '*****'
@ 58,35 SAY XINVOICE
@ 58,72 SAY '******.**'

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
IF XINVNAME='Y'
  HLINE1 = IIF(!EMPTY(lcDivLName), lcDivLName , QCOMPANY)
  @ 00,05 SAY ALLTRIM(HLINE1)
  SELECT INVHDR
ENDIF
@ 00,53 SAY XINVOICE
@ 00,70 SAY INVDATE

* LINE 1
IF XINVNAME='Y'
  @ 01,05 SAY HLINE2
ENDIF

* LINE 2
IF XINVNAME='Y'
  @ 02,05 SAY HLINE3
ENDIF

*E500340,1 Fix printing "REMIT TO" with non factor invoice. [START]
*IF PRINTFCTR
IF PRINTFCTR AND !EMPTY(INVHDR.cFACCODE)
*E500340,1 [END]

  @ 02,50 SAY 'R E M I T   T O:'
ENDIF

* LINE 3
IF XINVNAME='Y'
  @ 03,05 SAY HLINE4
ENDIF
IF PRINTFCTR
  @ 03,50 SAY XFNAME
ENDIF

* LINE 4
IF XINVNAME='Y'

  *E500340,1 Fix the wrong printing of the company address. [START]
  *@ 04,05 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
  @ 04,05 SAY XPHONE SIZE 1,16
  *E500340,1 [END]
  
ENDIF
IF PRINTFCTR
  @ 04,50 SAY XFADDR1
ENDIF

* LINE 5
IF .NOT. EMPTY(XDUNS)
  @ 05,05 SAY 'DUNS: '+ TRIM(XDUNS)
ENDIF
IF PRINTFCTR
  @ 05,50 SAY XFADDR2
ENDIF

IF PRINTFCTR
  @ 06,50 SAY XFADDR3
ENDIF

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

@ 07,10 SAY XBTNAME
IF CURLOOP = '1'
  @ 07,47 SAY XSTNAME
ENDIF

@ 08,10 SAY XBTADDR1
IF CURLOOP = '1'
  @ 08,47 SAY XSTADDR1
ENDIF

@ 09,10 SAY XBTADDR2
IF CURLOOP = '1'
  @ 09,47 SAY XSTADDR2
ENDIF

@ 10,10 SAY XBTADDR3
IF CURLOOP = '1'
  @ 10,47 SAY XSTADDR3
ENDIF

*E500340,1 Fix the wrong printing of the company address. [START]
@ 11,10 SAY 'DUNS: '+ TRIM(lcDunsNo)
SELECT INVHDR
*E500340,1 [END]

@ 15,01 SAY ACCOUNT

*E500340,1 Adjust the postion of this fields and print part of some fields. [START]   
*@ 15,09 SAY CUSTPO

*B803573,1 Increase the custpo 2 character.[START]
*@ 15,08 SAY LEFT(CUSTPO,10)
@ 15,08 SAY LEFT(CUSTPO,12)
*B803573,1 [END]

*E500340,1 [END]

*B803573,1 BWA 08/07/2000 Fix the bug of multi store order prints DC # in header, not store #.[START]
*@ 15,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
@ 15,21 SAY IIF(CURLOOP='1', XSTORE ,'')
*B803573,1 [END]

@ 15,30 SAY DEPT
@ 15,36 SAY ORDER

*E500340,1 Adjust the postion of this fields and print part of some fields. [START]
*@ 15,45 SAY PTERMS
*@ 15,60 SAY REP1
*@ 15,64 SAY REP2

@ 15,45 SAY LEFT(PTERMS,15)
@ 15,59 SAY REP1
@ 15,63 SAY REP2
*E500340,1 [END]


*E500340,1 Adjust the postion of this fields and print part of some fields. [START]   
*@ 15,68 SAY PSHIPVIA
@ 15,68 SAY LEFT(PSHIPVIA,14)
*E500340,1 [END]

Row = 22          && NEW LINE in the function

*B803023,1 BWA 08/02/2000 [END]