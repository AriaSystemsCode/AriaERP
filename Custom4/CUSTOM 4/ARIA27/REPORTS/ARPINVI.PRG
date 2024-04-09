*:************************************************************************
*: Program file  : ARPINVI.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:Modifications  :
*:            HDM B801884,1 Incorrect Bill to and ship to addresses
*:B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
*:                         it does not print the 2nd page and also does not print any totals.
*:                         And the phone.
*:B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*:B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.
*:B607103,1 KHM 05/04/2003 Fix the bug of not printing the decimals in the weight field
*:************************************************************************

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*

lnNotLine = 1
STORE .F. TO llNoRec
STORE lcCompPhon                         TO HLINE5

IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

XNOTE_LOOP = .F.
NEWDOC = .T.
MAXROW = 47
*B801604,1 (Bebin) Initilize the distribution center var.
lcDist = ' '
*B801604,1 (End)
llNote = llRpInvNot
lnNotLine = 1
LINEUP = .T.
SELECT INVHDR
*------------------------------
* SECTION: MAIN LOOP
*------------------------------
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
*DO WHILE .T.            &&         &lcRpExp          && HDM..     INKEY() <>32
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
SET DEVICE TO PRINTER
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
      PAGENO=0
      SELECT INVHDR
      NEWDOC    = .F.
      XORDER    = ORDER
      *XPHONE    = laCompAdd[5]
      
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
        *B801604,1 ADEL (Begin) Seek the DC if there is one.
        IF !EMPTY(DIST_CTR)
          lcDist = DIST_CTR
          = SEEK('S'+XACCOUNT+lcDist)
        ENDIF
        *B801604,1 (End)

      ***
      ****** FIND THE INVOICE LINES
      SELECT INVLINE
*      SEEK XINVOICE
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
      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      PRINTFCTR = llPrnFact
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

   SELECT INVHDR
   *-----------------
   * START PRINT
   *-----------------
   PAGENO=PAGENO+1
   
   @ 03,70 SAY XINVOICE
   @ 04,70 SAY INVHDR->CUSTPO
   @ 05,70 SAY INVDATE
   IF PRINTFCTR
      @ 06,06 SAY 'R E M I T   T O:'
   ENDIF
   @ 06,70 SAY PAGENO PICTURE '9999'
   
   IF PRINTFCTR
      @ 07,06 SAY XFNAME
      @ 08,06 SAY XFADDR1
      @ 09,06 SAY XFADDR2
      @ 10,06 SAY XFADDR3
   ENDIF
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

   @ 13,06 SAY XBTNAME
   IF CURLOOP = '1'
      @ 13,46 SAY XSTNAME
   ENDIF

   @ 14,06 SAY XBTADDR1
   IF CURLOOP = '1'
      @ 14,46 SAY XSTADDR1
   ENDIF

   @ 15,06 SAY XBTADDR2
   IF CURLOOP = '1'
      @ 15,46 SAY XSTADDR2
   ENDIF

   @ 16,06 SAY XBTADDR3
   IF CURLOOP = '1'
      @ 16,46 SAY XSTADDR3
   ENDIF
   SELECT INVHDR
   @ 20,01 SAY ACCOUNT
   @ 20,10 SAY CUSTPO
   *B801604,1 (Bebin) Say the DC if there is one.
   *@ 20,21 SAY IIF(CURLOOP='1',XSTORE,'')
   @ 20,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
   *B801604,1 (End)
   @ 20,31 SAY DEPT
   @ 20,37 SAY ORDER
   @ 20,47 SAY SUBSTR(PTERMS,1,13)
   @ 20,61 SAY REP1
   @ 20,65 SAY REP2
   @ 20,69 SAY PSHIPVIA
   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 24
   DO WHILE CURLOOP = '1' .AND. .NOT. XNOTE_LOOP           
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

      KEY = INVLINE->STYLE           &&+ INVLINE->COLOR
      SELECT STYLE
      SEEK KEY

      SELECT INVLINE

      @ ROW,00 SAY SUBSTR(STYLE,1,7)
*      @ ROW,08 SAY SUBSTR(COLOR,1,3)           &&TMI 06/16/94
      @ ROW,13 SAY STYLE->DESC
      @ ROW,60 SAY TOTQTY PICTURE '9999'

      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT

      @ ROW,65  SAY PRICE     PICTURE '9999.99'
      @ ROW,75  SAY XLINETOT  PICTURE '999999.99'
      ROW = ROW+1

*     IF LINEUP
*        EXIT
*     ENDIF

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING
   SET DEVICE TO PRINT
  IF LINEUP .AND. .NOT. XNOTE_LOOP .AND. gcDevice = 'PRINTER'
     EJECT
       lnRespns = gfModalGen("QRM40152B40014","Dialog")
       IF  lnRespns = 1
         SET DEVICE TO PRINT
         LINEUP =.T.
       ELSE
         IF lnRespns = 2
           LINEUP =.F.
         ELSE
           LINEUP =.F.
           SET DEVICE TO SCREEN
           RETURN
         ENDIF
       ENDIF
     SET DEVICE TO PRINT
     STORE 0.00 TO XPIECES, XSUBTOTAL
  ENDIF
   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. .NOT. XNOTE_LOOP                 
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR
      @ 52,16 SAY XNOTE1
      @ 53,16 SAY XNOTE2
      IF LEN(TRIM(MSG1 + MSG2)) >0
        @ 54,01 SAY MSG1
        @ 55,01 SAY MSG2
      ENDIF
      @ 58,02 SAY INVHDR->PIKTKT
      @ 58,10 SAY INVDATE
      @ 58,19 SAY '***'
      @ 58,25 SAY '*****'
      @ 58,75 SAY '******.**'
      LOOP
   ENDIF
IF llNote
   SELECT NOTEPAD
   *E100207,1 YMA 03/30/95 (Begin).
   lnOldMemW = SET("MEMOWIDTH")
   SET MEMOWIDTH TO 75

   IF TYPE + KEY <> 'C' + XINVOICE
     SEEK 'C' + XINVOICE
     *B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.[START]
     *lnMemLins = MEMLINES(NOTEPAD.MNOTES)
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
       @ 52,16 SAY XNOTE1
       @ 53,16 SAY XNOTE2
       IF LEN(TRIM(MSG1 + MSG2)) >0
        @ 54,01 SAY MSG1
        @ 55,01 SAY MSG2
       ENDIF
       @ 58,02 SAY INVHDR->PIKTKT
       @ 58,10 SAY INVHDR->INVDATE
       @ 58,19 SAY '***'
       @ 58,25 SAY '*****'
       @ 58,75 SAY '******.**'
       LOOP
     ENDIF
   ENDIF

   SET MEMOWIDTH TO lnOldMemW
   *E100207,1 YMA 03/30/95 (End).
ENDIF
   SELECT INVHDR
   ROW=ROW+1
   @ ROW,13 SAY 'TOTAL - M E R C H A N D I S E'
   @ ROW,59 SAY XPIECES              PICTURE '99999'
   @ ROW,74 SAY XSUBTOTAL            PICTURE '9999999.99' 
   ROW=ROW+1

   IF DISCOUNT<>0
      @ ROW,13 SAY 'TOTAL - D I S C O U N T'
      @ ROW,73 SAY DISCOUNT             PICTURE '99999999.99' 
      ROW=ROW+1
   ENDIF

   WKAMT = FREIGHT + INSUR + COD
   @ 52,16 SAY XNOTE1
   @ 52,74 SAY XSUBTOTAL + DISCOUNT PICTURE '9999999.99' 
   @ 53,16 SAY XNOTE2

   IF WKAMT <> 0
     @ 53,74 SAY WKAMT PICTURE '9999999.99' 
   ENDIF

   IF LEN(TRIM(MSG1 + MSG2)) >0
      @ 54,01 SAY MSG1
      @ 55,01 SAY MSG2
   ENDIF

   @ 58,02 SAY INVHDR->PIKTKT
   @ 58,10 SAY INVDATE
   @ 58,19 SAY CARTONS                  PICTURE '@Z 999'

   *B607103,1 KHM 05/04/2003 (Begin) Allow decimal point in the weight field.
   *@ 58,25 SAY WEIGHT                   PICTURE '@Z 99999'
   @ 58,25 SAY WEIGHT                   PICTURE '@Z 99.99'   
   *B607103,1 KHM 05/04/2003 (End)
   
   @ 58,74 SAY INVHDR->TOTALCHG         PICTURE '9999999.99'  
   SELECT INVHDR
   
   *B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.[START]
   *REPLACE PRTFLAG WITH 'P'
   *B603487,1 BWA 03/08/2000 [END]
   
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
*** END INV810H.PRG

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
@ 52,16 SAY XNOTE1
@ 53,16 SAY XNOTE2
IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 54,01 SAY MSG1
  @ 55,01 SAY MSG2
ENDIF
@ 58,02 SAY INVHDR->PIKTKT
@ 58,10 SAY INVHDR->INVDATE
@ 58,19 SAY '***'
@ 58,25 SAY '*****'
@ 58,75 SAY '******.**'

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

PAGENO=PAGENO+1
   
@ 03,70 SAY XINVOICE
@ 04,70 SAY INVHDR->CUSTPO
@ 05,70 SAY INVDATE
IF PRINTFCTR
  @ 06,06 SAY 'R E M I T   T O:'
ENDIF
@ 06,70 SAY PAGENO PICTURE '9999'
   
IF PRINTFCTR
  @ 07,06 SAY XFNAME
  @ 08,06 SAY XFADDR1
  @ 09,06 SAY XFADDR2
  @ 10,06 SAY XFADDR3
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

@ 13,06 SAY XBTNAME
IF CURLOOP = '1'
  @ 13,46 SAY XSTNAME
ENDIF

@ 14,06 SAY XBTADDR1
IF CURLOOP = '1'
  @ 14,46 SAY XSTADDR1
ENDIF

@ 15,06 SAY XBTADDR2
IF CURLOOP = '1'
  @ 15,46 SAY XSTADDR2
ENDIF

@ 16,06 SAY XBTADDR3
IF CURLOOP = '1'
  @ 16,46 SAY XSTADDR3
ENDIF
SELECT INVHDR
@ 20,01 SAY ACCOUNT
@ 20,10 SAY CUSTPO
@ 20,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
@ 20,31 SAY DEPT
@ 20,37 SAY ORDER
@ 20,47 SAY SUBSTR(PTERMS,1,13)
@ 20,61 SAY REP1
@ 20,65 SAY REP2
@ 20,69 SAY PSHIPVIA

Row = 24          && NEW LINE in the function

*B803023,1 BWA 08/02/2000 [END]