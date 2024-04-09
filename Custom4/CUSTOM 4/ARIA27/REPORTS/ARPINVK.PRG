*:************************************************************************
*: Program file  : ARPINVK.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:  Modifications:
*:              HDM B801884,1 Incorrect Bill to and ship to addresses
*:B803023,1 BWA 08/02/2000 Fix the bug if you have an Invoice with more than one page ,
*:                         it does not print the 2nd page and also does not print any totals.
*:                         And the phone.
*:B603443,1 BWA 10/02/2000 Fix the bug of shifting the first line of the style on printing.
*:B603444,1 BWA 10/02/2000 Fix the bug of variable lnMemLins not found.
*:B803039,1 ABD 02/20/2000 Fix bug that Program does not print all information in the part 
*:B803039,1                That print the CUST#,CUSTPO#,TERM,...etc.
*:B603487,1 BWA 03/08/2000 Comment the update of the field PRTFLAG in the piktkt file.
*:B803206,1 BWA 07/27/2000 Fix the bug of the line header doesn't print in the right postion.
*:B803206,1                2)Item quantities and the total pieces quantity has to shift 2 columns left.
*:B803206,1                3)Color field needs to be moved to the right 1 column.
*:B803206,1                4)The phone and the invoice notes has to print one line down.
*:B803538,1 BWA 07/27/2000 Fix the bug of we have more than 3 scale the invocie didn't print right.
*:B803717,1 BWA 10/08/2000 Fix the bug of add Comma after city code,print zip code and remove empty line on printout in the factor address.
*:C102212,1 ADEL 03/25/2001 Add HST tax to all Invoice formats.
*:B804211,1 AME 06/25/2001 Remove phone from address3,4,5 line 
*:B804211,1                and print it one line next, with fax No. follow it.
*:B804510,1 ADEL 11/05/01  Get the Customer's duns from the main account as it isn't
*:B804510,1                saved on Sotres level. 
*:B605147,1 SSE 11/21/2001 Fix bug of not printing the Cust Po in case of multi store order.
*:B605205,1 SSE 12/06/2001 Fix bug of always printing the factor information for all invoices.
*:B606528,1 RAE 12/21/2002 Fix the bug of printing the name of DC instead of the Store name.
*:B607103,1 KHM 05/04/2003 Fix the bug of not printing the decimals in the weight field
*:B607366,1 NAD 07/02/2003 Update the print flag
*:B126091,1 NNA 01/13/2005 Fix bug that if the customer is Extended size scale, report print
*:B126091,1 NNA            scale duplicated 
*:B128175,1 MMR 07/27/2005 Fix bug of printing * in the weight field.
*:************************************************************************


XNOTE_LOOP = .F.  && Flag to indicate whether we have finished
                  && printing the Notepad or not.

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95

*B126091,1 NNA 01/13/2005 (BEGIN) Initiate 2 new variables
*--llCScale : to know that I get all the scale string like(AAB--> if Extended) or (C)
llCScale  = .F.
lcSizeScal=''
*B126091,1 NNA (END)

*B804510,1 SSE Get the duns number. [Begin]
XDUNS = gfGetMemVar('XDUNS',gcAct_Comp)
*B804510,1 SSE Get the duns number. [End]

RELE ALL LIKE M_*

*B803206,1 BWA 07/27/2000 Fix 3)Color field needs to be moved to the right 1 column.[START]
STORE 0 TO lnLenth , lnClrLen , lnClrPos
*--THE STYLE LENGTH
lnLenth = LEN(gfItemMask('PM'))

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*B803206,1 [END]


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

SELECT INVHDR

*------------------------------
* SECTION: MAIN LOOP
*------------------------------

LINEUP = .F.
HLINE1 = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
*B804211,1 AME [Start] remove the phone number from the address line.
*STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]                   TO HLINE4
*B804211,1 AME [End]
lnNotLine = 1
STORE .F. TO llNoRec
*B804211,1 AME [Start] 
*STORE lcCompPhon                         TO HLINE5

*B804510,1 Remove the company fax number. [Begin]
*STORE TRANSFORM(lcCompPhon , lcPhonPict)+'      '+lcCompFax TO HLINE5
PRIVATE lnLoop , llPhonExst
llPhonExst = .F.
FOR lnLoop = 1 TO 4
  *lcLoop = STR(lnLoop,1)
  IF ALLTRIM(laCompAdd[lnLoop]) = ALLTRIM(TRANSFORM(lcCompPhon , lcPhonPict))
    llPhonExst = .T.
    HLINE5 = ''
  ENDIF
ENDFOR
IF !llPhonExst
  STORE TRANSFORM(lcCompPhon , lcPhonPict) TO HLINE5
ENDIF  
*B804510,1 Remove the company fax number. [End]

*B804211,1 AME [End] 
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

*B803717,1 BWA 10/08/2000 Comment this message because there is no work to the space bar.[START]
*WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
*B803717,1 [END]

SET DEVICE TO PRINT

XINVNAME = lcPrnComp
gcPhnFrmt = lcPhonPict
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

SELECT INVHDR
SCAN FOR &lcASExp

  *B803717,1 BWA 10/08/2000 Add this message instead of the message of the space bar.[START]
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  *B803717,1 [END]

  *-- Get Customer DUNS[start]
  *B804510,1 (Begin) Get the duns on main account level.
  *XDUNS = DUNS
  *B804510,1 (Begin)
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
      SEEK XINVOICE
      IF EOF()
         SELECT &INVHTEMP
         SKIP
         LOOP
      ENDIF
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
        
        *B601753,1 KHM 06/01/97 (Begin) Commenting the following lines
        *IF INVHDR->CONSOL = 'Y'       
          *SELECT CONSINVH
          *SEEK XINVOICE
          *SELECT CUSTOMER
          *SEEK 'S'+XACCOUNT+CONSINVH->STORE
        *ENDIF
        *B601753,1 KHM 06/01/97 (End) 
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

      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      *PRINTFCTR = (XINVFACTOR='Y')
      PRINTFCTR = llPrnFact           && Comes from form settings file

      *B803717,1 BWA 10/08/2000 Fix the bug of add Comma after city code,print zip code 
      *B803717,1                and remove empty line on printout in the factor address.[START]
      *STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
      *DO WHILE PRINTFCTR
      *   SELECT SYCFACT
      *   SEEK INVHDR->CFACCODE
      *   DO CASE
      *     CASE EOF()
      *       STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
      *     CASE FOUND()
      *       XFNAME  = CFACCOMP
      *       XFADDR1 = CADDRESS1
      *       XFADDR2 = CADDRESS2
      *       XFADDR3 = TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5
      *       IF CADDRESS2 = ' '
      *          CADDRESS2 = CADDRESS3
      *          CADDRESS3 = ' '
      *       ENDIF
      *   ENDCASE
      *   EXIT
      *ENDDO

      XFNAME  = lcFacName           && Factor company name.
      XFADDR1 = laFactor[1]         && Factor address#1.
      XFADDR2 = laFactor[2]
      XFADDR3 = laFactor[3]
      IF LEN(TRIM(laFactor[2])) = 0  && If address#2 is empty fill it with address#3.
        XFADDR2 = laFactor[3]
        XFADDR3 = ''
      ENDIF
      *B803717,1[END]

      CURLOOP = '1'
      STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
   ENDIF
   ****** END NEWDOC
   *-----------------
   * START PRINT
   *-----------------
   * LINE 2
   
   SELECT INVHDR
   @ 02,40 SAY INVDATE
   @ 02,50 SAY 'INVOICE: '+XINVOICE
   @ 02,68 SAY 'PO: '+INVHDR->CUSTPO

   * LINE 3
   IF XINVNAME='Y'
*     SELE CODE 
*     SEEK 'D' + INVHDR->DIVISION
*     HLINE1 = IIF(FOUND().AND. !EMPTY(DIVLNAME), DIVLNAME , QCOMPANY)
     @ 03,05 SAY HLINE1
     SELE INVHDR
   ENDIF
   *B800353,1 YMA 11/26/95 Moved one line up and modified to print the 
   *B800353,1 YMA 11/26/95 full length of the DUNS number. 
   IF !EMPTY(XDUNS)
     @ 03,50 SAY 'DUNS: '+ ALLTRIM(XDUNS)
   ENDIF
   *B800353,1 YMA 11/26/95 End.

   * LINE 4
   IF XINVNAME='Y'
     @ 04,05 SAY HLINE2
   ENDIF
   
   *B800353,1 YMA 11/26/95 Moved one line UP.
   *IF !EMPTY(XDUNS)
   *  @ 04,37 SAY 'DUNS: '+ SUBSTR(TRIM(XDUNS),1,7)
   *ENDIF
   *B800353,1 YMA 11/26/95 End.

   *B605205,1 Print factor on factor settings and invoice is factored. [Begin]
   *IF PRINTFCTR
   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
   *B605205,1 Print factor on factor settings and invoice is factored. [End]
   
      @ 04,50 SAY 'R E M I T   T O:'
   ENDIF

   * LINE 5
   IF XINVNAME='Y'
      @ 05,05 SAY HLINE3
   ENDIF

   *B605205,1 Print factor on factor settings and invoice is factored. [Begin]
   *IF PRINTFCTR
   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
   *B605205,1 Print factor on factor settings and invoice is factored. [End]

      @ 05,50 SAY XFNAME
   ENDIF

   * LINE 6
   IF XINVNAME='Y'
      @ 06,05 SAY HLINE4
   ENDIF

   *B605205,1 Print factor on factor settings and invoice is factored. [Begin]
   *IF PRINTFCTR
   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
   *B605205,1 Print factor on factor settings and invoice is factored. [End]

      @ 06,50 SAY XFADDR1
   ENDIF

   * LINE 7
   *B804211,1 AME [Start] Print the phone one line below and print fax with its line.
   *B804211,1 AME         and Shift the invhdr phone one line below.
   *IF XINVNAME='Y'
   *  @ 07,05 SAY XPHONE SIZE 1,16
   *ENDIF
   IF XINVNAME='Y'
      @ 07,05 SAY HLINE5
   ENDIF
   *B804211,1 AME [End]

   *B605205,1 Print factor on factor settings and invoice is factored. [Begin]
   *IF PRINTFCTR
   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
   *B605205,1 Print factor on factor settings and invoice is factored. [End]

      @ 07,50 SAY XFADDR2
   ENDIF

   * LINE 8
   *B804211,1 AME [Start] shift the invhdr phone one line below.

   *B804510,1 Remove the invoice phone, commented out. [Begin]
   *IF XINVNAME='Y'
   *  @ 08,05 SAY XPHONE SIZE 1,16
   *ENDIF
   *B804510,1 Remove the invoice phone, commented out. [End]

   *B804211,1 AME [End]

   *B605205,1 Print factor on factor settings and invoice is factored. [Begin]
   *IF PRINTFCTR
   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
   *B605205,1 Print factor on factor settings and invoice is factored. [End]

      @ 08,50 SAY XFADDR3
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
    
    *B605147,1 No need to select Order Header file. [Begin]
    *SELE ORDHDR
    SELECT InvHdr
    *B605147,1 No need to select Order Header file. [End]
    
    *B803039,1 ABD OrdHdr already engaged in relation with InvHdr, No need to this seek [ Begin]
    *SEEK XORDER
    *B803039,1 ABD [ End ]
    XSTADDR1 = laShipTo[1] &&CADDRESS12
    XSTADDR2 = laShipTo[2] &&laShipTo22
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF
    *--HDM B801884,1 Incorrect Bill to and ship to addresses[END]

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
   *B803039,1 ABD Remark the net line and print 11 character from the Cutomer PO. [ Begin ]
   *@ 17,09 SAY CUSTPO
   @ 17,09 SAY LEFT(CUSTPO,11)
   *B803039,1 ABD [ END ]
   *B801604,1 (Bebin) Say the DC if there is one.
   *@ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
   
   *B605147,1 Return to print the store of the invoice header again. [Begin]
   *@ 17,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
   @ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
   *B605147,1 Return to print the store of the invoice header again. [End]
   
   *B801604,1 (End)
   @ 17,30 SAY DEPT
   @ 17,36 SAY ORDER
   *B803039,1 ABD Remark the net line and print 15 character from the terms. [ Begin ]
   *@ 17,44 SAY PTERMS   
    @ 17,44 SAY LEFT(PTERMS,15)
   *B803039,1 ABD [ End]
   @ 17,60 SAY REP1
   @ 17,64 SAY REP2
   
   *B800946,1 ARH 01/28/97 (Begin) Fixed the bug of the ship via being
   *B800946,1              wrapped around to the next line in case of
   *B800946,1              printing a UPS invoice.
   *@ 17,68 SAY PSHIPVIA
   *B803039,1 ABD Remark the net line and print 12 character from the Shipvia. [ Begin ]
   *@ 17,67 SAY PSHIPVIA
   @ 17,68 SAY LEFT(PSHIPVIA,12)
   *B803039,1 ABD [ End ]
   *B800946,1 ARH 01/28/97 (End)
   
   *------------------
   * PRINT SCALES
   *------------------
   lcScalStr = SPACE(0)
   lcInvNo   = InvHDR.Invoice
   DO lpGtScl WITH lcInvNo
   SELECT Scale
   Row = 19

   FOR lnI = 1 TO LEN( lcScalStr )

     *B126091,1 NNA 01/13/2005 (BEGIN) Collecting the Scale Chrs. to seek with it
     *lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
     *--I get lcScalStr like (A-B-C->If not extended OR AAA-BBB-CCC->IF Extended)
     *--so if the current chr.<>'-' that mean I'm with the same scale and adding this chr. to 
     *--the scale's String. but if the current chr.='-' that mean I got the all string for the scale
     *--and I can now searhing with it to get its sizes.
     IF SUBSTR( lcScalStr, lnI, 1 )<>'-' 
       lcSizeScal = lcSizeScal+SUBSTR( lcScalStr, lnI, 1 )
       llCScale  = .F.     
     ELSE
       llCScale  = .T.     
     ENDIF
     *--if llCScale is true that mean I have the complete chrs. for the scale.
     *--but in the last chr. llCScale will be false so i'm using the second condition.
     IF llCScale OR lnI=LEN( lcScalStr )
     *B126091,1 NNA (END)

       SEEK 'S' + lcSizeScal
       @ Row,22 SAY lcSizeScal
       @ Row,24 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ1),1,3),3,' ')
       @ Row,28 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ2),1,3),3,' ')
       @ Row,32 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ3),1,3),3,' ')
       @ Row,36 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ4),1,3),3,' ')
       @ Row,40 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ5),1,3),3,' ')
       @ Row,44 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ6),1,3),3,' ')
       @ Row,48 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ7),1,3),3,' ')
       @ Row,52 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ8),1,3),3,' ')

       *B126091,1 NNA (BEGIN) Empty the variable lcSizeScal
       lcSizeScal = '' 
       *B126091,1 NNA (END)

       Row = Row + 1

     *B126091,1 NNA(BEGIN)       
     ENDIF
     *B126091,1 NNA (END)

   ENDFOR

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
      KEY = INVLINE->STYLE        &&+ INVLINE->COLOR
      SELECT STYLE
      SEEK KEY
      SELECT INVLINE
      
      *B803206,1 BWA 07/27/2000 Fix 3)Color field needs to be moved to the right 1 column.[START]
      *@ ROW,00 SAY STYLE
      *@ ROW,13 SAY COLOR
      *@ ROW,22 SAY STYLE->SCALE
      
      @ ROW,00 SAY LEFT(STYLE,lnLenth)
      @ ROW,14 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)

      *B126091,1 NNA 01/13/2005 (BEGIN) Show all style scale because it this extended s_scale
      *B126091,1 NNA            we show only the first chr. of the scale      
      *@ ROW,22 SAY LEFT(STYLE.SCALE,1)
      @ ROW,22 SAY ALLTRIM(STYLE->SCALE)
      *B126091,1 NNA (END)
      
      *B803206,1 [END]

      @ ROW,23 SAY QTY1   PICTURE '@Z 9999'
      @ ROW,27 SAY QTY2   PICTURE '@Z 9999'
      @ ROW,31 SAY QTY3   PICTURE '@Z 9999'
      @ ROW,35 SAY QTY4   PICTURE '@Z 9999'
      @ ROW,39 SAY QTY5   PICTURE '@Z 9999'
      @ ROW,43 SAY QTY6   PICTURE '@Z 9999'
      @ ROW,47 SAY QTY7   PICTURE '@Z 9999'
      @ ROW,51 SAY QTY8   PICTURE '@Z 9999'
      @ ROW,55 SAY TOTQTY PICTURE '99999'

      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      @ ROW,63  SAY PRICE     PICTURE '9999.99'
      @ ROW,71  SAY XLINETOT  PICTURE '999999.99'
      ROW = ROW + 1

      *B600757,1 TAK 10/30/95 Added style desc.
      @ ROW,00 SAY 'STYLE DESC : '+STYLE.Desc
      ROW = ROW + 1
      
      IF LINEUP
         EXIT
      ENDIF

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING

  IF LINEUP .AND. .NOT. XNOTE_LOOP .AND. gcDevice = 'PRINTER'
     EJECT
       IF gfModalGen("QRM40140B40000","Dialog") = 1
         SET DEVICE TO PRINT
         LINEUP =.T.
       ELSE
         LINEUP =.F.
       ENDIF
     SET DEVICE TO PRINT
     STORE 0.00 TO XPIECES, XSUBTOTAL
  ENDIF

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')

   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR
      
      *B803206,1 BWA 07/27/2000 Fix 4)The phone and the infoice notes has to print one line down.[START]
      *@ 54,01 SAY XPHONE SIZE 1,16
      *@ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2

      @ 55,01 SAY XPHONE SIZE 1,16
      @ 55,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
      *B803206,1 [END]
      
      IF LEN(TRIM(MSG1 + MSG2)) >0
         @ 56,10 SAY MSG1
         @ 57,10 SAY MSG2
      ENDIF
      @ 60,02 SAY INVHDR->PIKTKT
      @ 60,09 SAY ShipDate
      @ 60,19 SAY '***'
      @ 60,25 SAY '*****'
      @ 60,35 SAY XINVOICE
      @ 60,71 SAY '******.**'
      LOOP
   ENDIF

   *** Print invoice Notepad.
   IF llRpInvNot
     SELECT NOTEPAD
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
     
       IF !XNOTE_LOOP
         @ ROW,02 SAY '* -- END OF NOTES -- *'
         lnNotLine = 1
         ROW = ROW + 1 
       ELSE
         @ ROW+1,12 SAY 'C O N T I N U E D ...'
         SELECT INVHDR
         
         *B803206,1 BWA 07/27/2000 Fix 4)The phone and the infoice notes has to print one line down.[START]
         *@ 54,01 SAY XPHONE SIZE 1,16
         *@ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2

         @ 55,01 SAY XPHONE SIZE 1,16
         @ 55,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
         *B803206,1 [END]
      
          IF LEN(TRIM(MSG1 + MSG2)) >0
            @ 56,10 SAY MSG1
            @ 57,10 SAY MSG2
          ENDIF
          @ 60,02 SAY INVHDR->PIKTKT
          @ 60,09 SAY InvHdr.ShipDate
          @ 60,19 SAY '***'
          @ 60,25 SAY '*****'
          @ 60,35 SAY XINVOICE
          @ 60,71 SAY '******.**'
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
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
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
      @ ROW,70 SAY WKAMT            PICTURE '9999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
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

   *B803206,1 BWA 07/27/2000 Fix 4)The phone and the infoice notes has to print one line down.[START]
   *@ 54,01 SAY XPHONE SIZE 1,16
   *@ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2

   @ 55,01 SAY XPHONE SIZE 1,16
   @ 55,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
   *B803206,1 [END]

   IF LEN(TRIM(MSG1 + MSG2)) >0
      @ 56,10 SAY MSG1
      @ 57,10 SAY MSG2
   ENDIF

   IF INVHDR->APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ 59,56 SAY 'APPROVAL: ' + INVHDR->APPROVAL
   ENDIF

   *B800115,1 YMA 08/25/95 Moved the pick ticket number 2 character 
   *B800115,1 to the left.
   @ 60,00 SAY INVHDR->PIKTKT
   *B800115,1 YMA 08/25/95 End.
   
   @ 60,08 SAY ShipDate
   @ 60,17 SAY CARTONS               PICTURE '@Z 999'

   *B607103,1 KHM 05/04/2003 (Begin) Allow decimal point in the weight field.
   *@ 60,25 SAY WEIGHT                PICTURE '@Z 99999'
   *B128175,1 MMR 07/27/2005 Fix bug of printing * in the weight field.[Start]
   *@ 60,25 SAY WEIGHT                PICTURE '@Z 99.99'
   @ 60,25 SAY WEIGHT                PICTURE '@Z 999999.99'
   *B128175,1 MMR[End]
   *B607103,1 KHM 05/04/2003 (End)
   
   @ 60,35 SAY INVHDR->INVOICE
   @ 60,70 SAY INVHDR->TOTALCHG      PICTURE '9999999.99'
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

*B607366,1 NAD Update the print flag (START)
IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF
*B607366,1 NAD (END)

ENDSCAN
SET DEVICE TO SCREEN
RETURN
*--------------------------------
*    END INV810A.PRG
*--------------------------------

*!*************************************************************
*! Name : lpGtScl.
*! Auth : Mohamed Fahmy Mohamed (MFM).
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

  *B803538,1 BWA 07/27/2000 Fix the bug of we have more than 3 scale the invocie didn't print right.[START]
  *IF !( Scale $ lcScalStr )
  IF !( ALLTRIM(Scale) $ lcScalStr )
    *lcScalStr = lcScalStr + Scale

    *B126091,1 NNA 01/13/2005 (BEGIN) split between scales by '-'
    *lcScalStr = lcScalStr + ALLTRIM(Scale)
    lcScalStr = lcScalStr+IIF(!EMPTY(lcScalStr),'-','') + ALLTRIM(Scale)
    *B126091,1 NNA 01/13/2005 (BEGIN)
    
    *B803538,1 [END]

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
       
*B803206,1 BWA 07/27/2000 Fix 4)The phone and the infoice notes has to print one line down.[START]
*@ 54,01 SAY XPHONE SIZE 1,16
*@ 54,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2

@ 55,01 SAY XPHONE SIZE 1,16
@ 55,18 SAY INVHDR->NOTE1 + ' ' + INVHDR->NOTE2
*B803206,1 [END]
       
IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 56,10 SAY MSG1
  @ 57,10 SAY MSG2
ENDIF
@ 60,02 SAY INVHDR->PIKTKT
@ 60,09 SAY InvHdr.ShipDate
@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY XINVOICE
@ 60,71 SAY '******.**'

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
@ 02,40 SAY INVDATE
@ 02,50 SAY 'INVOICE: '+XINVOICE
@ 02,68 SAY 'PO: '+INVHDR->CUSTPO

* LINE 3
IF XINVNAME='Y'
  @ 03,05 SAY HLINE1
  SELE INVHDR
ENDIF
IF !EMPTY(XDUNS)
  @ 03,50 SAY 'DUNS: '+ ALLTRIM(XDUNS)
ENDIF

* LINE 4
IF XINVNAME='Y'
  @ 04,05 SAY HLINE2
ENDIF
   
*B605205,1 Print factor on factor settings and invoice is factored. [Begin]
*IF PRINTFCTR
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
*B605205,1 Print factor on factor settings and invoice is factored. [End]

  @ 04,50 SAY 'R E M I T   T O:'
ENDIF

* LINE 5
IF XINVNAME='Y'
  @ 05,05 SAY HLINE3
ENDIF

*B605205,1 Print factor on factor settings and invoice is factored. [Begin]
*IF PRINTFCTR
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
*B605205,1 Print factor on factor settings and invoice is factored. [End]

  @ 05,50 SAY XFNAME
ENDIF

* LINE 6
IF XINVNAME='Y'
  @ 06,05 SAY HLINE4
ENDIF

*B605205,1 Print factor on factor settings and invoice is factored. [Begin]
*IF PRINTFCTR
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
*B605205,1 Print factor on factor settings and invoice is factored. [End]

  @ 06,50 SAY XFADDR1
ENDIF

* LINE 7
*B804211,1 AME [Start] Print the phone one line below and print fax with its line.
*B804211,1 AME         and Shift the invhdr phone one line below.
*IF XINVNAME='Y'
*  @ 07,05 SAY XPHONE SIZE 1,16
*ENDIF
IF XINVNAME='Y'
  @ 07,05 SAY HLINE5
ENDIF
*B804211,1 AME [End]

*B605205,1 Print factor on factor settings and invoice is factored. [Begin]
*IF PRINTFCTR
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
*B605205,1 Print factor on factor settings and invoice is factored. [End]

  @ 07,50 SAY XFADDR2
ENDIF

* LINE 8
*B804211,1 AME [Start] shift the invhdr phone one line below.

*B804510,1 Remove the invoice phone, commented out. [Begin]
*IF XINVNAME='Y'
*  @ 08,05 SAY XPHONE SIZE 1,16
*ENDIF
*B804510,1 Remove the invoice phone, commented out. [End]

*B804211,1 AME [End]

*B605205,1 Print factor on factor settings and invoice is factored. [Begin]
*IF PRINTFCTR
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
*B605205,1 Print factor on factor settings and invoice is factored. [End]

  @ 08,50 SAY XFADDR3
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

*B803206,1 BWA 07/27/2000 Fix the bug of the line header doesn't print in the right postion.[START]
*SEEK XORDER
*B803206,1 [END]

XSTADDR1 = laShipTo[1] &&CADDRESS12
XSTADDR2 = laShipTo[2] &&laShipTo22
XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
IF LEN(TRIM(XSTADDR2)) =0
  XSTADDR2 = XSTADDR3
  XSTADDR3 = ''
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

*B803206,1 BWA 07/27/2000 Fix the bug of the line header doesn't print in the right postion.[SATRT]
*@ 17,09 SAY CUSTPO
@ 17,09 SAY LEFT(CUSTPO,11)
*B803206,1 [END]

*B606528,1 RAE Fix the bug of printing the name of DC instead of the Store name. [start]
*@ 17,21 SAY IIF(CURLOOP='1',IIF(!EMPTY(lcDist),lcDist,XSTORE),'')
@ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
*B606528,1 RAE [end]

@ 17,30 SAY DEPT
@ 17,36 SAY ORDER

*B803206,1 BWA 07/27/2000 Fix the bug of the line header doesn't print in the right postion.[SATRT]
*@ 17,44 SAY PTERMS
@ 17,44 SAY LEFT(PTERMS,15)
*B803206,1 [END]

@ 17,60 SAY REP1
@ 17,64 SAY REP2

*B803206,1 BWA 07/27/2000 Fix the bug of the line header doesn't print in the right postion.[SATRT]
*@ 17,67 SAY PSHIPVIA
@ 17,68 SAY LEFT(PSHIPVIA,12)
*B803206,1 [END]

*------------------
* PRINT SCALES
*------------------
lcScalStr = SPACE(0)
lcInvNo   = InvHDR.Invoice
DO lpGtScl WITH lcInvNo
SELECT Scale
Row = 19
FOR lnI = 1 TO LEN( lcScalStr )
  *B126091,1 NNA 01/13/2005 (BEGIN) Collecting the Scale Chrs. to seek with it
  *lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  *--I get lcScalStr like (A-B-C->If not extended OR AAA-BBB-CCC->IF Extended)
  *--so if the current chr.<>'-' that mean I'm with the same scale and adding this chr. to 
  *--the scale's String. but if the current chr.='-' that mean I got the all string for the scale
  *--and I can now searhing with it to get its sizes.
  IF SUBSTR( lcScalStr, lnI, 1 )<>'-' 
    lcSizeScal = lcSizeScal+SUBSTR( lcScalStr, lnI, 1 )
    llCScale  = .F.     
  ELSE
    llCScale  = .T.     
  ENDIF
  *--if llCScale is true that mean I have the complete chrs. for the scale.
  *--but in the last chr. llCScale will be false so i'm using the second condition.
  IF llCScale OR lnI=LEN( lcScalStr )
  *B126091,1 NNA (END)

    SEEK 'S' + lcSizeScal
    @ Row,22 SAY lcSizeScal

    @ Row,24 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ1),1,3),3,' ')
    @ Row,28 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ2),1,3),3,' ')
    @ Row,32 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ3),1,3),3,' ')
    @ Row,36 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ4),1,3),3,' ')
    @ Row,40 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ5),1,3),3,' ')
    @ Row,44 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ6),1,3),3,' ')
    @ Row,48 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ7),1,3),3,' ')
    @ Row,52 SAY PADL(SUBSTR(ALLTRIM(SCALE->SZ8),1,3),3,' ')

    *B126091,1 NNA (BEGIN) Empty the variable lcSizeScal
    lcSizeScal = '' 
    *B126091,1 NNA (END)

    Row = Row + 1

    *B126091,1 NNA (BEGIN)       
    ENDIF
    *B126091,1 NNA (END)
    
ENDFOR

Row = 25          && NEW LINE in the function

*B803023,1 BWA 08/02/2000 [END]