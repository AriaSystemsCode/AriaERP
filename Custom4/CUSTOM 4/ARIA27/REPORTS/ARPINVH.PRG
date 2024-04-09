*:************************************************************************
*: Program file  : ARPINVH.PRG
*:
*:         System: ARIA 2.7
*:         Module: Accounts Recevible
*:         Author: Hossam El Etreby
*:      Copyright (c) 
*:  Procs & Fncts: gfGetZone()
*:  Documented    12/17/1998
*:  Modifications:
*:           HDM B801884,1 Incorrect Bill to and ship to addresses
*:B602953,1 BWA 05/31/1999 Adjust invoice form H due to the attached form. 
*:B602969,1 BWA 06/08/1999 Fix the bug of not printing the terms payment
*:                         and print the long description  
*:B802337,1 BWA 06/14/1999 Modify the long desc. to each line of the invoice
*:B802373,1 BWA 06/27/1999 Fix the bug of not printing the lines of the invoice
*:B802373,1                in another page and fix the bug of the notepad
*:B802608,1 KEN 10/18/1999 Do not print '0' if there are no invoiced qty for size
*:B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # ,
*:B802722,1                and if the Sales tax = 0 we don't print it and repostion 
*:B802722,1                the cartons and the total fields and print REF.Account from customer
*:B802797,1 BWA 11/18/1999 Fix the bug of not printing all the scales in the second page 
*:                         even if the style having different scales
*:B802642,1 ABD 12/02/1999 Fix bug in NotePad That if the line Begin With '*'  Don't Print It.
*:B802988,1 NAD 01/24/2000 1-Fix the bug of factor account number somtimes prints and other time does not.
*:                         2-Fixing the bug in the ship address if you have a large name for city ,
*:                         the zip code will never appear
*:B802987,1 HDM 01/24/2000 Fixing bug of printing notes lines even it has an *
*:B803203,1 BWA 06/21/2000 1-Fix the bug of shifting the CONTINUED line up equal to the number of lines of the invoice above the comments field box.
*:B803203,1                2-Fix the bug of moving the departement field 2 character right.
*:B803585,1 BWA 08/09/2000 Fix the bug of when print the invoices the prtflag doesn't change to Yes.
*:C102212,1 ADEL 03/25/20001 Add HST tax to all Invoice formats.
*:B802988,1 NAD 01/24/2000 (Begin) Declare a variable to hold if the order have alt ship address or not.
*:B804555,1 SSE 12/24/2001 Fix bug of shifting the custpo 3 characters to the right.
*:B606433,1 MMM 09/22/2003 Shift the Custpo 1 character to the right & 2 characters to the left & Shift the Store 1 character to the left .
*:************************************************************************
llAltShp=.F.
*:B802988,1 NAD 01/24/2000 (End)
XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*

*-- YMA 03/29/94 To indecate either we are dealing with 
*-- a canadian company or not.
llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)

NEWDOC = .T.
MAXROW = 47
*B801604,1 (Bebin) Initilize the distribution center var.
lcDist = ' '
*B801604,1 (End)

*G000000,1 TAK 07/20/95 Changed the way of printing the sku.
*--

IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

llNote = llRpInvNot
lnNotLine = 1

SELECT INVHDR
LINEUP = .F.
STORE TRIM(QCOMPANY)                                       TO HLINE1
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
*STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
STORE TRIM(laCompAdd[3])                                   TO HLINE4
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
*** MAIN LOOP
*ahmed
*SET DEVICE TO PRINT
SCAN FOR &lcASExp   WHILE INKEY() <> 32
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
      SELECT (INVHTEMP)
      SKIP
      LOOP
    ENDIF
    NEWDOC = .F.
    XORDER = ORDER
    *XPHONE    = laCompAdd[5]
    XPHONE    = laCompAdd[5]
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XORDER    = ORDER
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION
    
    *B802797,1 BWA 11/18/1999 Fix the bug of not printing all the scales[START]
    lcScale1 = ""
    lcScale2 = ""
    *B802797,1 BWA 11/18/1999 [END]
    *** GET THE BILL TO AND SHIP ADDRESS
    SELE CUSTOMER
    *B802988,1 NAD 01/24/2000 [BEGIN] Get the factor account from the customer file.
    lcFactAcct = IIF(SEEK('M'+XACCOUNT),FactAcct,'')
    *B802988,1 NAD 01/24/2000 [END]
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)
    
    *B500670,1 MFM 04/26/95 (Begin).
    *XSKU     = (SKU='Y')
    *B500670,1 MFM 04/26/95 (End).

*--HDM B801884,1 Incorrect Bill to and ship to addresses[start]


      XBTNAME  = BTNAME
      XBTADDR1 = CADDRESS1
      XBTADDR2 = CADDRESS2
      XBTADDR3 = TRIM(CADDRESS3) + ' ' +TRIM(CADDRESS4) + ' ' + CADDRESS5
       
    
    
      IF LEN(TRIM(XBTADDR2)) =0
         XBTADDR2 = XBTADDR3
         XBTADDR3 = ''
      ENDIF
      *** TAK 04/06/94
      SELE ORDHDR
      SEEK XORDER
      
      IF ALT_SHPTO
     
        XSTADDR1 = CADDRESS12
        XSTADDR2 = laShipTo22        
        XSTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52
       
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ELSE
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
      XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
      XSTADDR1 = CADDRESS12
      XSTADDR2 = CADDRESS22      
      XSTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52
      
      IF LEN(TRIM(XSTADDR2)) =0
        XSTADDR2 = XSTADDR3
        XSTADDR3 = ''
      ENDIF
    ENDIF
    ***
    ****** FIND THE INVOICE LINES
    SELECT INVLINE
    SEEK XINVOICE
    IF EOF()
      PRTERR = 2
    ENDIF
    *** GET THE DESCRIPTION ABOUT THE CODES
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
*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns #.[START]
*--BAS I comment this loop becuase we use global function get the factor in the main program.
*    PRINTFCTR = llPrnFact         && (XINVFACTOR='Y')
*    STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
*    DO WHILE PRINTFCTR
*      SELECT SYCFACT
*      SEEK INVHDR->CFACCODE
*      DO CASE
*        CASE EOF()
*          STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
*        CASE FOUND()
*          XFNAME  = CFACCOMP
*          XFADDR1 = CADDRESS1
*          XFADDR2 = CADDRESS2
*          XFADDR3 = TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5
*          IF CADDRESS2 = ' '
*            CADDRESS2 = CADDRESS3
*            CADDRESS3 = ' '
*          ENDIF
*      ENDCASE
*      EXIT
*    ENDDO
*B802722,1 BWA 10/24/1999 [END]

    CURLOOP = '1'
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
  ENDIF
  *** END NEWDOC

*** 2
  SELECT INVHDR
  *-----------------
  * START PRINT
  *-----------------
  
  IF XINVNAME='Y'
    *SELE CODE 
    *SEEK 'D' + INVHDR->DIVISION
    *HLINE1 = IIF(FOUND().AND. !EMPTY(DIVLNAME), DIVLNAME , QCOMPANY)

   *B802722,1 BWA 10/24/1999 Fix the bug to print REF. ACCOUNT[START]
   *@ 03,05 SAY HLINE1
   @ 01,05 SAY HLINE1
    SELE INVHDR
  ENDIF
  
  @ 01,60 SAY XINVOICE
  *B602953,1 BWA  Modify the field dimension due to the attached form. 
  *@ 01,71 SAY INVDATE        && MFM 01/02/95.
  @ 01,75 SAY INVDATE        
  *B602953,1 BWA [END]
  *@ 03,71 SAY SUBSTR( PDIVISION, 1, 14 )  && MFM 01/02/95.

  IF XINVNAME='Y'
    @ 02,05 SAY HLINE2    
  ENDIF

  * LINE 4
  *IF XINVNAME='Y'
    *@ 04,05 SAY HLINE2
  *ENDIF


  * LINE 5
  IF XINVNAME='Y'
    *@ 05,05 SAY HLINE3
    @ 03,05 SAY HLINE3    
  ENDIF

  *B602953,1 BWA  Modify the field dimension due to the attached form. 
  *@ 03,71 SAY SUBSTR( CDIVISION, 1, 14 )  && MFM 01/02/95.
  @ 03,75 SAY SUBSTR( CDIVISION, 1, 14 )  
  *B602953,1 BWA [END]
  
  IF XINVNAME='Y'
    @ 04,05 SAY HLINE4
  ENDIF
  
  * LINE 6
  *IF XINVNAME='Y'
    *@ 06,05 SAY HLINE4
  *ENDIF

  * LINE 7
  IF XINVNAME='Y'
    *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
    *@ 07,05 SAY HLINE5  PICTURE "@R "+gcPhnFrmt SIZE 1,16
    *@ 07,05 SAY XPHONE  SIZE 1,16
     @ 05,05 SAY XPHONE  SIZE 1,16     
    *@ 07,05 SAY HLINE5
    *B500772,1 HISH 06/29/95.  ( End )
  ENDIF
  *B802722,1 BWA 10/24/1999 [END]
 
  *B602953,1 BWA  Modify the field dimension due to the attached form.   
  *@ 05,71 SAY ORDER    && MFM 01/02/95.
  @ 05,75 SAY ORDER    
  *B602953,1 BWA  [END]

  IF XINVNAME='Y' AND !EMPTY(lcDunsNo)
    @ 06,05 SAY 'Duns# :'
    @ 06,13 SAY lcDunsNo
  ENDIF

    *--HDM B801884,1 Incorrect Bill to and ship to addresses[start]

    =lfSolSpAdr()
    XBTNAME  = lcSolTName
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
    *[Start]
    *B802988,1 NAD 01/25/2000 Add address4 ,address5 to the ship to address  
    *XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
  
    llAltShp = OrdHdr.Alt_ShpTo
    IF llAltShp 
      XSTADDR3 = TRIM(laShipTo[3]) 
      XSTADDR4 = TRIM(laShipTo[4])
      XSTADDR5 = TRIM(laShipTo[5])
    ELSE     
      XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
   ENDIF 
   *B802988 ,1 NAD 01/25/2000  [END]
   
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      *[Start]
      *B802988 ,1 NAD 01/25/2000 shift address3, address4 ,address5 if address2 was empty 
      *XSTADDR3 = ''
      IF llAltShp    
        XSTADDR3 = XSTADDR4
        XSTADDR4 = XSTADDR5
        XSTADDR5 = ''
      ELSE
        XSTADDR3 = ''
      ENDIF
      *B802988 ,1 NAD 01/25/2000 [End]
    ENDIF

    *--HDM B801884,1  Incorrect Bill to and ship to addresses[END]
  SELECT INVHDR
  
  *B602953,1 BWA  Modify the field dimension due to the attached form. 
  *@ 07,71 SAY PIKTKT        && MFM 01/02/95.
  *@ 09,71 SAY APPROVAL      && MFM 01/02/95.
  
  *B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    @ 07,14 SAY "Remit to :"
    @ 07,25 SAY lcFacName 
  ENDIF  
  
  @ 07,75 SAY PIKTKT        
 
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    @ 08,25 SAY laFactor[1]
  ENDIF  
  *B802722,1 BWA 10/24/1999 [END]
  *B602953,1 BWA  [END]
  
  *B500627,1 TMI 04/05/95 (Start) Moving next lines 2 Char. to left.
  *B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    @ 09,25 SAY laFactor[2]
  ENDIF
  *B802722,1 BWA 10/24/1999 [END]
        
  @ 09,75 SAY APPROVAL    
  
  *B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]  
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
    @ 10,25 SAY laFactor[3]
  ENDIF
  *B802722,1 BWA 10/24/1999 [END]
    
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)    
    @ 11,25 SAY LEFT(laFactor[4] + IIF(!EMPTY(laFactor[4]),' ' + LEFT(laFactor[5],23) ,laFactor[5]),35)
  ENDIF
  
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    @ 12,25 SAY "Ref. Account:"
    *B802988 ,1 NAD 01/24/2000 [BEGIN] Get the factor account from the customer file.
    *@ 12,39 SAY CUSTOMER.FACTACCT
    @ 12,39 SAY lcFactAcct
    *B802988 ,1 NAD 01/24/2000 [END]
  ENDIF  
  *B802722,1 BWA 10/24/1999 [END]
  
  *B602953,1 BWA  Modify the field dimension due to the attached form. 
  *@ 13,08 SAY XBTNAME
  *@ 13,54 SAY XSTNAME
  *@ 14,08 SAY XBTADDR1
  *@ 14,54 SAY XSTADDR1
  *@ 15,08 SAY XBTADDR2
  *@ 15,54 SAY XSTADDR2
  *@ 16,08 SAY XBTADDR3
  *@ 16,54 SAY XSTADDR3
  
  @ 13,06 SAY XBTNAME
  @ 13,56 SAY XSTNAME
  @ 14,06 SAY XBTADDR1
  @ 14,56 SAY SUBSTR(XSTADDR1,1,25)
  @ 15,06 SAY XBTADDR2
  @ 15,56 SAY SUBSTR(XSTADDR2,1,25)
  @ 16,06 SAY SUBSTR(XBTADDR3,1,45)
  @ 16,56 SAY SUBSTR(XSTADDR3,1,29)
 *[Start]
 *B802988 ,1  NAD 01/25/2000 Print address4 ,address5 
  IF llAltShp
    @ 17,56 SAY ALLTRIM(SUBSTR(XSTADDR4,1,29))
    @ 18,56 SAY ALLTRIM(SUBSTR(XSTADDR5,1,29))
  ENDIF
  *B802988 ,1 NAD 01/25/2000  [End]
  *B602953,1 BWA [END]
  *B500627,1 TMI 04/05/95 (End) .
  @ 21,02 SAY ACCOUNT
  
  *B602953,1 BWA  Modify the field dimension due to the attached form. 
  *@ 21,12 SAY CUSTPO
  
  *B804555,1 Shift the Custpo 3 characters to the right. [Begin]
  *@ 21,15 SAY LEFT(CUSTPO,10)
  *B606433, 1 MMM 09/22/2003 Shift the Custpo 1 character to the right & 2 characters to the left. [Begin]
  *@ 21,12 SAY LEFT(CUSTPO,10)
  @ 21,11 SAY LEFT(CUSTPO,12)
  *B606433, 1 MMM 09/22/2003 Shift the Custpo 1 character to the right & 2 characters to the left. [End]
  *B804555,1 Shift the Custpo 3 characters to the right. [End]
 
  *B602953,1 BWA  [END]
  
  *B801604,1 (Bebin) Say the DC if there is one.
  *REN
 
   *B606433, 1 MMM 09/22/2003 Shift the Store 1 character to the left . [Begin]
  *@ 21,23 SAY STORE
   @ 21,24 SAY STORE
  *B606433, 1 MMM 09/22/2003 Shift the Store 1 character to the left . [End]
  *@21,23 SAY IIF(!EMPTY(lcDist),lcDist,STORE)
  *REN end
  *B801604,1 (End)
  
  *B803203,1 BWA 06/21/2000 2-Fix the bug of moving the departement field 2 character right. [START]
  *@ 21,33 SAY DEPT
  @ 21,35 SAY DEPT  
  *B803203,1 [END]


  *B602953,1 BWA  Modify the field dimension due to the attached form.   
  *@ 21,42 SAY PTERMS
  *B602969,1 BWA Fix the printing of the pterms 
  *@ 21,40 SAY SUBSTR(PTERMS,21)
  @ 21,41 SAY LEFT(PTERMS,21)
  *B602969,1 BWA [END]
  *B602953,1 BWA  [END]
  
  *-- MFM 01/02/95.
  
  *B602953,1 BWA  Modify the field dimension due to the attached form.   
  *@ 21,62 SAY REP1                         &&DM03/16/92 63->62
  *@ 21,66 SAY REP2
  @ 21,64 SAY REP1                         
  @ 21,68 SAY REP2
  *B602953,1 BWA  [END]
  
  *-- END MFM 01/02/95.  
  
  @ 21,72 SAY SUBSTR(PSHIPVIA,1,13)

  * PRINT SCALES
  ROW = 23

  select invline

  IF !XNOTE_LOOP
    trec1=RECNO()
    tscale1=" "
    tscale2=" "
    do while .T.
      if invoice<>xinvoice .or. (tscale1<>" " .and. tscale2<>" ")
        exit
      endif
      if tscale1=" "
        tscale1=scale
      endif
      if scale<>tscale1
        tscale2=scale
      endif
      skip
    enddo
    *--HDM
    SELECT INVLINE
    *--HDM    
    *ahmed
    *IF !EOF()
      *goto trec1
    *ENDIF
    SEEK XINVOICE
  ENDIF                                                 
  
  SELECT SCALE
  trec1=recno()
  i=49
  Y=' '
  do while i < 51
    ti=chr(i)
    SEEK 'S'+TSCALE&TI                    
    if !eof()
      *B500670,1 MFM 04/26/95 (Begin) Added the 'ALLTRIM'.
      
      *B602953,1 BWA  Modify the field dimension due to the attached form.       
      *@ ROW,29 SAY SCALE+Y+PADL(SUBSTR(ALLTRIM(SZ1),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ2),1,3),3,' ')+Y+;
                           PADL(SUBSTR(ALLTRIM(SZ3),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ4),1,3),3,' ')+Y+;
                           PADL(SUBSTR(ALLTRIM(SZ5),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ6),1,3),3,' ')+Y+;
                           PADL(SUBSTR(ALLTRIM(SZ7),1,3),3,' ')+Y+PADL(SUBSTR(ALLTRIM(SZ8),1,3),3,' ')   &&TAK 11/29/94
                           
      @ ROW,20 SAY SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
                           PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
                           PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')+;
                           PADC(SUBSTR(ALLTRIM(SZ7),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ8),1,3),5,' ')   
      *B602953,1 BWA  [END]
     
      *B500670,1 MFM 04/26/95 (End).
      row=row+1
    endif
    
    *B802797,1 BWA 11/18/1999 Fix the bug of not printing all the scales[START]
    IF EMPTY(lcScale1)
      lcScale1  =  SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
                   PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
                   PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')+;
                   PADC(SUBSTR(ALLTRIM(SZ7),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ8),1,3),5,' ')   
    ELSE
      lcScale2  =  SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
                   PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
                   PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')+;
                   PADC(SUBSTR(ALLTRIM(SZ7),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ8),1,3),5,' ')   
    ENDIF
    *B802797,1 BWA 11/18/1999 [END]
    
    i=i+1
  enddo
  *--HDM
  SELECT INVLINE
  *--HDM    
  *ahmed
  *IF !EOF()
    *goto trec1
  *ENDIF
  SEEK XINVOICE  
  
  CNT = 1
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 27
   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP           
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
      *B802373,1 BWA 06/27/1999 Fix the bug of not printing the lines of the invoice
         IF ROW >= MAXROW      && if there is a line will de delete from 
           
           =lfGetcont()        && the memo field
           =lfGetInHdr()
           LOOP
         ENDIF
        EXIT         
      *B802373,1 BWA 06/27/1999 [END]
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF
      KEY = INVLINE->STYLE           &&+ INVLINE->COLOR
      SELECT STYLE
      SEEK KEY
      XSTYDESC = IIF(FOUND() , SUBSTR(DESC,1,17) ,'')

      SELECT INVLINE
      *-- MFM 01/02/95.
      *-- Indinting the printing according to the sended form.
      
      *B602953,1 BWA  Modify the field dimension due to the attached form.       
      *@ ROW,00 SAY SUBSTR(STYLE,1,7)
      *@ ROW,13 SAY SUBSTR(STYLE,13,18)
      @ ROW,02 SAY SUBSTR(STYLE,1,10)
      @ ROW,14 SAY SUBSTR(STYLE,14,18)
       
      *@ ROW,07 SAY SUBSTR(COLOR,1,3)  && from the beginning
      *@ ROW,11 SAY XSTYDESC   && comment by BWA
      
      *@ ROW,29 SAY STYLE->SCALE
      *@ ROW,31 SAY QTY1   PICTURE '@Z 9999'
      *@ ROW,35 SAY QTY2   PICTURE '@Z 999'
      *@ ROW,39 SAY QTY3   PICTURE '@Z 999'
      *@ ROW,43 SAY QTY4   PICTURE '@Z 999'      
      *@ ROW,46 SAY QTY5   PICTURE '@Z 999'
      *@ ROW,50 SAY QTY6   PICTURE '@Z 999'
      *@ ROW,54 SAY QTY7   PICTURE '@Z 999'
      *@ ROW,58 SAY QTY8   PICTURE '@Z 999'
      
      @ ROW,20 SAY STYLE->SCALE      
      *B802608,1 KEN Do not print '0' if there are no invoiced qty for size
      *@ ROW,22 SAY PADC(QTY1,5,' ')   PICTURE '@Z 99999'
      *@ ROW,27 SAY PADC(QTY2,5,' ')   PICTURE '@Z 99999'
      *@ ROW,32 SAY PADC(QTY3,5,' ')   PICTURE '@Z 99999'
      *@ ROW,37 SAY PADC(QTY4,5,' ')   PICTURE '@Z 99999'
      *@ ROW,42 SAY PADC(QTY5,5,' ')   PICTURE '@Z 99999'
      *@ ROW,47 SAY PADC(QTY6,5,' ')   PICTURE '@Z 99999'
      *@ ROW,52 SAY PADC(QTY7,5,' ')   PICTURE '@Z 99999'
      *@ ROW,57 SAY PADC(QTY8,5,' ')   PICTURE '@Z 99999'
      @ ROW,22 SAY IIF(QTY1=0,'',PADC(QTY1,5,' '))   PICTURE '99999'
      @ ROW,27 SAY IIF(QTY2=0,'',PADC(QTY2,5,' '))   PICTURE '99999'
      @ ROW,32 SAY IIF(QTY3=0,'',PADC(QTY3,5,' '))   PICTURE '99999'
      @ ROW,37 SAY IIF(QTY4=0,'',PADC(QTY4,5,' '))   PICTURE '99999'
      @ ROW,42 SAY IIF(QTY5=0,'',PADC(QTY5,5,' '))   PICTURE '99999'
      @ ROW,47 SAY IIF(QTY6=0,'',PADC(QTY6,5,' '))   PICTURE '99999'
      @ ROW,52 SAY IIF(QTY7=0,'',PADC(QTY7,5,' '))   PICTURE '99999'
      @ ROW,57 SAY IIF(QTY8=0,'',PADC(QTY8,5,' '))   PICTURE '99999'
      *B802608,1 KEN [END]
        
      *B602953,1 BWA  [END]
      @ ROW,61 SAY TOTQTY PICTURE '9999'

      *-- END MFM 01/02/95.

      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      *B,500627,1 TMI 04/05/95 (Start) Moving next line 1 Char. to left.
      @ ROW,66  SAY PRICE     PICTURE '9999.99'
      *B,500627,1 TMI 04/05/95 (End)
      @ ROW,75  SAY XLINETOT  PICTURE '999999.99'
      ROW = ROW+1
      *B802337,1 BWA Modify the long desc. to each line of the invoice
      *@ ROW,02  SAY "Long Desc : "
      @ ROW,02  SAY  INVLINE.dESC1
      ROW = ROW+1
      *B802337,1 BWA [END]
      
      *G000000,1 TAK 07/20/95 Start.
      *Changed the way of printing the sku.
      DO lpPrtSku      &&Procedure to print the Style/Color Sku no.
      *Marked all the old way of printing the Sku.

      **** IF THIS ACCOUNT HAS ANY SKU # THEN SEEK IN SKU FILE FOR THIS STYLE/CLR
      *B500670,1 MFM 04/26/95 (Begin) Print the sku information.
*      IF SEEK('S'+INVLINE->ACCOUNT+INVLINE->STYLE+INVLINE->COLOR,'SPCK_LIN')
*        SELECT SPCK_LIN
*        IF !EMPTY(SKU)
*          @ ROW,00 SAY SKU
*          @ ROW,16 SAY 'CUSTOMER SKU #'
*          ROW=ROW+1
*        ELSE
*          lnI = 1
*          =SEEK('S'+Style.Scale,'Scale')
*         DO WHILE Style = InvLine.STYLE
*            lcStrToPrn = 'SKU N#' + SUBSTR(Pack_Id,1,8) + ' '
*           DO WHILE Style = InvLine.STYLE .AND. !EOF()
*              lcI = STR(lnI,1)
*              lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+;
*                                        SUBSTR(Pack_Id,9,16) + ' '
*              lnI = lnI + 1
*              SKIP
*              IF lnI = 5
*                EXIT
*              ENDIF
*            ENDDO
*            @ ROW,00 SAY lcStrToPrn
*            ROW = ROW + 1
*          ENDDO  
*        ENDIF
*      ENDIF
        
      *  SELE SKU
      *  SEEK INVLINE->ACCOUNT+INVLINE->STYLE+INVLINE->COLOR
      *  IF FOUND() .AND. (!EMPTY(SKU))
      *    @ ROW,00 SAY SKU
      *    *-- MFM 01/02/95.
      *    @ ROW,16 SAY 'CUSTOMER SKU #'
      *    ROW=ROW+1
      *  ENDIF
      *ENDIF
      *B500670,1 MFM 04/26/95 (End).
      
*      SELE INVLINE

*      IF LINEUP
*         EXIT
*      ENDIF

      * GET NEXT LINE ITEM
      SELECT INVLINE

      *B802987,1 [START] Print Line Notes
      IF llRpInvLNt
        @ROW,2 SAY '* -- LINE NOTES -- *'
        FOR lnLINLoop = 1 TO MEMLINES(NOTE_MEM)
          lcCurrLine = ALLTRIM(MLINE(Note_Mem,lnLINLoop))
          IF LEFT(lcCurrLine,1) <> '*'
            ROW = ROW + 1

            *B803203,1 BWA 06/21/2000 1-Fix the bug of shifting the CONTINUED line up equale to the number of lines of the invoice above the comments field box.[START]
            IF ROW >= MAXROW
              =lfGetcont()
              =lfGetInHdr()
              SELECT INVLINE
            ENDIF
            *B803203,1 [END]

            @ ROW,2 SAY lcCurrLine
          ENDIF
        ENDFOR
        ROW = ROW + 1
      ENDIF
      *B802987,1 [end]

      SKIP
   ENDDO

   *** END LINE PROCESSING
   *SET DEVICE TO PRINT
   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
   
     *B803203,1 BWA 06/21/2000 1-Fix the bug of shifting the CONTINUED line up equale to the number of lines of the invoice above the comments field box.[START]
      ROW = 49
     *B803203,1 [END]
      @ ROW+1,12 SAY 'C O N T I N U E D ...'

      *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
      *B802337,1 BWA 06/14/1999 Modify the phone format
      *@ 50,07 SAY INVHDR->PHONE  PICTURE "@R "+lcPhonPict SIZE 1,16
      @ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
      *B802337,1 BWA [END]
      *@ 50,07 SAY INVHDR->PHONE
      *B500772,1 HISH 06/29/95.  ( End )

      SELECT INVHDR
      @ 50,56 SAY 'MERCHANDISE'
      @ 50,75 SAY '**********'
      ROW=ROW+1
      
      *:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
      *@ 51,07 SAY INVHDR->NOTE1
      *@ 53,07 SAY INVHDR->NOTE2
      @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
      @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
      *:B802642,1 ABD 12/02/1999 [ End ]
     
     *B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]      
     *@ 58,18 SAY INVHDR->CARTONS                   PICTURE '@Z 999'
     *@ 58,62 SAY '******'
     *@ 58,75 SAY '******'
      
      @ 56,16 SAY INVHDR->CARTONS                   PICTURE '@Z 999'
      @ 56,65 SAY '*******'
      @ 56,75 SAY '**********'
     *B802722,1 BWA 10/24/1999 [END]
     
      ROW = 1
      *B602953,1 BWA [END]
      LOOP
   ENDIF
   
IF llNote
   SELECT NOTEPAD
   
   *E100207,1 YMA 03/30/95 (Begin).
   lnOldMemW = SET("MEMOWIDTH")
   SET MEMOWIDTH TO 75
   IF TYPE + KEY <> 'C' + XINVOICE
     SEEK 'C' + XINVOICE
     lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   ENDIF
   IF TYPE + KEY = 'C' + XINVOICE
     @ ROW,02 SAY '* -- N O T E S -- *' 
     ROW = ROW + 1 

     *SCAN WHILE TYPE + KEY = 'C' + XINVOICE
     
     *B802373,1 BWA 06/27/1999 Fix the bug of not printing the notepad in the second time preview
     *DO WHILE lnNotLine <= lnMemLins
     DO WHILE lnNotLine <= MEMLINES(NOTEPAD.MNOTES)
     *B802373,1 BWA 06/27/1999 [END]
       IF ROW >= MAXROW
         XNOTE_LOOP = .T.
       *B602953,1 BWA 05/31/1999 Adjust invoice form H due to the attached form.          
         *EXIT
       ELSE
         XNOTE_LOOP = .F.
         *@ ROW,02 SAY MLINE(MNOTES,lnNotLine)
         *B802642,1  if the line Begin With '*'  Don't Print It. [ Begin ]
         *@ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
         *ROW = ROW + 1
         lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
         *B802987,1 [start] check the alltrim of the notepad
         *IF  LEFT(lcCurrLine,1) # '*'
         IF  LEFT(ALLTRIM(lcCurrLine),1) # '*'
         *B802987,1 [end]
           @ ROW,02 SAY lcCurrLine
           ROW = ROW + 1
         ENDIF
        *B802642,1 Abd [ End ]
       ENDIF
       IF ROW >= MAXROW      && if there is a line will de delete from 
         =lfGetcont()        && the memo field
         =lfGetInHdr()
       ENDIF
       *B602953,1 BWA 05/31/1999 [END]
       lnNotLine = lnNotLine + 1
     ENDDO
     
     IF !XNOTE_LOOP
       @ ROW,02 SAY '* -- END OF NOTES -- *'
       lnNotLine = 1
       ROW = ROW + 1 
     ELSE
       *B803203,1 BWA 06/21/2000 1-Fix the bug of shifting the CONTINUED line up equale to the number of lines of the invoice above the comments field box.[START]
       ROW = 49
       *B803203,1 [END]
     
       @ ROW+1,12 SAY 'C O N T I N U E D ...'

       *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
       *@ 50,07 SAY INVHDR->PHONE  PICTURE "@R "+lcPhonPict SIZE 1,16
       @ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
       *@ 50,07 SAY INVHDR->PHONE
       *B500772,1 HISH 06/29/95.  ( End )

       SELECT INVHDR
       @ 50,56 SAY 'MERCHANDISE'
       @ 50,75 SAY '**********'
       ROW = ROW + 1
       
       *:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
       *@ 51,07 SAY INVHDR->NOTE1
       *@ 53,07 SAY INVHDR->NOTE2
       @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
       @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
       *:B802642,1 ABD 12/02/1999 [ End ]

     *B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]      
     *@ 58,18 SAY INVHDR->CARTONS PICTURE '@Z 999'
     *@ 58,62 SAY '******'
     *@ 58,75 SAY '******'
      
     @ 56,16 SAY INVHDR->CARTONS PICTURE '@Z 999'
     @ 56,65 SAY '*******'
     @ 56,75 SAY '**********'
     *B802722,1 BWA 10/24/1999 [END]
     
     *B602953,1 BWA 05/31/1999 Adjust invoice form H due to the attached form.
       ROW = 1
       *B602953,1 BWA [END]
       LOOP
     ENDIF
   ENDIF

   SET MEMOWIDTH TO lnOldMemW
   *E100207,1 YMA 03/30/95 (End).
ENDIF

   *B500772,1 HISH 06/29/95.  ( Begin )   Display phone with new format.
   *@ 50,07 SAY INVHDR->PHONE  PICTURE "@R "+lcPhonPict SIZE 1,16
   @ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
   *@ 50,07 SAY INVHDR->PHONE
   *B500772,1 HISH 06/29/95.  ( End )

   SELECT INVHDR
   @ 50,56 SAY 'MERCHANDISE'
   @ 50,74 SAY XSUBTOTAL            PICTURE '9999999.99'  

   ROW=51
   
   *:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
   *@ 51,07 SAY INVHDR->NOTE1
   @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
   *:B802642,1 ABD 12/02/1999 [ End ]

   
   
   *B802722,1 BWA 10/24/1999 Fix the bug of the Sales tax = 0  we don't print it[START]
   *IF XTAX .AND. XTAX_METH = 'M' 
   IF XTAX .AND. XTAX_METH = 'M' .AND. INVHDR.TAX_AMT # 0
     *-- TMI 01/17/95
     IF !EMPTY(lcTaxRefr)
       @ Row,55 SAY lcTaxRefr
       Row = Row + 1
     ENDIF  
     *-- End TMI 01/17/95
     XSTRING_RATE = ALLTRIM(STR (INVHDR->TAX_RATE,5,2))
     @ ROW ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
     @ ROW ,74 SAY INVHDR->TAX_AMT   PICT '9999999.99'
     ROW = ROW + 1
   ENDIF  
   *B802722,1 BWA 10/24/1999 [END]

   *-- TMI 01/17/95

   IF ROW = 53
     *:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
     *@ 53,07 SAY INVHDR->NOTE2
     *B802987,1 [start] note2 has to be printed in row 53 not 51
     *@ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
     @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
     *B802987,1 [end]
     *:B802642,1 ABD 12/02/1999 [ End ]

     
     llNotePrnt = .T.
   ELSE
     llNotePrnt = .F.  
   ENDIF  
   *-- End TMI 01/17/95

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,55 SAY 'TOTAL - FREIGHT'
      @ ROW,74 SAY WKAMT       PICTURE '9999999.99'  
      ROW=ROW+1
   ENDIF

   IF DISCOUNT<>0
      @ ROW,55 SAY 'DISCOUNT'
      @ ROW,73 SAY DISCOUNT    PICTURE  '99999999.99'   
   ENDIF

   *-- TMI 01/17/95
   IF !llNotePrnt
     *:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
     *@ 53,07 SAY INVHDR->NOTE2
     *B802987,1 [start] note2 has to be printed in row 53 not 51
     *@ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
     @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
     *B802987,1 [end]
     *:B802642,1 ABD 12/02/1999 [ End ]
   ENDIF
   *-- End TMI 01/17/95

  *B602953,1 BWA  Modify the field dimension due to the attached form. 
  * IF XTAX .AND. XTAX_METH = 'A'
  *   @ 54,55 SAY lcTaxRefr                  && TMI 01/17/95
  *   XSTRING_RATE = ALLTRIM(STR (INVHDR->TAX_RATE,5,2))
  *   @ 55 ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
  *   @ 55 ,74 SAY INVHDR->TAX_AMT   PICT '9999999.99'
  * ENDIF  
  
   *B802722,1 BWA 10/24/1999 Fix the bug of the Sales tax = 0  we don't print it[START]  
   *IF XTAX .AND. XTAX_METH = 'A'
   IF XTAX .AND. XTAX_METH = 'A' .AND. INVHDR.TAX_AMT # 0
     @ 53,55 SAY lcTaxRefr                  && TMI 01/17/95
     XSTRING_RATE = ALLTRIM(STR (INVHDR->TAX_RATE,5,2))
     @ 54 ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
     @ 54 ,74 SAY INVHDR->TAX_AMT   PICT '9999999.99'
   ENDIF  
   *B802722,1 BWA 10/24/1999 [END]

  *B602953,1 BWA  [END]

   *-- YMA 03/29/94 Print the PST tax rate and amount.
   IF xTAx .AND. llIsCanada 
     lcStrRate = ALLTRIM(STR(InvHdr->nPstRate,5,2))
     
     *B602953,1 BWA  Modify the field dimension due to the attached form. 
     *lnRow = IIF (xTAx_Meth='A',56,55)
     lnRow = IIF (xTAx_Meth='A',55,54)
     *B602953,1 BWA  [END]
     
     @ lnRow ,55 SAY 'PST TAX    :' + lcStrRate + ' %'
     @ lnRow ,74 SAY InvHdr->nPstAmt PICTURE '9999999.99'
     *C102212,1 (Begin) Add HST Tax.
     IF InvHdr.nHSTAmt <> 0
       lnRow = lnRow + 1
       @ lnRow,55 SAY 'HST TAX    :' + STR(InvHdr.nHSTRate,5,2)+' %'
       @ lnRow,74 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
     ENDIF  
     *C102212,1 (End)
   ENDIF
   
   *B602953,1 BWA  Modify the field dimension due to the attached form. 
   *B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]
   *@ 58,18 SAY INVHDR->CARTONS   PICTURE '@Z 999'
   *@ 58,62 SAY XPIECES           PICTURE '9999999'
   *@ 58,74 SAY INVHDR->TOTALCHG  PICTURE '9999999.99'
   
   @ 56,18 SAY INVHDR->CARTONS   PICTURE '@Z 999'
   @ 56,65 SAY XPIECES           PICTURE '9999999'
   @ 56,74 SAY INVHDR->TOTALCHG  PICTURE '9999999.99'
   *B802722,1 BWA 10/24/1999 [END]
   *B602953,1 BWA  [END]
   *SELECT INVHDR 
   *REPLACE PRTFLAG WITH 'P'
   *-- GET NEXT HEADER RECORD
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

*!*************************************************************
*! Name : gfGetZone.
*! Auth : Tarek Mohamed Ismael (TMI).
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


*!***************************************************************
*! Name : lpPrtSku.
*! Auth : Timour Abdalla Khalil.
*! Date : 07/20/95.
*! 컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
*! Synopsis : Print the style/color Skus for a specific account.
*G000000,1 TAK 07/20/95.
*!***************************************************************
PROCEDURE lpPrtSku

*IF ! SEEK('S'+InvLine.Account+InvLine.Style+InvLine.Color,'Spck_Lin')
IF ! SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  RETURN
ENDIF

SELECT Spck_Lin
IF EMPTY(Sku)
  lnI = 1
  =SEEK('S'+Style.Scale,'Scale')
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF 

  DIME laSku[8]
  laSku = SPACE(16)
*  SCAN WHILE Type+Account+Style+Color = 'S'+InvLine.Account+InvLine.Style+InvLine.Color .AND. lnI < 9
   SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku(lnX)=SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN

  lnI = 1
*  = SEEK('S'+InvLine.Account+InvLine.Style+InvLine.Color,'Spck_Lin')
  = SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
*  DO WHILE Type+Account+Style+Color = 'S'+InvLine.Account+InvLine.Style+InvLine.Color .AND. lnI < 9
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    lcStrToPrn = 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
*    DO WHILE Type+Account+Style+Color = ;
*             'S'+InvLine.Account+InvLine.Style+InvLine.Color .AND. !EOF()
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style .AND. !EOF()

      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
    @ ROW,00 SAY lcStrToPrn
    ROW = ROW + 1
  ENDDO  
ELSE
  @ ROW,00 SAY Sku
  @ ROW,16 SAY 'CUSTOMER SKU #'
  ROW=ROW+1
ENDIF
RETURN

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 06/09/1999
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
*:Modifications:
*:B602962,1 BWA Collect all the header lines in a function to call
*:              this function from any where from the program.
*!*************************************************************
FUNCTION lfGetInHdr
PRIVATE lnSelFile , lcalias

lcAlias = ALIAS()
*B802722,1 BWA 10/24/1999 Fix the bug to print REF. ACCOUNT[START]
IF XINVNAME='Y'
  *@ 03,05 SAY HLINE1
  @ 01,05 SAY HLINE1
  SELE INVHDR
ENDIF

@ 01,60 SAY XINVOICE
@ 01,75 SAY INVDATE        

IF XINVNAME='Y'
    @ 02,05 SAY HLINE2    
ENDIF

* LINE 4
*IF XINVNAME='Y'
  *@ 04,05 SAY HLINE2
*ENDIF

* LINE 5
IF XINVNAME='Y'
    *@ 05,05 SAY HLINE3
    @ 03,05 SAY HLINE3    
ENDIF

@ 03,75 SAY SUBSTR( CDIVISION, 1, 14 )  

IF XINVNAME='Y'
    @ 04,05 SAY HLINE4
ENDIF
  
* LINE 6
*IF XINVNAME='Y'
    *@ 06,05 SAY HLINE4
*ENDIF

* LINE 7
IF XINVNAME='Y'
  *@ 07,05 SAY XPHONE  SIZE 1,16
  @ 05,05 SAY XPHONE  SIZE 1,16     
ENDIF

@ 05,75 SAY ORDER    

*B802722,1 BWA 10/24/1999 [END]
IF XINVNAME='Y' AND !EMPTY(lcDunsNo)  
  @ 06,05 SAY 'Duns# :'
  @ 06,13 SAY lcDunsNo
ENDIF

=lfSolSpAdr()
XBTNAME  = lcSolTName
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
*[Start]
*B802988 ,1 NAD  01/25/2000 Add  address4 ,address5 to the ship to section 
*XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
  
llAltShp=OrdHdr.Alt_ShpTo
IF llAltShp
  XSTADDR3 = TRIM(laShipTo[3]) 
  XSTADDR4 =TRIM(laShipTo[4])
  XSTADDR5 = TRIM(laShipTo[5])
  
ELSE     
  XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
 
ENDIF 
*B802988 ,1 NAD 01/25/2000  [End]
   
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      *[Start]
      *B802988 ,1  01/25/2000  NAD Shift address4 ,address5 if address2 was empty  
      *XSTADDR3 = ''
      IF llAltShp 
        XSTADDR3 = XSTADDR4
        XSTADDR4 = XSTADDR5
        XSTADDR5 = ''
      ELSE
        XSTADDR3 = ''
      ENDIF 
   *IF LEN(TRIM(laShipTo[2])) =0
     * XSTADDR2 = laShipTo[3]
     * XSTADDR3 = ''
   *ENDIF
   ENDIF
  *B802988 ,1 NAD 01/25/2000  [End]

SELECT INVHDR

*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 07,14 SAY "Remit to :"
  @ 07,25 SAY lcFacName 
ENDIF    
@ 07,75 SAY PIKTKT        
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 08,25 SAY laFactor[1]
ENDIF  
*B802722,1 BWA 10/24/1999 [END]

*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 09,25 SAY laFactor[2]
ENDIF
*B802722,1 BWA 10/24/1999 [END]

@ 09,75 SAY APPROVAL      

*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 10,25 SAY laFactor[3]
ENDIF
*B802722,1 BWA 10/24/1999 [END]

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 11,25 SAY LEFT(laFactor[4] + IIF(!EMPTY(laFactor[4]),' ' + LEFT(laFactor[5],23) ,laFactor[5]),35)
ENDIF

*B802722,1 BWA 10/24/1999 Fix the bug to print REF. ACCOUNT[START]
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 12,25 SAY "Ref. Account:"
  *B802988 ,1 NAD 01/24/2000 [BEGIN] Print the factor account from the customer file.
  *@ 12,39 SAY CUSTOMER.FACTACCT
  @  12,39 SAY lcFACTACCT
ENDIF
*B802988 ,1 NAD 01/24/2000 [END]
*B802722,1 BWA 10/24/1999 [END]
  
*B802722,1 BWA 10/24/1999 [END]
@ 13,06 SAY XBTNAME
@ 13,56 SAY XSTNAME
@ 14,06 SAY XBTADDR1
@ 14,56 SAY SUBSTR(XSTADDR1,1,25)
@ 15,06 SAY XBTADDR2
@ 15,56 SAY SUBSTR(XSTADDR2,1,25)
@ 16,06 SAY SUBSTR(XBTADDR3,1,45)
@ 16,56 SAY SUBSTR(XSTADDR3,1,29)
*[Start]
*B802988 ,1 NAD 01/25/2000 Print address4 ,address5 
IF llAltShp

  @ 17,56 SAY ALLTRIM(SUBSTR(XSTADDR4,1,29))
  
  @ 18,56 SAY ALLTRIM(SUBSTR(XSTADDR5,1,29))
ENDIF
*B802988 ,1 NAD 01/25/2000  [End]

@ 21,02 SAY ACCOUNT

*B804555,1 Shift the Custpo 3 characters to the right. [Begin]
*mmm
*@ 21,15 SAY LEFT(CUSTPO,10)
*@ 21,12 SAY LEFT(CUSTPO,10)
@ 21,11  SAY LEFT(CUSTPO,12)
* mmm
*B804555,1 Shift the Custpo 3 characters to the right. [End]

@ 21,23 SAY IIF(!EMPTY(lcDist),lcDist,STORE)

*B803203,1 BWA 06/21/2000 2-Fix the bug of moving the departement field 2 character right.[START]
*@ 21,33 SAY DEPT
@ 21,35 SAY DEPT
*B803203,1 [END]

@ 21,41 SAY LEFT(PTERMS,21)
@ 21,64 SAY REP1                         
@ 21,68 SAY REP2
@ 21,72 SAY SUBSTR(PSHIPVIA,1,13)

*B802722,1 BWA 10/24/1999 Fix the bug of print the factor information and the duns # [START]
ROW = 23
*B802797,1 BWA 11/18/1999 Fix the bug of not printing all the scales[START]
*lnSelFile =  SELECT(0)
*SELECT SCALE
*@ ROW,20 SAY SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
*                     PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
*                     PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')+;
*                     PADC(SUBSTR(ALLTRIM(SZ7),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ8),1,3),5,' ')   
*SELECT (lnSelFile)                     
@ 23,20 SAY lcScale1
@ 24,20 SAY lcScale2
*B802797,1 BWA 11/18/1999 [END]
*B802722,1 BWA 10/24/1999 [END]
ROW = 27

SELECT (lcalias)
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 06/09/1999
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
*:Modifications:
*:B602962,1 BWA Collect all the footer lines in a function to call
*:              this function from any where from the program.
*!*************************************************************
FUNCTION lfGetcont


*B803203,1 BWA 06/21/2000 1-Fix the bug of shifting the CONTINUED line up equale to the number of lines of the invoice above the comments field box.[START]
ROW = 49
*B803203,1 [END]

@ ROW+1,12 SAY 'C O N T I N U E D ...'
*B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]
*@ 50,07 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
@ 50,09 SAY INVHDR->PHONE  PICTURE lcPhonPict SIZE 1,16
*B802722,1 BWA 10/24/1999 [END]

SELECT INVHDR
@ 50,56 SAY 'MERCHANDISE'
@ 50,75 SAY '**********'
ROW = ROW + 1


*:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
*@ 51,07 SAY INVHDR->NOTE1
*@ 53,07 SAY INVHDR->NOTE2
@ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
@ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
*:B802642,1 ABD 12/02/1999 [ End ]


*B802722,1 BWA 10/24/1999 Fix the bug of repostion the cartons and the total fields [START]      
*@ 58,18 SAY INVHDR->CARTONS PICTURE '@Z 999'
*@ 58,62 SAY '******'
*@ 58,75 SAY '******'

@ 56,16 SAY INVHDR->CARTONS PICTURE '@Z 999'
@ 56,65 SAY '*******'
@ 56,75 SAY '**********'
*B802722,1 BWA 10/24/1999 [END]

