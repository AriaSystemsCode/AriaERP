****************************************************************************
*: Program file : ARPINVZ.PRG    
*: DESC : PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" (For YL)
*:        A copy from invoice form D with some changes.   
*: System: Aria2.7 
*: Module: Accounts Receivable
*: DATE : 09/01/99
*: Developer: Sherif Attala Ishak 
*C101577,1 Refer to.
*:************************************************************************
*: Calls : 
*:         Functions  : gfGetMemVar()
*:                    : gfGetZone()
*:************************************************************************
*: Modifications :
*:B803302,1 SHA Some modifications in the header of the form.
****************************************************************************

XNOTE_LOOP = .F.   && Flag to indicate whether we have finished
                   && printing the Notepad or not.

STORE "" TO ENDPAGE
llOpen = gfOpenFile(GCDATADIR+'SALESREP','SALESREP','SH')

XTAX      = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)  && TMI 01/17/95
RELE ALL LIKE M_*

IF EMPTY(laSettings)
  PRINTFCTR = .T.
ELSE
  PRINTFCTR = EVAL(laSettings[2,1])
ENDIF

gcPhnFrmt = lcPhonPict
llNote = llRpInvNot
NEWDOC = .T.
MAXROW = 50
lcDist = ' '

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

lnNotLine = 1

DIMENSION laScales(4)
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
MSG3 = lcRpMsg1
WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT
XINVNAME = lcPrnComp
gcPhnFrmt = lcPhonPict
SELECT INVHDR

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  IF llOpen
    USE IN SALESREP
  ENDIF
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

SCAN FOR &lcASExp

    XINVOICE = INVOICE
    *-- Get Customer DUNS[start]
    lnPrevAl = SELECT()
    SELECT CUSTOMER
    XDUNS = DUNS
    SELECT (lnPrevAl)
    *-- Get Customer DUNS[end]

   *-----------------------------------------------------------------
   * Get invoice header, line items, and financial history records.
   * If any records are not found, skip to next invoice.
   * Initialize document totals.
   *-----------------------------------------------------------------
   IF NEWDOC
      STORE SPACE(3) TO laScales
      PRTERR = 0
      STORE 0.00 TO XPIECES, XSUBTOTAL
      SELECT INVHDR
      NEWDOC    = .F.
      XORDER    = ORDER
      XPHONE    = laCompAdd[5]
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
    XBTNAME  = BTNAME
    XBTADDR1 = CADDRESS12
    XBTADDR2 = CADDRESS22
    XBTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52

      IF LEN(TRIM(XBTADDR2)) =0
         XBTADDR2 = XBTADDR3
         XBTADDR3 = ''
      ENDIF
      SELE ORDHDR
      IF ALT_SHPTO
        XSTNAME  = stName
        XSTADDR1 = CADDRESS1
        XSTADDR2 = CADDRESS2
        XSTADDR3 = TRIM(CADDRESS3) + ' ' +TRIM(CADDRESS4) + ' ' + CADDRESS5
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ELSE
        IF INVHDR->CONSOL = 'Y'       
          SELECT CONSINVH
          SEEK XINVOICE
          SELECT CUSTOMER
          SEEK 'S'+XACCOUNT+CONSINVH->STORE
        ENDIF
        SELE CUSTOMER
        IF !EMPTY(DIST_CTR)
          lcDist = DIST_CTR
          = SEEK('S'+XACCOUNT+lcDist)
        ENDIF
        XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
        XSTADDR1 = CADDRESS1
        XSTADDR2 = CADDRESS2
        XSTADDR3 = TRIM(CADDRESS3)+' '+TRIM(CADDRESS4)+ ' '+CADDRESS5 
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ENDIF
      ***
      ****** FIND THE INVOICE LINES
      ***
      SELECT INVLINE
      SEEK XINVOICE
      IF EOF()
         PRTERR = 2
      ENDIF
      *** GET THE DESCRIPTION ABOUT THE CODES
      
      SELECT CODES
      SET ORDER TO CODES IN CODES 
  
      PTERMS = gfCodDes(INVHDR->CTERMCODE,'CTERMCODE')
      PSHIPVIA = gfCodDes(INVHDR->SHIPVIA,'SHIPVIA')

      PSPCINST= gfCodDes(INVHDR->SPCINST,'SPCINST')
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
      STORE 1 TO lnCount
      SELECT INVLINE
      SCAN WHILE Invoice = xInvoice AND lnCount <= 4
        IF ASCAN(laScales,Scale) = 0 
          STORE Scale to laScales(lnCount)
          lnCount = lnCount + 1
        ENDIF
      ENDSCAN
      SEEK xInvoice
   ELSE
     GO lnRecNo IN INVLINE
   ENDIF
   ***
   ****** END NEWDOC
   ***
   *-----------------
   * START PRINT
   *-----------------
   * LINE 2
   =SEEK("M"+XACCOUNT,"CUSTOMER")
   @ 0,1 SAY "          SOYA FASHIONS INC., DBA:YL"
   @ 1,1 SAY "    201 PENHORN AVENUE, SECAUCUS, NJ 07094"
   
   *B803302,1 SHA(Begin)Changed the telephone and fax# 
   *@ 2,1 SAY "   TEL: (201) 601-0333  FAX: (201) 974-1821"
   *@ 3,1 SAY " CUST. SERV: 1(800)446-4689 FAX:(212)967-6069"
   @ 2,1 SAY  "   TEL: (212) 947-9333  EXT 63 CUSTOMER SERVICE"
   @ 3,1 SAY  "             FAX (212)967-3367"
   *B803302,1 SHA(End)
   
   @ 3,69 SAY "ORDER  #: "+ INVHDR.Order
   @ 4,1 SAY  "             DUNS: 10-330-4044"   
   @ 4,69 SAY IIF(!EMPTY(CUSTOMER.cCusVend),"VENDOR #: " + CUSTOMER.cCusVend,"")
   IF !EMPTY(INVHDR.cFacCode)
     *B803302,1 SHA(Begin)Changed the factor name
     *@ 5,1 SAY  " ---------------------------------------------------------------------------------- "
     *@ 6,1 SAY  "|THIS RECEIVABLE IS ASSIGNED TO, OWNED BY AND PAYABLE ONLY TO: REPUBLIC FACTORS    |"
     *@ 7,1 SAY  "|CORP. AT P.O. BOX  7777-W8720, PHILADELPHIA, PA 19175 OR DEPT. 49941, LOS ANGELES,|"
     *@ 8,1 SAY  "|CA 90088, WHICHEVER IS NEARER.  ANY OBJECTION TO THIS INVOICE MUST BE REPORTED TO |"
     *@ 9,1 SAY  "|REPUBLIC FACTORS CORP. AT 452 FIFTH AVE. 4TH FL. NEW YORK, NY 10018.  ALL OTHER   |"
     *@ 10,1 SAY "|CORESPONDENCE TO 201 PENHORN AVE., UNIT 1, SECAUCUS, NJ 07094.                    |"
     *@ 11,1 SAY " ---------------------------------------------------------------------------------- "

     @ 5,1 SAY  " ---------------------------------------------------------------------------------- "
     @ 6,1 SAY  "|THIS RECEIVABLE IS ASSIGNED TO, OWNED BY AND PAYABLE ONLY TO: HSBC BUSINESS CREDIT |"
     @ 7,1 SAY  "|(USA) INC. AT P.O. BOX  7777-W8720, PHILADELPHIA, PA 19175 OR DEPT. 49941, LOS     |"
     @ 8,1 SAY  "|ANGELES, CA 90088, WHICHEVER IS NEARER.  ANY OBJECTION TO THIS INVOICE MUST BE     |"
     @ 9,1 SAY  "|REPORTED TO HSBC BUSINESS CREDIT(USA) INC. AT 452 FIFTH AVE. 4TH FL. NEW YORK,     |"
     @ 10,1 SAY "|NY 10018.  ALL OTHER CORESPONDENCE TO 201 PENHORN AVE., UNIT 1, SECAUCUS, NJ 07094.|"
     @ 11,1 SAY " ---------------------------------------------------------------------------------- "
     *B803302,1 SHA(End)
   ENDIF
   * LINE 4 
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
    XSTADDR1 = laShipTo[1] &&CADDRESS12
    XSTADDR2 = laShipTo[2] &&laShipTo22
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
    IF LEN(TRIM(XSTADDR2)) =0
      XSTADDR2 = XSTADDR3
      XSTADDR3 = ''
    ENDIF

   @ 12,04 SAY XBTNAME
   IF CURLOOP = '1'
      @ 12,48 SAY XSTNAME
   ENDIF

   @ 13,04 SAY XBTADDR1
   IF CURLOOP = '1'
      @ 13,48 SAY XSTADDR1
   ENDIF

   @ 14,04 SAY XBTADDR2
   IF CURLOOP = '1'
      @ 14,48 SAY XSTADDR2
   ENDIF

   @ 15,04 SAY XBTADDR3
   IF CURLOOP = '1'
      @ 15,48 SAY XSTADDR3
   ENDIF

   @ 21,47 SAY ALLT(PSHIPVIA)
   @ 21,67 SAY INVHDR.Cartons
   @ 21,72 SAY INVHDR.Weight
   @ 21,84 SAY INVHDR.Rep1
   @ 24,47 SAY SUBSTR(PSPCINST,1,20)
   @ 24,67 SAY LEFT(PTERMS,11)
   @ 24,IIF(ORDHDR.STATUS = "C",78,84) SAY "/"
   @ 27,2 SAY INVHDR.Account 
   @ 27,12 SAY INVHDR.Store
   @ 27,20 SAY INVHDR.Invoice
   @ 27,29 SAY INVHDR.InvDate
   @ 27,39 SAY INVHDR->APPROVAL
   @ 27,51 SAY INVHDR.CustPo
   @ 27,67 SAY INVHDR.Dept
   @ 27,75 SAY INVHDR.Piktkt


   ROW = 29
   SELECT SCALE
   FOR lnCount = 1 TO 4
     IF EMPTY(laScales(lnCount))
       EXIT
     ENDIF
     IF SEEK("S" + laScales(lnCount))
       @ ROW,24 SAY LEFT(SCALE,2)+PADL(ALLTRIM(SZ1),4)+PADL(ALLTRIM(SZ2),4)+PADL(ALLTRIM(SZ3),4)+;
                                  PADL(ALLTRIM(SZ4),4)+PADL(ALLTRIM(SZ5),4)+PADL(ALLTRIM(SZ6),4)+;
                                  PADL(ALLTRIM(SZ7),4)+PADL(ALLTRIM(SZ8),4)
       ROW = ROW + 1 
     ENDIF
   ENDFOR
   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 34
   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
      SELECT INVLINE
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW>=MAXROW
         EXIT
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF
      KEY = INVLINE->STYLE       && + INVLINE->COLOR
      SELECT STYLE
      SEEK KEY

      SELECT INVLINE
      =SEEK("S"+STYLE.Scale,"SCALE")
      lnCol = 20
      @ ROW,2 SAY LEFT(INVLINE.Style,7)
      @ ROW,14 SAY SUBSTR(INVLINE.Style,9,6)
      @ ROW,24 SAY LEFT(SCALE,2)
      lnCol = 26
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        @ ROW,lnCol SAY QTY&lcCount   PICTURE '@Z 9999'
        lnCol = lnCol + 4
      ENDFOR 
      @ ROW,65 SAY TOTQTY PICTURE '9999'
      @ ROW,70 SAY PRICE PICTURE '999.99'
      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT

      @ ROW,78  SAY XLINETOT  PICTURE '99999.99'
      ROW = ROW+1

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING

   SET DEVICE TO PRINT

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------

   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      @ 61,1 SAY INVHDR.Account
      @ 61,13 SAY INVHDR.Invoice
      @ 61,22 SAY INVHDR.Store
      @ 61,30 SAY SUBSTR(INVHDR.Custpo,1,12)
      @ 61,42 SAY INVHDR.Dept 
      @ 61,78 SAY '*****.**' PICTURE '99999.99'
      lnRecNo = RECNO("INVLINE")
      SKIP -1 IN INVHDR
      LOOP
   ENDIF

   *** Print invoice Notepad.
 IF llNote
   SELECT NOTEPAD
   lnOldMemW = SET("MEMOWIDTH")
   SET MEMOWIDTH TO 75

   IF TYPE + KEY <> 'C' + XINVOICE
     SEEK 'C' + XINVOICE
     lnMemLins = MEMLINES(NOTEPAD.MNOTES)
   ENDIF
   
   IF TYPE + KEY = 'C' + XINVOICE
     @ ROW,02 SAY '* -- N O T E S -- *' 
     ROW = ROW + 1 

      DO WHILE lnNotLine <= lnMemLins
       IF ROW >= MAXROW
         XNOTE_LOOP = .T.
         EXIT
       ELSE
         XNOTE_LOOP = .F.
         @ ROW,02 SAY MLINE(MNOTES,lnNotLine)
         ROW = ROW + 1
       ENDIF
       lnNotLine = lnNotLine + 1
     ENDDO
     
     IF .NOT. XNOTE_LOOP
       @ ROW,02 SAY '* -- END OF NOTES -- *'
       lnNotLine = 1
       ROW = ROW + 1 
     ELSE
       @ ROW+1,12 SAY 'C O N T I N U E D ...'
       @ 61,1 SAY INVHDR.Account
       @ 61,13 SAY INVHDR.Invoice
       @ 61,22 SAY INVHDR.Store
       @ 61,30 SAY SUBSTR(INVHDR.Custpo,1,12)
       @ 61,42 SAY INVHDR.Dept 
       @ 61,78 SAY '*****.**' PICTURE '99999.99'
       lnRecNo = RECNO("INVLINE")
       SKIP -1 IN INVHDR
       LOOP
     ENDIF
   ENDIF
   SET MEMOWIDTH TO lnOldMemW
 ENDIF

   SELECT INVHDR
   ROW = ROW + 1
   @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
   @ ROW,63 SAY XPIECES    PICTURE '999999'
   @ ROW,77 SAY XSUBTOTAL  PICTURE '999999.99'
   ROW = ROW + 1
   IF DISCOUNT<>0
      @ ROW,12 SAY 'TOTAL - D I S C O U N T'
      @ ROW,72 SAY DISCOUNT  PICTURE '9999999.99'
      ROW = ROW + 1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     IF INVHDR->TAX_AMT <> 0
       @ ROW,12 SAY XTAX_DESC
       IF !EMPTY(lcTaxRefr)
         @ Row,33 SAY lcTaxRefr
       ENDIF  
       @ ROW,62 SAY XSTRING_RATE + '%'
       @ ROW,69 SAY INVHDR->TAX_AMT  
     ENDIF
      
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,60 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,71 SAY InvHdr.nPSTAmt PICTURE '999999.99'
       Row = Row + 1
     ENDIF  
   ENDIF  

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,12 SAY 'TOTAL - F R E I G H T'
      @ ROW,73 SAY WKAMT  PICTURE '999999.99'
      ROW = ROW + 1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     *-- TMI 01/10/95
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     @ ROW,61 SAY XSTRING_RATE + '%'
     @ ROW,70 SAY INVHDR->TAX_AMT  
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,61 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,73 SAY InvHdr.nPSTAmt PICTURE '999999.99'
       Row = Row + 1
     ENDIF  
   ENDIF  

   IF LEN(TRIM(MSG1 + MSG2)) >0
      @ 51,10 SAY MSG1
      @ 52,10 SAY MSG2
   ENDIF


   @ 61,1 SAY INVHDR.Account
   @ 61,13 SAY INVHDR.Invoice
   @ 61,22 SAY INVHDR.Store
   @ 61,30 SAY SUBSTR(INVHDR.Custpo,1,12)
   @ 61,42 SAY INVHDR.Dept 
   @ 61,77 SAY INVHDR.TotalChg PICTURE '999999.99'
   SELECT INVHDR
   REPLACE PRTFLAG WITH 'P'
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     IF llOpen
       USE IN SALESREP
     ENDIF
     RETURN
   ELSE
     NEWDOC = .T.
   ENDIF
   
   * GET NEXT HEADER RECORD
ENDSCAN
IF llOpen
  USE IN SALESREP
ENDIF
SET DEVICE TO SCREEN
RETURN
*--------------------------------
*    END INV810B.PRG
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