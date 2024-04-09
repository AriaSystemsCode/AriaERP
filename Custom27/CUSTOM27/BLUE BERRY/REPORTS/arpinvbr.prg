****************************************************************************
*: Program file : ARPINVZ.PRG    
*: DESC : PRINT INVOICES - 66 LINE PAGE, 8 1/2" x 11" (For Blue Berry)
*:        A copy from invoice form D with some changes.   
*: System: Aria2.7 
*: Module: Accounts Receivable
*: DATE : 10/11/99
*: Developer: Sherif Attala Ishak 
*C101685,1 Refer to.
*:************************************************************************
*: Calls : 
*:         Functions  : gfGetMemVar()
*:                    : gfGetZone()
*:************************************************************************
*: Modifications :
*:B603512,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.
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
MAXROW = 47
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

STORE 1 TO lnStart
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
        XSTADDR1 = CADDRESS12
        XSTADDR2 = laShipTo22
        XSTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52
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
   @ 06,62 SAY INVHDR.INVDATE
   @ 06,71 SAY XINVOICE
   


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

   @ 13,9 SAY XBTNAME
   IF CURLOOP = '1'
      @ 13,50 SAY XSTNAME
   ENDIF

   @ 14,9 SAY XBTADDR1
   IF CURLOOP = '1'
      @ 14,50 SAY XSTADDR1
   ENDIF

   @ 15,9 SAY XBTADDR2
   IF CURLOOP = '1'
      @ 15,50 SAY XSTADDR2
   ENDIF

   @ 16,9 SAY XBTADDR3
   IF CURLOOP = '1'
      @ 16,50 SAY XSTADDR3
   ENDIF

   *@ 16,11 SAY IIF(SEEK(INVHDR.REP1,"SALESREP"),SALESREP.Name,"")
   *@ 16,48 SAY ORDER
   *@ 16,74 SAY INVHDR.SHIPDATE
   *@ 19,2 SAY INVHDR->CUSTPO
   *@ 19,19 SAY ORDHDR.Entered
   *@ 19,30 SAY PTERMS
   *@ 19,50 SAY ALLT(PSHIPVIA)
   *@ 19,67 SAY IIF(!EMPTY(INVHDR.Cod),"COLLECT","PREPAID")  
   
   @ 25,0 SAY INVHDR->CUSTPO
   @ 25,12 SAY SUBSTR(PTERMS,1,12)
   @ 25,25 SAY INVHDR.REP1
   @ 25,30 SAY INVHDR.ShipDate
   @ 25,40 SAY SUBSTR(ALLT(PSHIPVIA),1,8)

   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT INVLINE
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 29
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
      FOR lnCount = lnStart TO SCALE.Cnt
        lcCount = STR(lnCount,1)
        IF EMPTY(QTY&lcCount)
          LOOP
        ENDIF  
        IF  ROW>=MAXROW
          lnStart = lnCount
          EXIT
        ELSE
          lnStart = 1
        ENDIF
        @ ROW,2 SAY QTY&lcCount   PICTURE '@Z 9999999'
        @ ROW,11 SAY SUBSTR(STYLE,1,6) + " " + SUBSTR(ALLT(SCALE.SZ&lcCount),1,3)
        @ ROW,22 SAY SUBSTR(STYLE,14) + " " + STYLE.DESC
        @ ROW,60  SAY PRICE PICTURE '999.99'
        @ ROW,69  SAY PRICE * TOTQTY  PICTURE '999999.99'
        ROW = ROW + 1
      ENDFOR 

      * CUM INVOICE TOTALS
      IF lnStart = 1
        XLINETOT   = PRICE * TOTQTY
        XPIECES    = XPIECES + TOTQTY
        XSUBTOTAL  = XSUBTOTAL+XLINETOT
        SKIP
      ENDIF 

   ENDDO
   *** END LINE PROCESSING

   SET DEVICE TO PRINT

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------

   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'

      @ 62,30 SAY INVHDR.Note1
      @ 62,30 SAY INVHDR.Note2
      @ 63,68 SAY '*******.**'
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
       @ 62,30 SAY INVHDR.Note1
       @ 63,30 SAY INVHDR.Note2
       @ 63,68 SAY '*******.**'
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
*   @ ROW,62 SAY XPIECES    PICTURE '999999'
   @ ROW,68 SAY XSUBTOTAL  PICTURE '9999999.99'
   ROW = ROW + 1
   IF DISCOUNT<>0
      @ ROW,12 SAY 'TOTAL - D I S C O U N T'
      @ ROW,68 SAY DISCOUNT  PICTURE '9999999.99'
      ROW = ROW + 1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     @ ROW,56 SAY XSTRING_RATE + '%'
     @ ROW,66 SAY INVHDR->TAX_AMT  
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,56 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,66 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
   ENDIF  

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,12 SAY 'TOTAL - F R E I G H T'
      @ ROW,68 SAY WKAMT  PICTURE '9999999.99'
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
     @ ROW,56 SAY XSTRING_RATE + '%'
     @ ROW,66 SAY INVHDR->TAX_AMT  
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,59 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,69 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
   ENDIF  


    @ 62,30 SAY INVHDR.Note1
    @ 63,30 SAY INVHDR.Note2
    @ 63,68 SAY INVHDR->TOTALCHG  PICTURE '9999999.99'

   SELECT INVHDR
   *B603512,1 BWA 03/08/2000 Comment the update of the field PRTFLAG.[START]
   *REPLACE PRTFLAG WITH 'P'
   *B603512,1 BWA 03/08/2000 [END]
   
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