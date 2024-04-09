****************************************************************************
*: Program file : RMCMEMUN.PRG    
*: DESC : PRINT CREDIT MEMOS - 66 LINE PAGE, 8 1/2" x 11" (For UniFlair)
*:        It has the same layout as their custom invoice form.
*: System: Aria2.7 
*: Module: Accounts Receivable
*: DATE : 11/11/99
*: Developer: Sherif Attala Ishak 
*C101681,1 Refer to.
*:************************************************************************
*: Calls : 
*:         Functions  : 
*:                    : 
*:************************************************************************
*: Modifications :
****************************************************************************


XNOTE_LOOP = .F.   && Flag to indicate whether we have finished
                   && printing the Notepad or not.

STORE "" TO ENDPAGE
llOpen = gfOpenFile(GCDATADIR+'SALESREP','SALESREP','SH')
llOpen = gfOpenFile(GCDATADIR+'ORDHDR','ORDHDR','SH')

gcPhnFrmt = lcPhonPict
NEWDOC = .T.
MAXROW = 45
lcDist = ' '

lnNotLine = 1
STORE .F. TO llNoRec
STORE lcCompPhon  TO HLINE5

DECLARE laSoldTo[5,1] , laShipTo[5,1]
laSoldTo   = ''
laShipTo   = ''
lnNotLine = 1

SELECT RETHDR

*------------------------------
* SECTION: MAIN LOOP
*------------------------------
XNOTE_LOOP = .F.  && Flag to indicate whether we have finished
                  && printing the Notepad or not.

lcZone = ''
DECLARE laZone[1,2]


WAIT WINDOW 'PRINTING CREDIT MEMOS - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT
gcPhnFrmt = lcPhonPict


SELECT RETHDR

SET RELATION TO

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

    XCRMEMO = CRMEMO
    *-- Get Customer DUNS[start]
    lnPrevAl = SELECT()
    SELECT CUSTOMER
    XDUNS = DUNS
    SELECT (lnPrevAl)
    *-- Get Customer DUNS[end]

   *-----------------------------------------------------------------
   * Get CRMEMO header, line items, and financial history records.
   * If any records are not found, skip to next CRMEMO.
   * Initialize document totals.
   *-----------------------------------------------------------------
   IF NEWDOC
      PRTERR = 0
      STORE 0.00 TO XPIECES, XSUBTOTAL
      SELECT RETHDR
      NEWDOC    = .F.
      XPHONE    = laCompAdd[5]
      XACCOUNT  = ACCOUNT
      XDIVISION = CDIVISION
      XSTORE    = STORE
      ***
      *** GET THE BILL TO AND SHIP ADDRESS
      =lfHeadVar()
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
        XSTADDR3 = ALLTRIM(CADDRESS3) + ' ' +ALLTRIM(CADDRESS4) + ' ' + CADDRESS5
        IF LEN(TRIM(XSTADDR2)) =0
          XSTADDR2 = XSTADDR3
          XSTADDR3 = ''
        ENDIF
      ELSE
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

      
      
      ****** FIND THE CRMEMO LINES

      SELECT &LCRETLINE
      SEEK XCRMEMO
      IF EOF()
         PRTERR = 2
      ENDIF
      *** GET THE DESCRIPTION ABOUT THE CODES
      

      *----------------------------------------------
      * [FACTOR] NAME & ADDRESS
      *----------------------------------------------
      CURLOOP = '1'
      STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
   ELSE
      GO lnRecNo IN &LCRETLINE
   ENDIF
   ***
   ****** END NEWDOC
   ***
   *-----------------
   * START PRINT
   *-----------------
   * LINE 2
   @ 03, 01 SAY "C R E D I T  M E M O"
   @ 03 ,62 SAY "CM  " + RetHdr.CrMemo
   @ 04,66 SAY RETHDR.CRDATE


   @ 8,04 SAY RETHDR.Account
   IF CURLOOP = '1'
      @ 8,42 SAY RETHDR.Store
   ENDIF
    
   @ 9,04 SAY XBTNAME
   IF CURLOOP = '1'
      @ 9,42 SAY XSTNAME
   ENDIF

   @ 10,04 SAY XBTADDR1
   IF CURLOOP = '1'
      @ 10,42 SAY XSTADDR1
   ENDIF

   @ 11,04 SAY XBTADDR2
   IF CURLOOP = '1'
      @ 11,42 SAY XSTADDR2
   ENDIF

   @ 12,04 SAY XBTADDR3
   IF CURLOOP = '1'
      @ 12,42 SAY XSTADDR3
   ENDIF


   @ 16,11 SAY IIF(SEEK(RetHdr.SalesRep1,"SALESREP"),SALESREP.Name,"")
   @ 16,48 SAY RETHDR.ORDER
   @ 19,2 SAY RETHDR->CUSTPO
   @ 19,19 SAY IIF(SEEK("O"+RETHDR.Order,"ORDHDR"),ORDHDR.Entered,"")
   @ 19,30 SAY ALLT(lcTerms)
   @ 22,1 SAY SUBSTR(lcStyTitle,1,ATC("-",lcStyTitle,2) - 1)
   @ 22,20 SAY "S"
   @ 22,60 SAY "T. Pcs"
   @ 22,67 SAY "U.Price"
   @ 22,77 SAY "Total"
   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT &LCRETLINE
   XSCALE =  ' '
   ROW    = 24
   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
      SELECT &LCRETLINE
      IF EOF() .OR. CRMEMO <> XCRMEMO .OR. ROW>=MAXROW
         EXIT
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF
      lcReason = gfCodDes(&LCRETLINE..REASON,'REASON')
      KEY = &LCRETLINE..STYLE       && + LCRETLINE->COLOR
      SELECT STYLE
      SEEK KEY

      SELECT &LCRETLINE
      =SEEK("S"+STYLE.Scale,"SCALE")
      @ ROW,1 SAY SUBSTR(STYLE,1,ATC("-",STYLE,2) - 1)
      lnCol = 20
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        IF EMPTY(QTY&lcCount)
          LOOP
        ENDIF
        @ ROW,lnCol SAY PADL(ALLT(SCALE.SZ&lcCount),4)
        lnCol = lnCol + 5
      ENDFOR 
      ROW = ROW + 1
      @ ROW,1 SAY SUBSTR(STYLE->DESC,1,19)
      lnCol = 20
      FOR lnCount = 1 TO 8
        lcCount = STR(lnCount,1)
        IF EMPTY(QTY&lcCount)
          LOOP
        ENDIF
        @ ROW,lnCol SAY QTY&lcCount   PICTURE '@Z 9999'
        lnCol = lnCol + 5
      ENDFOR 
      @ ROW,62 SAY TOTQTY PICTURE '9999'
      ROW = ROW + 1
      @ ROW,1 SAY "Return Reason : " + ALLT(lcReason)
      * CUM CRMEMO TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      @ ROW,68  SAY PRICE     PICTURE '999.99'
      @ ROW,74  SAY XLINETOT  PICTURE '99999.99'
      ROW = ROW+1

      * GET NEXT LINE ITEM
      SELECT &LCRETLINE
      SKIP
   ENDDO
   *** END LINE PROCESSING

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------

   ENDPAGE = IIF(&LCRETLINE..CRMEMO = XCRMEMO ,'1','0')
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'

      @ 49,23 SAY RetHdr.cRetNote1 + ' ' + RetHdr.cRetNote2

      *@ 54,49 SAY RETHDR.DUEDATE
      @ 54,73 SAY '*****.**'
      lnRecNo = RECNO(LCRETLINE)
      SKIP -1 IN RETHDR
      LOOP
   ENDIF

   *** Print CRMEMO Notepad.

   SELECT RETHDR
   ROW = ROW + 1
   @ ROW,12 SAY 'TOTAL - M E R C H A N D I S E'
   @ ROW,60 SAY XPIECES    PICTURE '999999'
   @ ROW,73 SAY XSUBTOTAL  PICTURE '999999.99'
   ROW = ROW + 1
   IF llTax AND RetHdr.Tax_Amt <>  0
     @ ROW,12 SAY lcTaxDsc 
     @ ROW,73 SAY RetHdr.Tax_Amt PICTURE '999999.99'
     ROW = ROW + 1
   ENDIF
   @ ROW,12 SAY "Other Credits" 
   @ ROW,73 SAY RetHdr.Other PICTURE '999999.99'

   @ 49,23 SAY RetHdr.cRetNote1 + ' ' + RetHdr.cRetNote2

   @ 54,73 SAY RetHdr.Amount + RetHdr.Other + RETHDR.Tax_Amt  PICTURE '999999.99'
   SELECT RETHDR
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
*! Synopsis : Get the zone to be printed in the CRMEMO format.
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