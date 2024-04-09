*:***************************************************************************
*: Program file  : ARPINVMO.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR MORELLE PRODUCTS.
*: 				   The program is a conversion from 2.6 ..INV810Z.PRG
*! Date          : 09/20/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)
*: Tracking Job Number: C101974
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVMO
*:***************************************************************************
*: Modifications ..
*:***************************************************************************
*: B606787,1 RAE 01/01/2003 Some Adjustments in the invoice form.
*:***************************************************************************
           *--Section of color length --*
DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

           *--Section of Initializing the variables --*
XNOTE_LOOP = .F.      && Flag to indicate whether we have finished printing the Notepad or not.
llNoRec    = .F.      && Variable used to make the endreport function in the main program.
lnNotLine  = 1        && For the notepad
NEWDOC     = .T.
MAXROW     = 49
ROW        = 0
PAGENO     = 1

IF !USED(lcFabric)
  =gfOpenFile(gcDataDir+"FABRIC","CFABRIC", 'SH', @lcFabric, .T.)
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
    PAGENO    = 1                 
    NEWDOC    = .F.
    XORDER    = ORDER
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION

    *-- GET THE BILL TO AND SHIP ADDRESS
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

    *-- GET THE DESCRIPTION ABOUT THE CODES
    SELECT CODES
    SET ORDER TO CODES IN CODES
    PTERMS   = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST = gfCodDes(INVHDR.SPCINST,'SPCINST')

    DO CASE
      CASE 'G' $ lcUpsType
        XZN = '(' + INVHDR.UPSZONE +')'
      CASE '2' $ lcUpsType
        XZN  = '(12)'
      CASE 'N' $ lcUpsType
        XZN  = '(22)'
      OTHERWISE
        XZN  = ''
    ENDCASE

    PSHIPVIA = TRIM(PSHIPVIA)+XZN
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
   *B606787,1 RAE move the detail portion one line up. [start]
   *ROW    =  23
   ROW    =  22
   *B606787,1 RAE [end]

   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
     SELECT INVLINE
     IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
       IF ROW >= MAXROW
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
     
     XCOLOR = SUBSTR(STYLE,lnClrPos,lnClrLen)
     KEY = INVLINE.STYLE
     SELECT STYLE
     SEEK KEY
     
      SELECT CODES
      lcColors = gfCodDes(XCOLOR,'COLOR')
      XCOLDESC = IIF(!EMPTY(lcColors) , lcColors , "")
      
      lcGroup = gfCodDes(STYLE.CSTYGROUP ,'CSTYGROUP')
      XGROUP  = IIF(!EMPTY(lcGroup) , lcGroup , "")
      
      SELECT (lcFabric)
      SEEK STYLE.FABRIC
      XFABRIC = IIF(EMPTY(DESC) , "" , DESC)
        
      SELECT INVLINE
      @ ROW,01 SAY LEFT(STYLE,lnClrPos-2)
      @ ROW,15 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
      @ ROW,25 SAY QTY1   PICTURE '@Z 999'
      @ ROW,29 SAY QTY2   PICTURE '@Z 999'
      @ ROW,33 SAY QTY3   PICTURE '@Z 999'
      @ ROW,37 SAY QTY4   PICTURE '@Z 999'
      @ ROW,41 SAY QTY5   PICTURE '@Z 999'
      @ ROW,45 SAY QTY6   PICTURE '@Z 999'
      @ ROW,49 SAY QTY7   PICTURE '@Z 999'
      @ ROW,53 SAY QTY8   PICTURE '@Z 999'
      *B606787,1 RAE move the Total one character to the left. [start]
      *@ ROW,58 SAY TOTQTY PICTURE '99999'
      @ ROW,57 SAY TOTQTY PICTURE '99999'
      *B606787,1 RAE [end]

      * CUM INVOICE TOTALS
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT
      
      *B606787,1 RAE move the total price one character to the left. [start]
      *@ ROW,65  SAY PRICE     PICTURE '9999.99'
      *@ ROW,77  SAY XLINETOT  PICTURE '999999.99'
      @ ROW,64  SAY PRICE     PICTURE '9999.99'
      @ ROW,76  SAY XLINETOT  PICTURE '999999.99'
      *B606787,1 RAE [end]
      ROW = ROW+1

      @ ROW,01 SAY SUBSTR(ALLTRIM(STYLE.DESC),1,12)
      @ ROW,15 SAY SUBSTR(ALLTRIM(XCOLDESC),1,10)
      @ ROW,28 SAY SUBSTR(ALLTRIM(XGROUP),1,8)
      @ ROW,39 SAY XFABRIC  
      ROW = ROW+1

      * GET NEXT LINE ITEM
      SELECT INVLINE
      SKIP

   ENDDO
   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')

   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
     =lfGetcont()
     LOOP
   ENDIF
   
   *-- Print invoice Notepad.
   IF llRpInvNot
     SELECT NOTEPAD
     lnOldMemW = SET("MEMOWIDTH")
     SET MEMOWIDTH TO 75

     IF TYPE + KEY <> 'C' + XINVOICE
       SEEK 'C' + XINVOICE
     ENDIF
     lnMemLins = MEMLINES(NOTEPAD.MNOTES)

     IF TYPE + KEY = 'C' + XINVOICE
       @ ROW,02 SAY '* -- N O T E S -- *' 
       ROW = ROW + 1 
       DO WHILE lnNotLine <= lnMemLins
         IF ROW >= MAXROW
           XNOTE_LOOP = .T.
         ELSE
           XNOTE_LOOP = .F.
           @ ROW,02 SAY MLINE(NOTEPAD.MNOTES,lnNotLine)
           ROW = ROW + 1
         ENDIF
         IF ROW >= MAXROW
           =lfGetcont()
           =lfGetInHdr()
         ENDIF
         lnNotLine = lnNotLine + 1
       ENDDO
       IF !XNOTE_LOOP
         @ ROW,02 SAY '* -- END OF NOTES -- *'
         lnNotLine = 1
         ROW = ROW + 1 
       ELSE
         =lfGetcont()
         LOOP
       ENDIF
     ENDIF
     SET MEMOWIDTH TO lnOldMemW
   ENDIF

   SELECT INVHDR
   @ ROW,11 SAY '** MERCHANDISE**'
   *B606787,1 RAE move the total price one character to the left. [start]
   *@ ROW,45 SAY 'TOTAL PIECES'
   *@ ROW,58 SAY XPIECES              PICTURE '99999'
   *@ ROW,77 SAY XSUBTOTAL            PICTURE '999999.99'
   @ ROW,44 SAY 'TOTAL PIECES'
   @ ROW,57 SAY XPIECES              PICTURE '99999'
   @ ROW,76 SAY XSUBTOTAL            PICTURE '999999.99'
   *B606787,1 RAE [end]
   
   ROW=ROW+1

   IF DISCOUNT<>0
      @ ROW,11 SAY 'LESS : DISCOUNT'
      @ ROW,77 SAY DISCOUNT             PICTURE '999999.99'
      ROW=ROW+1
   ENDIF

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,11 SAY '** FREIGHT **'
      *B606787,1 RAE move the freight one character to the left. [start]
      *@ ROW,77 SAY WKAMT                PICTURE '999999.99'
      @ ROW,76 SAY WKAMT                PICTURE '999999.99'
      *B606787,1 RAE [end]
      ROW=ROW+1
   ENDIF
   
   *B606787,1 RAE move the footer one line up. [start]
   *ROW =  52
   ROW =  51
   *B606787,1 RAE [end]
   @ ROW,08 SAY XNOTE1
   @ ROW,40 SAY CHR(179)
   @ ROW,42 SAY lcRpMsg1
   ROW = ROW + 1
   @ ROW,08 SAY XNOTE2
   @ ROW,40 SAY CHR(179)     
   @ ROW,42 SAY lcRpMsg2
   
   *B606787,1 RAE move the footer one line up. [start]
   *ROW = 54
   ROW = 53
   *B606787,1 RAE [end]
   
   IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
     @ ROW,08 SAY 'APPROVAL: ' + INVHDR.APPROVAL
   ENDIF
   @ ROW,63 SAY 'INVOICE: '+INVHDR.INVOICE
   
   *B606787,1 RAE move the footer one line up. [start]
   *ROW = 57
   ROW = 56
   *B606787,1 RAE [end]
   @ ROW,10 SAY INVHDR.SHIPDATE
   @ ROW,33 SAY PSHIPVIA
   @ ROW,53 SAY CARTONS                  PICTURE '@Z 999'
   @ ROW,57 SAY WEIGHT                   PICTURE '@Z 99999'
   @ ROW,73 SAY INVHDR.TOTALCHG          PICTURE '999999.99'
   

   SELECT INVHDR
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN
   ELSE
      NEWDOC = .T.
   ENDIF
ENDSCAN
WAIT CLEAR
SET DEVICE TO SCREEN
RETURN

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 09/21/2000
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
ROW = 1

* LINE 1
IF llPrntComp
  @ ROW,02 SAY 'MORELLE PRODUCTS, LTD.'               
ENDIF

IF llPrntComp
  @ ROW,41 SAY '209 W 38th STREET'                   
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,60 SAY 'R E M I T   T O:'
ENDIF

* LINE 2
ROW = 2

IF llPrntComp
  @ ROW,02 SAY 'philippe adec'           
  @ ROW,41 SAY 'NEW YORK, N.Y. 10018'    
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,60 SAY XFNAME
ENDIF

* LINE 3
ROW = 3
IF llPrntComp
  @ ROW,41 SAY 'TEL: 212-391-8070'               
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,60 SAY XFADDR1
ENDIF

* LINE 4
ROW = 4
IF llPrntComp
  @ ROW,41 SAY 'FAX: 212-921-5321'   
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.cFACCODE)
  @ ROW,60 SAY XFADDR2
ENDIF

* LINE 6
*B606787,1 RAE move the INVOICE and the PAGE NO one line up.  [start]
*ROW = 6
ROW = 5
*B606787,1 RAE [end]

@ ROW,35 SAY 'INVOICE: '+XINVOICE
@ ROW,58 SAY 'PAGE NO.: '               
@ ROW,68 SAY PAGENO PICTURE '99'

* LINE 9
ROW = 9
@ ROW,10 SAY XBTNAME
IF CURLOOP = '1'
  @ ROW,58 SAY XSTNAME
ENDIF

ROW = 10
@ ROW,10 SAY XBTADDR1
IF CURLOOP = '1'
  @ ROW,58 SAY XSTADDR1
ENDIF

ROW = 11
@ ROW,10 SAY XBTADDR2
IF CURLOOP = '1'
  @ ROW,58 SAY XSTADDR2
ENDIF

ROW = 12
@ ROW,10 SAY XBTADDR3
IF CURLOOP = '1'
  @ ROW,58 SAY XSTADDR3
ENDIF
*B606787,1 RAE move this line one line up. [start]
*ROW = 18
ROW = 17
*B606787,1 RAE [end]
@ ROW,01 SAY ACCOUNT
@ ROW,08 SAY IIF(INVHDR.STATUS ='V' , '** VOID **' , LEFT(CUSTPO,10))
@ ROW,19 SAY IIF(CURLOOP='1', XSTORE ,'')
@ ROW,28 SAY DEPT
@ ROW,34 SAY ORDER
@ ROW,42 SAY LEFT(PTERMS,14)
@ ROW,57 SAY REP1
@ ROW,61 SAY REP2
@ ROW,65 SAY PSHIPVIA
*------------------
* PRINT SCALES
*------------------
lcScalStr = SPACE(0)
lcInvNo   = INVHDR.INVOICE
DO lpGtScl WITH lcInvNo
SELECT Scale
Row = 20
FOR lnI = 1 TO LEN( lcScalStr )
  lcSizeScal = SUBSTR( lcScalStr, lnI, 1 )
  SEEK 'S' + lcSizeScal

*  @ Row,22 SAY lcSizeScal
  @ Row,25 SAY SUBSTR(SCALE.SZ1,1,3)
  @ Row,29 SAY SUBSTR(SCALE.SZ2,1,3)
  @ Row,33 SAY SUBSTR(SCALE.SZ3,1,3)
  @ Row,37 SAY SUBSTR(SCALE.SZ4,1,3)
  @ Row,41 SAY SUBSTR(SCALE.SZ5,1,3)
  @ Row,45 SAY SUBSTR(SCALE.SZ6,1,3)
  @ Row,49 SAY SUBSTR(SCALE.SZ7,1,3)
  @ Row,53 SAY SUBSTR(SCALE.SZ8,1,3)
     
  Row = Row + 1
ENDFOR

Row = 23

*--End of lfGetInHdr.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 09/21/2000
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
ROW = 57
PAGENO = PAGENO + 1                    
@ ROW,10 SAY INVHDR.SHIPDATE

@ ROW,32 SAY PSHIPVIA
@ ROW,53 SAY '***'
@ ROW,57 SAY '*****'
@ ROW,73 SAY '******.**'

*--End of lfGetcont.
*!*************************************************************
*! Name      : lpGtScl
*! Developer : BASSEM RAFAAT 
*! Date      : 09/21/2000
*! Purpose   : Build a string holding the first uniqe 3 scales
*!           : from the invoice line scal.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lpGtScl()
*!*************************************************************
PROCEDURE lpGtScl
PARAMETERS lcInvNo
PRIVATE lcAlias, lnCount

lcAlias = ALIAS()
SELECT InvLine
lcExp   = Invoice + STR(LineNo,6)   && To hold the invocie before the scan.
lnCount = 1
SEEK lcInvNo
SCAN WHILE InvLine.Invoice = lcInvNo .AND. lnCount <= 3
  IF !( LEFT(Scale,1) $ lcScalStr )
    lcScalStr = lcScalStr + LEFT(Scale,1)
    lnCount   = lnCount + 1
  ENDIF
ENDSCAN
= SEEK(lcExp ,"InvLine")
SELECT (lcAlias)

*--End of lpGtScl.
