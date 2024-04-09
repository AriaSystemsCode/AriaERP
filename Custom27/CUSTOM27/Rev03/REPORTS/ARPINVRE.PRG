*:**************************************************************************
*: Program file  : ARPINVRE
*: Program desc. : Custom invoice form based on Form H.
*: System        : Aria Advantage Series.
*: Module        : Accounts Receivable (AR)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 12/17/2002
*: Reference     : C102698
*:**************************************************************************
*: Notes         : This invoice is exactly the same as form H
*: B120846,1 ABD 12/17/2003 custom invoice missing style# on 2nd page. 
*:**************************************************************************
*

PRIVATE lcTaxRefr , lcOldStyle , lnStyleLen
lcOldStyle = ''
lnStyleLen = LEN(ALLTRIM(gfItemMask('PM')))
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

*-- Case the user select sort by style.
SELECT InvHdr
LOCATE FOR &lcRpExp
IF !FOUND()
  *-- Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , '.T.' , lcRpExp)
SET DEVICE TO PRINT

SELECT INVLINE
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcStylIndx) FROM ARRAY laFileStru

*-- The invline is right due to use this name when replace the invline file place the temp. file(gcWorkDir + INVLINE + '.CDX')
INDEX ON Invoice + Style TAG INVLINE OF (gcWorkDir + INVLINE + '.CDX')
*INDEX ON Invoice + STR(LineNo,6) TAG INVLINE OF (gcWorkDir + INVLINE + '.CDX')

CREATE TABLE (gcWorkDir+lcInvFoter) (Invoice C(6), cLine1 C(40), cLine2 C(40), cLine3 C(40), cLine4 C(40))
INDEX ON Invoice TAG InvFooter OF (gcWorkDir + lcInvFoter + '.CDX') 

SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO CONSINVL ADDITIVE

PRIVATE lnConsLine , lcDateCent , lcInvLnTag , lnLinesNo , lcLinesNo
lcDateCent = SET('CENTURY')
SET CENTURY ON

lnConsLine = 0
SCAN FOR &lcASExp

  *--lcInvcee    >> Variable hold the invoice No.
  lcInvcee = InvHdr.Invoice
  SELECT (lcInvFoter)
  lnLinesNo = 1
  APPEND BLANK
  REPLACE Invoice WITH lcInvcee

  IF llTAX AND lcTaxMeth = 'M' AND InvHdr.Tax_Amt # 0
    IF !EMPTY(lcTaxRefr)
      lcLinesNo = ALLTRIM(STR(lnLinesNo,1))
      REPLACE cLine&lcLinesNo WITH lcTaxRefr
      lnLinesNo = lnLinesNo + 1
    ENDIF  
    lcLinesNo = ALLTRIM(STR(lnLinesNo,1))
    REPLACE cLine&lcLinesNo WITH lcTaxDesc + ' :' + ALLTRIM(STR(INVHDR.TAX_RATE,5,2)) + '  ' + ;
                                 TRANSFORM(InvHdr.Tax_Amt,'9999999.99')
    lnLinesNo = lnLinesNo + 1
  ENDIF

  IF Invhdr.FREIGHT + Invhdr.INSUR + Invhdr.COD <> 0
    lcLinesNo = ALLTRIM(STR(lnLinesNo,1))
    *REPLACE cLine&lcLinesNo WITH 'TOTAL - FREIGHT' + SPACE(4) + TRANSFORM(Invhdr.FREIGHT+Invhdr.INSUR+Invhdr.COD,'9999999.99')
    REPLACE cLine&lcLinesNo WITH 'TOTAL - FREIGHT ' + TRANSFORM(Invhdr.FREIGHT+Invhdr.INSUR+Invhdr.COD,'9999999.99')
    lnLinesNo = lnLinesNo + 1
  ENDIF

  IF Invhdr.DISCOUNT<>0
    lcLinesNo = ALLTRIM(STR(lnLinesNo,1))
    *REPLACE cLine&lcLinesNo WITH 'DISCOUNT' + SPACE(9) + TRANSFORM(Invhdr.Discount,'99999999.99')
    REPLACE cLine&lcLinesNo WITH 'DISCOUNT' + SPACE(5) + TRANSFORM(Invhdr.Discount,'99999999.99')
  ENDIF
  
  lnConsLine = 0    
  IF INVHDR.CONSOL = 'Y'
    *--Case consolidated invoice.
    SELECT CONSINVL
    SCAN REST WHILE Invoice+Store+Order+Style+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT

      SCATTER MEMVAR MEMO
      lnConsLine = lnConsLine + 1
      m.LineNo = lnConsLine
      
      SELECT InvLine
      lcInvLnTag = TAG()
      SET ORDER TO TAG InvLines
      m.Desc1 = IIF(SEEK(m.Style+m.Invoice),Desc1,'')
      SET ORDER TO TAG (lcInvLnTag)
      
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO      
    ENDSCAN
  ELSE
    SELECT INVLINE
    SCAN REST WHILE Invoice+STR(lineno,6) = lcInvcee
      WAIT WINDOW 'Selecting Records For The Report ...' + lcInvcee NOWAIT
      SCATTER MEMVAR MEMO
      SELECT(lcStylIndx)
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDSCAN
  ENDIF  
ENDSCAN

SET CENTURY &lcDateCent

*-- Section break the relations
SELECT InvHdr
SET RELATION OFF INTO (lcTmpDbt)
SET RELATION OFF INTO CUSTOMER
SET RELATION OFF INTO ORDHDR
SET RELATION OFF INTO CONSINVL
SELECT (lcTmpDbt)
SET RELATION TO

SELECT InvLine
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT STYLE
SET RELATION OFF INTO SCALE

*--Close the invline and open the temp. file with the invline name.
SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
USE IN (lcStylIndx)
USE (gcWorkDir+lcStylIndx) IN 0 ALIAS INVLINE
SELECT INVLINE
SET INDEX TO (gcworkdir+invline+'.cdx')
SET ORDER TO TAG Invline OF (gcworkdir+invline+'.cdx')

*-- Section Create the new relations
SELECT INVHDR
IF llPrntInst .OR. llRpInvNot 
  SET RELATION TO '' INTO (lcTmpDbt)
  SELECT (lcTmpDbt)
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO INVLINE ADDITIVE
ELSE
  SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
ENDIF

SELECT INVLINE
LOCATE
SET RELATION TO IIF(!EMPTY(INVLINE.ALTSTYLE) , INVLINE.ALTSTYLE , INVLINE.Style) INTO STYLE ADDITIVE
SET RELATION TO "S" + INVLINE.Account + INVLINE.Style INTO SPCK_LIN ADDITIVE

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE

SELECT INVHDR
SET RELATION TO IIF(EMPTY(Store) OR Store = "********", 'M' + Account ,;
                  'S' + Account + Store) INTO CUSTOMER ADDITIVE
SET RELATION TO 'O' + INVHDR.ORDER INTO OrdHdr ADDITIVE
SET RELATION TO 'C' + INVHDR.INVOICE INTO NOTEPAD ADDITIVE
SET RELATION TO INVHDR.INVOICE INTO (lcInvFoter) ADDITIVE

IF llPrntInst .OR. llRpInvNot
  SET SKIP TO (lcTmpDbt) , INVLINE
ELSE
  SET SKIP TO INVLINE
ENDIF

SELECT INVHDR
DO gfDispRe WITH EVAL('lcFormName'), 'FOR ' + lcRpExp
*DO gfDispRe WITH EVAL('lcFormName')
SET DEVICE TO SCREEN
llarpinv = .F.
WAIT CLEAR

SELECT INVLINE
CLOSE INDEX
USE IN INVLINE
=gfOpenFile(gcDataDir+"InvLine","InvLine",'SH')
*-- End of Program.

if .f.
IF EMPTY(lcRpFac1) AND EMPTY(lcRpFac2) AND EMPTY(lcRpFac3)
  IF FILE(gcDataDir+lcFacStNam+'.MEM')
    RESTORE FROM gcDataDir+lcFacStNam+'.MEM' ADDITIVE
  ENDIF
ENDIF

llAltShp = .F.
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

*-- To indicate whether we are a canadian company or not.
llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)

NEWDOC = .T.
MAXROW = 47
lcDist = ' '
llNote = llRpInvNot

SELECT INVHDR
lnNotLine = 1
STORE .F. TO llNoRec

XNOTE_LOOP = .F.  && Flag to indicate whether we have finished printing the Notepad or not.

lcZone = ''
DECLARE laZone[1,2]
laZone[1,1]     = 'CUPS'
laZone[1,2]     = 'lcZone'

WAIT WINDOW 'PRINTING INVOICES - <Space Bar> TO ABORT' NOWAIT
SET DEVICE TO PRINT

XINVNAME = lcPrnComp

SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

SCAN FOR &lcASExp WHILE INKEY() <> 32
  XINVOICE = INVOICE
  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------
  IF NEWDOC
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
    XPHONE    = laCompAdd[5]
    XNOTE1    = IIF(NOTE1<>'*', NOTE1, '')
    XNOTE2    = IIF(NOTE2<>'*', NOTE2, '')
    XORDER    = ORDER
    XPIKTKT   = PIKTKT
    XACCOUNT  = ACCOUNT
    XSTORE    = STORE
    XSEASON   = SEASON
    XDIVISION = CDIVISION
    
    lcScale1 = ""
    lcScale2 = ""
    *** GET THE BILL TO AND SHIP ADDRESS
    SELECT CUSTOMER
    lcFactAcct = IIF(SEEK('M'+XACCOUNT),FactAcct,'')
    SEEK IIF(XSTORE= SPACE(8),'M'+XACCOUNT,'S'+XACCOUNT+XSTORE)
    
    XBTNAME  = BTNAME
    XBTADDR1 = CADDRESS1
    XBTADDR2 = CADDRESS2
    XBTADDR3 = TRIM(CADDRESS3) + ' ' +TRIM(CADDRESS4) + ' ' + CADDRESS5
    
    IF LEN(TRIM(XBTADDR2)) =0
      XBTADDR2 = XBTADDR3
      XBTADDR3 = ''
    ENDIF

    SELECT ORDHDR
    *SEEK XORDER
      
    IF ALT_SHPTO
      XSTADDR1 = CADDRESS12
      XSTADDR2 = laShipTo22        
      XSTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52       
        
      IF LEN(TRIM(XSTADDR2)) =0
        XSTADDR2 = XSTADDR3
        XSTADDR3 = ''
      ENDIF
    ELSE
      IF INVHDR.CONSOL = 'Y'       
        SELECT CONSINVH
        SEEK XINVOICE
        SELECT CUSTOMER
        SEEK 'S'+XACCOUNT+CONSINVH.STORE
      ENDIF

      SELECT CUSTOMER
      IF !EMPTY(DIST_CTR)
        lcDist = DIST_CTR
        = SEEK('S'+XACCOUNT+lcDist)
      ENDIF
      XSTNAME  = IIF( EMPTY(DBA) , STNAME , DBA)
      XSTADDR1 = CADDRESS12
      XSTADDR2 = CADDRESS22      
      XSTADDR3 = TRIM(CADDRESS32) + ' ' +TRIM(CADDRESS42) + ' ' + CADDRESS52
      
      IF LEN(TRIM(XSTADDR2)) =0
        XSTADDR2 = XSTADDR3
        XSTADDR3 = ''
      ENDIF
    ENDIF

    ****** FIND THE INVOICE LINES
    SELECT INVLINE
    SEEK XINVOICE

    *-- GET THE DESCRIPTION ABOUT THE CODE      
    SELECT CODES
    SET ORDER TO CODES IN CODES 
  
    PTERMS = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSHIPVIA = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    PSPCINST= gfCodDes(INVHDR.SPCINST,'SPCINST')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
    XZN = laZone[1,2]

    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------

    CURLOOP = '1'
  ENDIF
  *** END NEWDOC

  SELECT INVHDR
  *-----------------
  * START PRINT
  *-----------------
  
  IF XINVNAME='Y'
    @ 01,05 SAY HLINE1
    SELECT INVHDR
  ENDIF
  
  @ 01,60 SAY XINVOICE
  @ 01,75 SAY INVDATE        

  IF XINVNAME='Y'
    @ 02,05 SAY HLINE2    
  ENDIF

  * LINE 5
  IF XINVNAME='Y'
    @ 03,05 SAY HLINE3    
  ENDIF

  @ 03,75 SAY SUBSTR( CDIVISION, 1, 14 )  
  
  IF XINVNAME='Y'
    @ 04,05 SAY HLINE4
  ENDIF

  * LINE 7
  IF XINVNAME='Y'
     @ 05,05 SAY XPHONE  SIZE 1,16     
  ENDIF
 
  @ 05,75 SAY ORDER    

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
  
  llAltShp = OrdHdr.Alt_ShpTo
  IF llAltShp 
    XSTADDR3 = TRIM(laShipTo[3]) 
    XSTADDR4 = TRIM(laShipTo[4])
    XSTADDR5 = TRIM(laShipTo[5])
  ELSE     
    XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
  ENDIF 
   
  IF LEN(TRIM(laShipTo[2])) =0
    XSTADDR2 = laShipTo[3]
    IF llAltShp    
      XSTADDR3 = XSTADDR4
      XSTADDR4 = XSTADDR5
      XSTADDR5 = ''
    ELSE
      XSTADDR3 = ''
    ENDIF
  ENDIF

  SELECT INVHDR
   
  *C102541,1 Always print the word "Remit to :". [Begin]  
  *IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)    
  *  @ 07,14 SAY "Remit to :"  
  *  @ 07,25 SAY lcFacName 
  *ENDIF
  IF EMPTY(INVHDR.CFACCODE)
    @ 07,14 SAY "Remit to :"
    @ 07,25 SAY HLINE1 
  ELSE
    IF llPrnFact
      @ 07,14 SAY "Remit to :"  
      @ 07,25 SAY lcFacName 
    ENDIF  
  ENDIF  
  *C102541,1 Always print the word "Remit to :". [End]
  
  @ 07,75 SAY PIKTKT        
 
  *C102541,1 Always print company address. [Begin]  
  *IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  *  @ 08,25 SAY laFactor[1]
  *ENDIF
  IF EMPTY(INVHDR.CFACCODE)
      @ 08,25 SAY HLINE2
  ELSE
    IF llPrnFact
      @ 08,25 SAY laFactor[1]
    ENDIF
  ENDIF      
  *C102541,1 Always print company address. [End]
  
  *C102541,1 Always print company address. [Begin]  
  *IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  *  @ 09,25 SAY laFactor[2]
  *ENDIF
  IF EMPTY(INVHDR.CFACCODE)
      @ 09,25 SAY HLINE3
  ELSE
    IF llPrnFact
      @ 09,25 SAY laFactor[2]
    ENDIF
  ENDIF      
  *C102541,1 Always print company address. [End]
        
  @ 09,75 SAY APPROVAL    
  
  *C102541,1 Always print company address. [Begin]  
  *IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  *  @ 10,25 SAY laFactor[3]
  *ENDIF
  IF EMPTY(INVHDR.CFACCODE)
  
      *@ 10,25 SAY IIF(HLINE4=lcCompPhon,'',HLINE4)
       *-- Transform tel no to the global format.
      @ 10,25 SAY IIF(HLINE4=ALLTRIM(TRANSFORM(lcCompPhon , lcPhonPict)),'',HLINE4)
  ELSE
    IF llPrnFact
      @ 10,25 SAY laFactor[3]
    ENDIF
  ENDIF      
  *C102541,1 Always print company address. [End]
    
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)    
    @ 11,25 SAY LEFT(laFactor[4] + IIF(!EMPTY(laFactor[4]),' ' + LEFT(laFactor[5],23) ,laFactor[5]),35)
  ENDIF
  
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    @ 12,25 SAY "Ref. Account:"
    @ 12,39 SAY lcFactAcct
  ENDIF  
    
  @ 13,06 SAY XBTNAME
  @ 13,56 SAY XSTNAME
  @ 14,06 SAY XBTADDR1
  @ 14,56 SAY SUBSTR(XSTADDR1,1,25)
  @ 15,06 SAY XBTADDR2
  @ 15,56 SAY SUBSTR(XSTADDR2,1,25)
  @ 16,06 SAY SUBSTR(XBTADDR3,1,45)
  @ 16,56 SAY SUBSTR(XSTADDR3,1,29)

  IF llAltShp
    @ 17,56 SAY ALLTRIM(SUBSTR(XSTADDR4,1,29))
    @ 18,56 SAY ALLTRIM(SUBSTR(XSTADDR5,1,29))
  ENDIF

  @ 21,02 SAY ACCOUNT
  @ 21,12 SAY LEFT(CUSTPO,10)
  @ 21,23 SAY STORE
  @ 21,35 SAY DEPT  
  @ 21,41 SAY LEFT(PTERMS,21)
  @ 21,64 SAY REP1                         
  @ 21,68 SAY REP2
  @ 21,72 SAY SUBSTR(PSHIPVIA,1,13)

  * PRINT SCALES
  ROW = 23

  SELECT InvLine

  IF !XNOTE_LOOP
    trec1 = RECNO()
    tscale1=" "
    tscale2=" "
    do while .T.
      if invoice<>xinvoice .or. (tscale1<>" " AND tscale2<>" ")
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

    SELECT INVLINE
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
      @ ROW,20 SAY SCALE+PADC(SUBSTR(ALLTRIM(SZ1),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ2),1,3),5,' ')+;
                           PADC(SUBSTR(ALLTRIM(SZ3),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ4),1,3),5,' ')+;
                           PADC(SUBSTR(ALLTRIM(SZ5),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ6),1,3),5,' ')+;
                           PADC(SUBSTR(ALLTRIM(SZ7),1,3),5,' ')+PADC(SUBSTR(ALLTRIM(SZ8),1,3),5,' ')   
      row=row+1
    endif
    
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
    
    i=i+1
  enddo
  SELECT INVLINE
  SEEK XINVOICE  
  
  CNT = 1
  *---------------------------------------------------------------
  * [1] LINE ITEM PRINT LOOP
  *---------------------------------------------------------------
  SELECT INVLINE
  XSTORE = STORE
  XSCALE =  ' '
  ROW    = 27
  DO WHILE CURLOOP = '1' AND !XNOTE_LOOP           
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
      IF ROW >= MAXROW      && if there is a line will de delete from 
        =lfGetcont()        && the memo field
        =lfGetInHdr()
        LOOP
      ENDIF
      EXIT         
    ENDIF
    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF
    KEY = INVLINE.STYLE
    SELECT STYLE
    SEEK KEY
    XSTYDESC = IIF(FOUND() , SUBSTR(DESC,1,17) ,'')

    SELECT INVLINE
    @ ROW,02 SAY SUBSTR(STYLE,1,10)
    @ ROW,14 SAY SUBSTR(STYLE,14,18)
    @ ROW,20 SAY STYLE.SCALE      
    @ ROW,22 SAY IIF(QTY1=0,'',PADC(QTY1,5,' '))   PICTURE '99999'
    @ ROW,27 SAY IIF(QTY2=0,'',PADC(QTY2,5,' '))   PICTURE '99999'
    @ ROW,32 SAY IIF(QTY3=0,'',PADC(QTY3,5,' '))   PICTURE '99999'
    @ ROW,37 SAY IIF(QTY4=0,'',PADC(QTY4,5,' '))   PICTURE '99999'
    @ ROW,42 SAY IIF(QTY5=0,'',PADC(QTY5,5,' '))   PICTURE '99999'
    @ ROW,47 SAY IIF(QTY6=0,'',PADC(QTY6,5,' '))   PICTURE '99999'
    @ ROW,52 SAY IIF(QTY7=0,'',PADC(QTY7,5,' '))   PICTURE '99999'
    @ ROW,57 SAY IIF(QTY8=0,'',PADC(QTY8,5,' '))   PICTURE '99999'        
    @ ROW,61 SAY TOTQTY PICTURE '9999'
      
    XLINETOT   = PRICE * TOTQTY
    XPIECES    = XPIECES + TOTQTY
    XSUBTOTAL  = XSUBTOTAL+XLINETOT
    @ ROW,66  SAY PRICE     PICTURE '9999.99'
    @ ROW,75  SAY XLINETOT  PICTURE '999999.99'
    ROW = ROW+1
    @ ROW,02  SAY  INVLINE.dESC1
    ROW = ROW+1
      
    DO lpPrtSku      &&Procedure to print the Style/Color Sku no.
        
    * GET NEXT LINE ITEM
    SELECT INVLINE

    IF llRpInvLNt
      @ ROW,2 SAY '* -- LINE NOTES -- *'
      FOR lnLINLoop = 1 TO MEMLINES(NOTE_MEM)
        lcCurrLine = ALLTRIM(MLINE(Note_Mem,lnLINLoop))
        IF LEFT(lcCurrLine,1) <> '*'
          ROW = ROW + 1

          IF ROW >= MAXROW
            =lfGetcont()
            =lfGetInHdr()
            SELECT INVLINE
          ENDIF

          @ ROW,2 SAY lcCurrLine
        ENDIF
      ENDFOR
      ROW = ROW + 1
    ENDIF

    SKIP
  ENDDO

  *** END LINE PROCESSING
  *---------------------------------------------------------------
  * CURRENTLY PROCESSING REGULAR LINE ITEMS
  *---------------------------------------------------------------
  ENDPAGE = IIF(INVLINE.INVOICE = XINVOICE ,'1','0')
  IF ENDPAGE = '1' AND !XNOTE_LOOP
    ROW = 49
    
    *@ ROW+1,12 SAY 'C O N T I N U E D ...'
    @ ROW,12 SAY 'C O N T I N U E D ...'
    
    @ 50,09 SAY INVHDR.PHONE  PICTURE lcPhonPict SIZE 1,16

    SELECT INVHDR
    @ 50,56 SAY 'MERCHANDISE'
    @ 50,75 SAY '**********'
    ROW = ROW + 1
      
    @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
    @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)     
    @ 56,16 SAY INVHDR.CARTONS                   PICTURE '@Z 999'
    @ 56,65 SAY '*******'
    @ 56,75 SAY '**********'
     
    ROW = 1
    LOOP
  ENDIF
   
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
     
      DO WHILE lnNotLine <= MEMLINES(NOTEPAD.MNOTES)
        IF ROW >= MAXROW
          XNOTE_LOOP = .T.
        ELSE
          XNOTE_LOOP = .F.
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF  LEFT(ALLTRIM(lcCurrLine),1) # '*'
            @ ROW,02 SAY lcCurrLine
            ROW = ROW + 1
          ENDIF
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
        ROW = 49
        
        *@ ROW+1,12 SAY 'C O N T I N U E D ...'
        @ ROW,12 SAY 'C O N T I N U E D ...'        
        @ 50,09 SAY INVHDR.PHONE  PICTURE lcPhonPict SIZE 1,16

        SELECT INVHDR
        @ 50,56 SAY 'MERCHANDISE'
        @ 50,75 SAY '**********'
        ROW = ROW + 1
        @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
        @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
        @ 56,16 SAY INVHDR.CARTONS PICTURE '@Z 999'
        @ 56,65 SAY '*******'
        @ 56,75 SAY '**********'
     
        ROW = 1
        LOOP
      ENDIF
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  *C102541,1 Print factor statement for factored invoices. [Begin]
  IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
    IF ROW + IIF(llNote,2,3) >= MAXROW
      =lfGetcont()
      =lfGetInHdr()    
    ENDIF
    ROW = ROW + 1
    @ ROW,02 SAY lcRpFac1
    ROW = ROW + 1
    @ ROW,02 SAY lcRpFac2
    ROW = ROW + 1
    @ ROW,02 SAY lcRpFac3
    ROW = ROW + 1
  ENDIF  
  *C102541,1 Print factor statement for factored invoices. [End]

  @ 50,09 SAY INVHDR.PHONE  PICTURE lcPhonPict SIZE 1,16
  SELECT INVHDR
  @ 50,56 SAY 'MERCHANDISE'
  @ 50,74 SAY XSUBTOTAL            PICTURE '9999999.99'  
  ROW=51
  @ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
   
  IF XTAX AND XTAX_METH = 'M' AND INVHDR.TAX_AMT # 0
    IF !EMPTY(lcTaxRefr)
      @ Row,55 SAY lcTaxRefr
      Row = Row + 1
    ENDIF  
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ ROW ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
    @ ROW ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
    ROW = ROW + 1
  ENDIF  

  IF ROW = 53
    @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
    llNotePrnt = .T.
  ELSE
    llNotePrnt = .F.  
  ENDIF  

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

  IF !llNotePrnt
    @ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
  ENDIF

  IF XTAX AND XTAX_METH = 'A' AND INVHDR.TAX_AMT # 0
    @ 53,55 SAY lcTaxRefr                  && TMI 01/17/95
    XSTRING_RATE = ALLTRIM(STR (INVHDR.TAX_RATE,5,2))
    @ 54 ,55 SAY SUBSTR(XTAX_DESC,1,10)+' :'+XSTRING_RATE + ' %'
    @ 54 ,74 SAY INVHDR.TAX_AMT   PICT '9999999.99'
  ENDIF  

  *-- YMA 03/29/94 Print the PST tax rate and amount.
  IF xTAx AND llIsCanada 
    lcStrRate = ALLTRIM(STR(InvHdr.nPstRate,5,2))     
    lnRow = IIF (xTAx_Meth='A',55,54)
     
    @ lnRow ,55 SAY 'PST TAX    :' + lcStrRate + ' %'
    @ lnRow ,74 SAY InvHdr.nPstAmt PICTURE '9999999.99'
    IF InvHdr.nHSTAmt <> 0
      lnRow = lnRow + 1
      @ lnRow,55 SAY 'HST TAX    :' + STR(InvHdr.nHSTRate,5,2)+' %'
      @ lnRow,74 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
    ENDIF  
  ENDIF
   
  @ 56,18 SAY INVHDR.CARTONS   PICTURE '@Z 999'
  @ 56,65 SAY XPIECES           PICTURE '9999999'
  @ 56,74 SAY INVHDR.TOTALCHG  PICTURE '9999999.99'

  *-- GET NEXT HEADER RECORD
  SELECT INVHDR
  IF EOF()
    NEWDOC = .F.
    SET DEVICE TO SCREEN
    RETURN

  ELSE
    NEWDOC = .T.
  ENDIF

  IF gcDevice <> 'SCREEN'
    INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
  ENDIF

ENDSCAN
SET DEVICE TO SCREEN
endif
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
IF XINVNAME='Y'
  *@ 03,05 SAY HLINE1
  @ 01,05 SAY HLINE1
  SELECT INVHDR
ENDIF

@ 01,60 SAY XINVOICE
@ 01,75 SAY INVDATE        

IF XINVNAME='Y'
    @ 02,05 SAY HLINE2    
ENDIF

* LINE 3
IF XINVNAME='Y'
  @ 03,05 SAY HLINE3    
ENDIF

@ 03,75 SAY SUBSTR( CDIVISION, 1, 14 )  

IF XINVNAME='Y'
  @ 04,05 SAY HLINE4
ENDIF
  
* LINE 5
IF XINVNAME='Y'
  @ 05,05 SAY XPHONE  SIZE 1,16     
ENDIF

@ 05,75 SAY ORDER    

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
  
llAltShp=OrdHdr.Alt_ShpTo
IF llAltShp
  XSTADDR3 = TRIM(laShipTo[3]) 
  XSTADDR4 =TRIM(laShipTo[4])
  XSTADDR5 = TRIM(laShipTo[5])
  
ELSE     
  XSTADDR3 = TRIM(laShipTo[3]) + ' ' +TRIM(laShipTo[4]) + ' ' + laShipTo[5]
 
ENDIF 
   
    IF LEN(TRIM(laShipTo[2])) =0
      XSTADDR2 = laShipTo[3]
      IF llAltShp 
        XSTADDR3 = XSTADDR4
        XSTADDR4 = XSTADDR5
        XSTADDR5 = ''
      ELSE
        XSTADDR3 = ''
      ENDIF 
   ENDIF

SELECT INVHDR

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 07,14 SAY "Remit to :"
  @ 07,25 SAY lcFacName 
ENDIF    
@ 07,75 SAY PIKTKT        
IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 08,25 SAY laFactor[1]
ENDIF  

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 09,25 SAY laFactor[2]
ENDIF

@ 09,75 SAY APPROVAL      

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 10,25 SAY laFactor[3]
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)  
  @ 11,25 SAY LEFT(laFactor[4] + IIF(!EMPTY(laFactor[4]),' ' + LEFT(laFactor[5],23) ,laFactor[5]),35)
ENDIF

IF llPrnFact AND !EMPTY(INVHDR.CFACCODE)
  @ 12,25 SAY "Ref. Account:"
  @  12,39 SAY lcFACTACCT
ENDIF
  
@ 13,06 SAY XBTNAME
@ 13,56 SAY XSTNAME
@ 14,06 SAY XBTADDR1
@ 14,56 SAY SUBSTR(XSTADDR1,1,25)
@ 15,06 SAY XBTADDR2
@ 15,56 SAY SUBSTR(XSTADDR2,1,25)
@ 16,06 SAY SUBSTR(XBTADDR3,1,45)
@ 16,56 SAY SUBSTR(XSTADDR3,1,29)
IF llAltShp

  @ 17,56 SAY ALLTRIM(SUBSTR(XSTADDR4,1,29))
  
  @ 18,56 SAY ALLTRIM(SUBSTR(XSTADDR5,1,29))
ENDIF

@ 21,02 SAY ACCOUNT
@ 21,12 SAY LEFT(CUSTPO,10)
@ 21,23 SAY IIF(!EMPTY(lcDist),lcDist,STORE)

@ 21,35 SAY DEPT

@ 21,41 SAY LEFT(PTERMS,21)
@ 21,64 SAY REP1                         
@ 21,68 SAY REP2
@ 21,72 SAY SUBSTR(PSHIPVIA,1,13)

ROW = 23
@ 23,20 SAY lcScale1
@ 24,20 SAY lcScale2
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

ROW = 49

*@ ROW+1,12 SAY 'C O N T I N U E D ...'
@ ROW,12 SAY 'C O N T I N U E D ...'

@ 50,09 SAY INVHDR.PHONE PICTURE lcPhonPict SIZE 1,16

SELECT INVHDR
@ 50,56 SAY 'MERCHANDISE'
@ 50,75 SAY '**********'
ROW = ROW + 1


*:B802642,1 ABD  if the line Begin With '*'  Don't Print It. [ Begin ]
*@ 51,07 SAY INVHDR.NOTE1
*@ 53,07 SAY INVHDR.NOTE2
@ 51,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE1),1) = '*','',INVHDR.NOTE1)
@ 53,07 SAY IIF(LEFT(ALLTRIM(INVHDR.NOTE2),1) = '*','',INVHDR.NOTE2)
*:B802642,1 ABD 12/02/1999 [ End ]

@ 56,16 SAY INVHDR.CARTONS PICTURE '@Z 999'
@ 56,65 SAY '*******'
@ 56,75 SAY '**********'

*:**************************************************************************
*! Name      : lfFacMsg
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 02/19/2002
*! Purpose   : Function to get Factor Statement from the User
*!             [Validation function for the Push button Factor Statement]
*:**************************************************************************
*! Called from : Option Grid    [Factor Statement option]
*:**************************************************************************
*! Calls       : gfOptMsg()
*:**************************************************************************
*
FUNCTION lfFacMsg
PARAMETER llDummy
llDummy = .T.
PRIVATE laFacMsg
DECLARE laFacMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laFacMsg[1,1] = 'lcRpFac1'        && 1st. line Variable
laFacMsg[2,1] = 'lcRpFac2'        && 2nd. line Variable
laFacMsg[3,1] = 'lcRpFac3'        && 2nd. line Variable

laFacMsg[1,2] = 75                && Line length

IF EMPTY(lcRpFac1) AND EMPTY(lcRpFac2) AND EMPTY(lcRpFac3)

  IF FILE(gcDataDir+lcFacStNam+'.MEM')
    RESTORE FROM gcDataDir+lcFacStNam+'.MEM' ADDITIVE
  ENDIF

  =gfOptMsg('laFacMsg','Factor Statement')

  SET MEMOWIDTH TO 75              && the length of the memo field.
  SAVE TO gcDataDir+lcFacStNam+'.MEM' ALL LIKE lcRpFac*

ELSE
  =gfOptMsg('laFacMsg','Factor Statement')
ENDIF
RETURN llDummy
*-- End of lfFacMsg

*!**************************************************************************
*! Name      : lfRevPrSku
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2002
*! Purpose   : Print the style Skus for a specific account.
*!**************************************************************************
*
FUNCTION lfRevPrSku
PARAMETER llDummy
llDummy = .F.
lcStrToPrn = ''
IF !SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  RETURN llDummy
ENDIF
llDummy = .T.
PRIVATE lcAlias
lcAlias = ALIAS()
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
   SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style AND lnI < 9
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
  = SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  DO WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style AND lnI < 9
    lcStrToPrn = lcStrToPrn + 'SKU N#' + SUBSTR(Pack_Id,1,lnDime1) + ' '
    DO WHILE Type+Account+Style = ;
             'S'+InvLine.Account+InvLine.Style AND !EOF()
      lcI = STR(lnI,1)
      lcStrToPrn = lcStrToPrn + Scale.Sz&lcI+':'+laSku(lnI) + ' '
      lnI = lnI + 1
      SKIP
      IF lnI = 5 .OR. lnI = 9
        EXIT
      ENDIF
    ENDDO
    lcStrToPrn = lcStrToPrn + CHR(13)
    *@ ROW,00 SAY lcStrToPrn
    *ROW = ROW + 1
  ENDDO  
ELSE
  lcStrToPrn = Sku + 'CUSTOMER SKU #'
  *@ ROW,00 SAY Sku
  *@ ROW,16 SAY 'CUSTOMER SKU #'
  *ROW=ROW+1
ENDIF
IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
RETURN llDummy
*-- End of lfRevPrSku.

*!**************************************************************************
*! Name      : lfRevSclSz
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2002
*! Purpose   : To get the scale and size.
*!**************************************************************************
*
FUNCTION lfRevSclSz
PARAMETER lcDummy
lcDummy = ''
PRIVATE lcAlias  , lcX , lnCountSc , lcKey , lnSclCnt , lcScaleSz
lcOldInv   = INVHDR.INVOICE
lcAlias = ALIAS()
SELECT InvLine
lcKey = Invoice + Style
SEEK InvHdr.Invoice
lnSclCnt = 0

STORE SPACE(0) TO laScale
*wait wind lckey + 'sameh saiid ezzat' timeout 5 
SCAN WHILE Invoice = InvHdr.Invoice  AND lnSclCnt < lcScalCont + 1
  IF ASCAN(laScale,INVLINE.SCALE) = 0 AND SEEK('S'+INVLINE.SCALE,'SCALE')
    lnSclCnt = lnSclCnt + 1
    laScale[lnSclCnt,1] = SCALE.Scale
    FOR lnCountSc = 1 TO 8 
      lcX = STR(lnCountSc,1)
      laScale[lnSclCnt,lnCountSc+1] = SCALE.SZ&lcX
    ENDFOR
  ENDIF
ENDSCAN    
=SEEK(lcKey,'InvLine')
*wait wind lckey + space(10) + alias() timeout 5
SELECT (lcAlias)
RETURN lcDummy
*-- End of lfRevSclSz.

*!**************************************************************************
*! Name      : lfOldStyle
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2002
*! Purpose   : To check if style is changed.
*!**************************************************************************
*
FUNCTION lfOldStyle
PARAMETER lcDummy
lcDummy = ''
IF !(lcOldStyle == InvLine.Invoice + SUBSTR(InvLine.Style,1,lnStyleLen))
  lcOldStyle = InvLine.Invoice + SUBSTR(InvLine.Style,1,lnStyleLen)
ENDIF
RETURN lcDummy
*-- End of lfOldStyle.

*!**************************************************************************
*! Name      : lfNewStye
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 12/18/2002
*! Purpose   : assigne empty to the old style to reprint the style again at 2 page.
*!**************************************************************************
*: B120846,1 ABD - [Begin]
FUNCTION lfNewStye

PARAMETER lcDummy
lcDummy = ''

lcOldStyle  = ''

RETURN lcDummy
*- End OF lfNewStye
*: B120846,1 ABD - [End]
*!**************************************************************************