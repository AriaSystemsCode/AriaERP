*:***************************************************************************
*: Program file  : ARPINVSY.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR SHARON YOUNG.
*: Date          : 19/04/2005
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST(BWA)			
*: Tracking Job Number: C126262 (This invoice taken from form K)
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVSY
*:***************************************************************************
*:Modifications :
*:***************************************************************************

PRIVATE laInvScal , laScalStr , Stylen

DIMENSION laInvScal[1,10] , laScalStr[5]
STORE '' TO laInvScal , laScalStr

XNOTE_LOOP = .F.  && Flag to indicate whether we have finished printing the Notepad or not.
XTAX       = IIF(gfGetMemVar("M_TAX",gcAct_Comp)='Y', .T. , .F.)  && (M_TAX='Y')
XTAX_DESC  = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE  = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH  = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr  = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

*--llCScale : to know that I get all the scale string like(AAB--> if Extended) or (C)
llCScale  = .F.
lcSizScal = ''
XDUNS     = gfGetMemVar('XDUNS',gcAct_Comp)
RELE ALL LIKE M_*

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

STORE SPACE(0) TO Stylen
Stylen = lnLenth + lnClrLen + 1          && 1 FOR "-"

NEWDOC = .T.
MAXROW = 47
lcDist = ' '

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
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]                  TO HLINE4

lnNotLine = 1
STORE .F. TO llNoRec

PRIVATE lnLoop , llPhonExst
llPhonExst = .F.
FOR lnLoop = 1 TO 4
  IF ALLTRIM(laCompAdd[lnLoop]) = ALLTRIM(TRANSFORM(lcCompPhon , lcPhonPict))
    llPhonExst = .T.
    HLINE5 = ''
  ENDIF
ENDFOR
IF !llPhonExst
  STORE TRANSFORM(lcCompPhon , lcPhonPict) TO HLINE5
ENDIF  

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

MSG1 = lcRpMsg1
MSG2 = lcRpMsg2
MSG3 = lcRpMsg3

SET DEVICE TO PRINT

XINVNAME = lcPrnComp
gcPhnFrmt = lcPhonPict
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ELSE
  IF EMPTY(lcTempInv) OR !USED(lcTempInv)
    =lfCreatTmp()
  ELSE
    SELECT (lcTempInv)
    ZAP
  ENDIF
  =lfGetScale()
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)

*--Section collect the data.
SELECT INVHDR
SCAN FOR &lcASExp
  WAIT WINDOW 'Selecting Records For The Report ...' + INVHDR.Invoice NOWAIT
  =lfCollData()
ENDSCAN

*--Section break the relation and creat the new one with the new temp file.
SELECT INVHDR
SET RELATION OFF INTO INVLINE
SET RELATION TO INVHDR.INVOICE INTO &lcTempInv ADDITIVE

SELECT INVLINE
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN

SELECT (lcTempInv)
LOCATE
SET RELATION TO EVAL(lcTempInv+'.STYLE') INTO STYLE ADDITIVE
SET RELATION TO "S" + EVAL(lcTempInv+'.Account') + EVAL(lcTempInv+'.STYLE') INTO SPCK_LIN ADDITIVE

             *-- End Section of the relatoins --*
*--Print the records.
SELECT INVHDR
SCAN FOR &lcASExp

  WAIT WINDOW 'Printing Records For The Report ...' + Invoice NOWAIT
  XINVOICE = INVHDR.INVOICE

  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------

   IF NEWDOC
      STORE 0.00 TO XPIECES, XSUBTOTAL

      SELECT INVHDR
      NEWDOC    = .F.
      XORDER    = ORDER
      XPHONE    = INVHDR.PHONE    
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

      SELE CUSTOMER
      IF !EMPTY(DIST_CTR)
        lcDist = DIST_CTR
        = SEEK('S'+XACCOUNT+lcDist)
      ENDIF

      *--Find the invoice lines.
      SELECT (lcTempInv)
      SEEK XINVOICE
     
      *-- GET THE DESCRIPTION ABOUT THE CODES
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
      PRINTFCTR = llPrnFact           && Comes from form settings file
      XFNAME  = lcFacName             && Factor company name.
      XFADDR1 = laFactor[1]           && Factor address#1.
      XFADDR2 = laFactor[2]
      XFADDR3 = laFactor[3]
      IF LEN(TRIM(laFactor[2])) = 0  && If address#2 is empty fill it with address#3.
        XFADDR2 = laFactor[3]
        XFADDR3 = ''
      ENDIF

      CURLOOP = '1'
      STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
   ENDIF
   *--END NEWDOC

   *-----------------
   * START PRINT
   *-----------------
   * LINE 2
   
   SELECT INVHDR
   @ 02,40 SAY INVDATE
   @ 02,50 SAY 'INVOICE: '+XINVOICE
   @ 02,68 SAY 'PO: '+INVHDR.CUSTPO

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
   
   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
      @ 04,50 SAY 'R E M I T   T O:'
   ENDIF

   * LINE 5
   IF XINVNAME='Y'
      @ 05,05 SAY HLINE3
   ENDIF

   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
     @ 05,50 SAY XFNAME
   ENDIF

   * LINE 6
   IF XINVNAME='Y'
      @ 06,05 SAY HLINE4
   ENDIF

   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
     @ 06,50 SAY XFADDR1
   ENDIF

   * LINE 7
   IF XINVNAME='Y'
      @ 07,05 SAY HLINE5
   ENDIF

   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
      @ 07,50 SAY XFADDR2
   ENDIF

   * LINE 8
   IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
      @ 08,50 SAY XFADDR3
   ENDIF

   *--Bill to and ship to addresses.
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

   SELECT InvHdr
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
   @ 17,09 SAY LEFT(CUSTPO,11)
   @ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
   @ 17,30 SAY DEPT
   @ 17,36 SAY ORDER
   @ 17,44 SAY LEFT(PTERMS,15)
   @ 17,60 SAY REP1
   @ 17,64 SAY REP2
   @ 17,68 SAY LEFT(PSHIPVIA,12)

   *------------------
   * PRINT SCALES
   *------------------
   STORE '' TO laScalStr
   lnSizes = 0
   lcInvNo   = InvHDR.Invoice
   DO lpGtScl WITH lcInvNo
   SELECT Scale
   Row = 19

   FOR lnI = 1 TO ALEN(laScalStr,1)
     IF !EMPTY(laScalStr[lnI])
       SEEK 'S' + laScalStr[lnI]
       @ Row,22 SAY LEFT(laScalStr[lnI],1)
	   lnSizes = ASUBSCRIPT(laInvScal,ASCAN(laInvScal,"SCALE_" + laScalStr[lnI]),1)
       
       @ Row,26 SAY PADL(laInvScal[lnSizes,2] ,3,' ')
       @ Row,30 SAY PADL(laInvScal[lnSizes,3] ,3,' ')
       @ Row,34 SAY PADL(laInvScal[lnSizes,4] ,3,' ')
       @ Row,38 SAY PADL(laInvScal[lnSizes,5] ,3,' ')
       @ Row,42 SAY PADL(laInvScal[lnSizes,6] ,3,' ')
       @ Row,46 SAY PADL(laInvScal[lnSizes,7] ,3,' ')
       @ Row,50 SAY PADL(laInvScal[lnSizes,8] ,3,' ')
       @ Row,54 SAY PADL(laInvScal[lnSizes,9] ,3,' ')
       @ Row,58 SAY PADL(laInvScal[lnSizes,10] ,3,' ')

       Row = Row + 1
     ENDIF
   ENDFOR

   *---------------------------------------------------------------
   * [1] LINE ITEM PRINT LOOP
   *---------------------------------------------------------------
   SELECT (lcTempInv)
   =SEEK(XINVOICE)
   XSTORE = STORE
   XSCALE =  ' '
   ROW    = 25
  
   DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
      SELECT (lcTempInv)
      IF EOF() .OR. INVOICE <> XINVOICE .OR. ROW >= MAXROW
        IF ROW >= MAXROW
          PRIVATE lnTmpLnRec
          lnTmpLnRec = IIF(EOF(lcTempInv) , 0 , RECNO(lcTempInv))
          
          =lfGetcont()
          =lfGetInHdr()
          
          IF lnTmpLnRec > 0
            GO (lnTmpLnRec) IN (lcTempInv)
          ENDIF

          LOOP
        ENDIF
        EXIT
      ENDIF
      IF TOTQTY = 0
         SKIP
         LOOP
      ENDIF
      KEY = &lcTempInv..STYLE
      SELECT STYLE
      SEEK KEY
      SELECT (lcTempInv)
      
      @ ROW,00 SAY LEFT(STYLE,lnLenth)
      @ ROW,14 SAY SUBSTR(STYLE,lnClrPos,lnClrLen)
      @ ROW,22 SAY STYLE.SCALE

      @ ROW,26 SAY IIF( QTY1 # 0 , PADR(ALLTRIM(STR(QTY1)) ,3,' ') , "")
      @ ROW,30 SAY IIF( QTY2 # 0 , PADR(ALLTRIM(STR(QTY2)) ,3,' ') , "")
      @ ROW,34 SAY IIF( QTY3 # 0 , PADR(ALLTRIM(STR(QTY3)) ,3,' ') , "")
      @ ROW,38 SAY IIF( QTY4 # 0 , PADR(ALLTRIM(STR(QTY4)) ,3,' ') , "")
      @ ROW,42 SAY IIF( QTY5 # 0 , PADR(ALLTRIM(STR(QTY5)) ,3,' ') , "")
      @ ROW,46 SAY IIF( QTY6 # 0 , PADR(ALLTRIM(STR(QTY6)) ,3,' ') , "")
      @ ROW,50 SAY IIF( QTY7 # 0 , PADR(ALLTRIM(STR(QTY7)) ,3,' ') , "")
      @ ROW,54 SAY IIF( QTY8 # 0 , PADR(ALLTRIM(STR(QTY8)) ,3,' ') , "")
      @ ROW,58 SAY IIF( QTY9 # 0 , PADR(ALLTRIM(STR(QTY9)) ,3,' ') , "")

      @ ROW,62 SAY TOTQTY PICTURE '99999'
*      @ ROW,62 SAY TOTQTY PICTURE '999'

      *--SUM INVOICE TOTALS.
      XLINETOT   = PRICE * TOTQTY
      XPIECES    = XPIECES + TOTQTY
      XSUBTOTAL  = XSUBTOTAL+XLINETOT

      @ ROW,63  SAY PRICE     PICTURE '9999.99'
      @ ROW,71  SAY XLINETOT  PICTURE '999999.99'
*      @ ROW,66  SAY PRICE     PICTURE '999.99'
*      @ ROW,73  SAY XLINETOT  PICTURE '9999.99'

      ROW = ROW + 1

      @ ROW,00 SAY 'STYLE DESC : '+STYLE.Desc
      ROW = ROW + 1
      
      *-- GET NEXT LINE ITEM
      SELECT (lcTempInv)
      SKIP
   ENDDO

   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(&lcTempInv..INVOICE = XINVOICE ,'1','0')

   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ ROW+1,12 SAY 'C O N T I N U E D ...'
      SELECT INVHDR
      @ 55,01 SAY XPHONE SIZE 1,16
      @ 55,18 SAY INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
      
      IF LEN(TRIM(MSG1 + MSG2)) >0
         @ 56,10 SAY MSG1
         @ 57,10 SAY MSG2
      ENDIF
      @ 60,02 SAY INVHDR.PIKTKT
      @ 60,09 SAY ShipDate
      @ 60,19 SAY '***'
      @ 60,25 SAY '*****'
      @ 60,35 SAY XINVOICE
      @ 60,71 SAY '******.**'
      LOOP
   ENDIF

   *--Print invoice Notepad.
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
         IF ROW >= MAXROW      && if there is a line will de delete from 
           =lfGetcont()        && the memo field
           =lfGetInHdr()
         ENDIF
         lnNotLine = lnNotLine + 1
       ENDDO
     
       IF !XNOTE_LOOP
         @ ROW,02 SAY '* -- END OF NOTES -- *'
         lnNotLine = 1
         ROW = ROW + 1 
       ELSE
         @ ROW+1,12 SAY 'C O N T I N U E D ...'
         SELECT INVHDR
         
         @ 55,01 SAY XPHONE SIZE 1,16
         @ 55,18 SAY INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
         IF LEN(TRIM(MSG1 + MSG2)) >0
           @ 56,10 SAY MSG1
           @ 57,10 SAY MSG2
         ENDIF
         @ 60,02 SAY INVHDR.PIKTKT
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

   @ ROW,61 SAY XPIECES              PICTURE '999999'
   @ ROW,73 SAY XSUBTOTAL            PICTURE '9999999.99'

*   @ ROW,61 SAY XPIECES              PICTURE '9999'
*   @ ROW,73 SAY XSUBTOTAL            PICTURE '9999.99'
   ROW=ROW+1

   IF DISCOUNT<>0
      @ ROW,12 SAY 'TOTAL - D I S C O U N T'
      @ ROW,69 SAY DISCOUNT         PICTURE '99999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  

     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
   ENDIF

   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,12 SAY 'TOTAL - F R E I G H T'
      @ ROW,73 SAY WKAMT            PICTURE '9999999.99'
*      @ ROW,73 SAY WKAMT            PICTURE '9999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR.TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY lcTaxRefr
     ENDIF  
     @ ROW,64 SAY XSTRING_RATE + '%'
     @ ROW,70 SAY INVHDR.TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,64 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  

     IF UPPER(ALLTRIM(gcContCode))='CANADA' AND InvHdr.nHSTAmt <> 0 
       @ Row,12 SAY 'H S T   T A X'
       @ Row,64 SAY STR(InvHdr.nHSTRate,5,2)+'%'
       @ Row,70 SAY InvHdr.nHSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
   ENDIF  

   @ 55,01 SAY XPHONE SIZE 1,16
   @ 55,18 SAY INVHDR.NOTE1 + ' ' + INVHDR.NOTE2

   IF LEN(TRIM(MSG1 + MSG2)) >0
      @ 56,10 SAY MSG1
      @ 57,10 SAY MSG2
   ENDIF

   IF INVHDR.APPROVAL<>' ' .AND. UPPER(APPROVAL)<>'DEC'
      @ 59,56 SAY 'APPROVAL: ' + INVHDR.APPROVAL
   ENDIF

   @ 60,00 SAY INVHDR.PIKTKT
   @ 60,08 SAY ShipDate
   @ 60,17 SAY CARTONS               PICTURE '@Z 999'
   @ 60,25 SAY WEIGHT                PICTURE '@Z 99.99'
   @ 60,35 SAY INVHDR.INVOICE
   @ 60,73 SAY INVHDR.TOTALCHG      PICTURE '9999999.99'
*   @ 60,73 SAY INVHDR.TOTALCHG      PICTURE '9999.99'
   SELECT INVHDR 

   *-- GET NEXT HEADER RECORD
   SELECT INVHDR
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     SELECT (lcTempInv)
	 SET RELATION OFF INTO STYLE
 	 SET RELATION OFF INTO SPCK_LIN
	 SELECT INVHDR
     RETURN
   ELSE
     NEWDOC = .T.
   ENDIF
   
   IF gcDevice <> 'SCREEN'
     INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
   ENDIF
ENDSCAN

SELECT (lcTempInv)
SET RELATION OFF INTO STYLE
SET RELATION OFF INTO SPCK_LIN
SELECT INVHDR

SET DEVICE TO SCREEN
WAIT CLEAR
RETURN
                       *-- End of the Program --*
*!*************************************************************
*! Name      : lpGtScl
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
*! Purpose   : Build a string holding the first uniqe 5 scales
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
*! Example     : =lpGtScl()
*!*************************************************************
PROCEDURE lpGtScl
PARAMETERS lcInvNo
PRIVATE lcAlias , lcKey , lcCheckScl , lnFill , lnContAry

STORE SPACE(0) TO lcCheckScl
STORE 0 TO lnFill , lnContAry
*-- Save the current alias.
lcAlias = ALIAS()

SELECT (lcTempInv)
lcKey = EVAL(KEY())

SEEK lcInvNo
SCAN REST WHILE INVOICE + STR(LINENO,6) = lcInvNo .AND. lnContAry <= 5
  IF !(LEFT(Scale,2) $ lcCheckScl)
    lcCheckScl = IIF(EMPTY(lcCheckScl) , "" , lcCheckScl + ",") + LEFT(Scale,2)
    lnFill = lnFill + 1
    laScalStr[lnFill] = LEFT(Scale,2)
  ENDIF
ENDSCAN

SELECT (lcTempInv)
=SEEK(lcKey)

SELECT (lcAlias)

*-- End of lpGtScl.
*!*************************************************************
*! Name      : gfGetZone
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : THE PROGRAM
*!*************************************************************
*! Calls       : ......
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =gfGetZone()
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

*-- End of gfGetZone.
*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
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

@ 55,01 SAY XPHONE SIZE 1,16
@ 55,18 SAY INVHDR.NOTE1 + ' ' + INVHDR.NOTE2
       
IF LEN(TRIM(MSG1 + MSG2)) >0
  @ 56,10 SAY MSG1
  @ 57,10 SAY MSG2
ENDIF
@ 60,02 SAY INVHDR.PIKTKT
@ 60,09 SAY InvHdr.ShipDate
@ 60,19 SAY '***'
@ 60,25 SAY '*****'
@ 60,35 SAY XINVOICE
@ 60,71 SAY '******.**'

*-- End of lfGetcont.
*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
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
@ 02,68 SAY 'PO: '+INVHDR.CUSTPO

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
   
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 04,50 SAY 'R E M I T   T O:'
ENDIF

* LINE 5
IF XINVNAME='Y'
  @ 05,05 SAY HLINE3
ENDIF

IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 05,50 SAY XFNAME
ENDIF

* LINE 6
IF XINVNAME='Y'
  @ 06,05 SAY HLINE4
ENDIF

IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 06,50 SAY XFADDR1
ENDIF

* LINE 7
IF XINVNAME='Y'
  @ 07,05 SAY HLINE5
ENDIF

IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
  @ 07,50 SAY XFADDR2
ENDIF

* LINE 8
IF PrintFctr AND !EMPTY(InvHdr.cFacCode)
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
@ 17,09 SAY LEFT(CUSTPO,11)
@ 17,21 SAY IIF(CURLOOP='1',XSTORE,'')
@ 17,30 SAY DEPT
@ 17,36 SAY ORDER
@ 17,44 SAY LEFT(PTERMS,15)
@ 17,60 SAY REP1
@ 17,64 SAY REP2
@ 17,68 SAY LEFT(PSHIPVIA,12)

*------------------
* PRINT SCALES
*------------------
lcScalStr = SPACE(0)
lcInvNo   = InvHDR.Invoice
DO lpGtScl WITH lcInvNo
SELECT Scale
Row = 19
FOR lnI = 1 TO ALEN(laScalStr,1)
  IF !EMPTY(laScalStr[lnI])
    SEEK 'S' + laScalStr[lnI]
    @ Row,22 SAY LEFT(laScalStr[lnI],1)
	lnSizes = ASUBSCRIPT(laInvScal,ASCAN(laInvScal,"SCALE_" + laScalStr[lnI]),1)
       
    @ Row,26 SAY PADL(laInvScal[lnSizes,2] ,3,' ')
    @ Row,30 SAY PADL(laInvScal[lnSizes,3] ,3,' ')
    @ Row,34 SAY PADL(laInvScal[lnSizes,4] ,3,' ')
    @ Row,38 SAY PADL(laInvScal[lnSizes,5] ,3,' ')
    @ Row,42 SAY PADL(laInvScal[lnSizes,6] ,3,' ')
    @ Row,46 SAY PADL(laInvScal[lnSizes,7] ,3,' ')
    @ Row,50 SAY PADL(laInvScal[lnSizes,8] ,3,' ')
    @ Row,54 SAY PADL(laInvScal[lnSizes,9] ,3,' ')
    @ Row,58 SAY PADL(laInvScal[lnSizes,10] ,3,' ')

    Row = Row + 1
  ENDIF
ENDFOR
Row = 25          && NEW LINE in the function

*-- End of lfGetInHdr.
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
*! Purpose   : Creat the Tmp. file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCreatTmp()
*!*************************************************************
FUNCTION lfCreatTmp

SELECT INVLINE
=AFIELDS(laTmpStru)
lnTmpStru = ALEN(laTmpStru,1)

=lfAddField("laTmpStru", "Qty9"  , "N" ,6  ,0)

FOR lnCrtTmp = 1 TO 9
  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("laTmpStru", "Sz"  + lcNumSiz , "C" , 5 ,0)
ENDFOR

CREATE TABLE (gcWorkDir + lcTempInv) FROM ARRAY laTmpStru

INDEX ON LEFT(STYLE,Stylen) + INVOICE TAG lcTempInv OF (gcWorkDir + lcTempInv +'.CDX') ADDITIVE
INDEX ON ORDER + STR(LINENO,6) + INVOICE TAG INVLINEO OF (gcWorkDir + lcTempInv +'.CDX') ADDITIVE
INDEX ON STYLE + INVOICE + STR(LINENO,6) TAG INVLINES OF (gcWorkDir + lcTempInv +'.CDX') ADDITIVE
INDEX ON INVOICE + STR(LINENO,6) TAG INVLINES OF (gcWorkDir + lcTempInv +'.CDX') ADDITIVE

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfGetScale
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
*! Purpose   : Collect the size data.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGetScale()
*!*************************************************************
FUNCTION lfGetScale
PRIVATE lcAlias , Lckey , lnChck9Fld

lcAlias = SELECT(0)
SELECT SCALE
Lckey = EVAL(KEY())

*-- I add this suffix "SCALE_" to unique the scale.
SELECT DISTINCT "SCALE_" + LEFT(SCALE.SCALE,2) , "" , "" , "" , "" , "" , "" ,;
 "" , "" , "" FROM SCALE INTO ARRAY laInvScal

SELECT SCALE
FOR lnScl = 1 TO ALEN(laInvScal,1)
  IF SEEK( "S" + SUBSTR(laInvScal[lnScl,1],7,2) , 'SCALE')
    lnChck9Fld = 0
    SCAN REST WHILE TYPE + LEFT(SCALE,2) + PREPAK = "S" + SUBSTR(laInvScal[lnScl,1],7,2)
      FOR lnSz = 1 TO SCALE.CNT
        lcSizScl = "Sz" + ALLTRIM(STR(lnSz))
      
        lnSzChk = lnSz + lnChck9Fld                && check for the 9 fields.
        IF lnSzChk = 10
          EXIT
        ENDIF
        laInvScal[lnScl , lnSzChk + 1] = SCALE.&lcSizScl
      ENDFOR
      lnChck9Fld = lnSzChk
    ENDSCAN
  ENDIF
ENDFOR
SELECT(lcAlias)
=SEEK(Lckey)

*--End of lfGetScale.
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
*! Purpose   : Collect the invline data.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCollData()
*!*************************************************************
FUNCTION lfCollData
PRIVATE lcAlias , lcRcNoHdr , lcoldOrd

STORE SPACE(0) TO lcAlias , lcRcNoHdr , lcoldOrd
lcAlias = ALIAS()
lcRcNoHdr = EVAL(KEY())
  
SELECT (lcTempInv)
lcoldOrd = ORDER()
SET ORDER TO TAG lcTempInv

SELECT INVLINE
=SEEK(INVHDR.INVOICE)
SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
  
  SCATTER MEMVAR MEMO  
  SELECT (lcTempInv)
  =SEEK("S" + INVLINE.SCALE , 'SCALE')

  *IF SEEK(INVHDR.INVOICE + LEFT(M.SCALE,2) , lcTempInv)
  IF SEEK( LEFT(INVLINE.STYLE,Stylen) + INVLINE.INVOICE , lcTempInv)
  
    lnScalUpdt = ASUBSCRIPT(laInvScal,ASCAN(laInvScal,"SCALE_" + LEFT(&lcTempInv..SCALE,2)),1)
    
    FOR lnScle = 1 TO SCALE.CNT
      lcSizScl = "Sz" + ALLTRIM(STR(lnScle))
      lcQtyVal = "Qty" + ALLTRIM(STR(lnScle))

      FOR lnUpdt = 1 TO 9
        lcSizTmp = "Sz" + ALLTRIM(STR(lnUpdt))
        lcQtyTmp = "Qty" + ALLTRIM(STR(lnUpdt))
        IF SCALE.&lcSizScl == &lcTempInv..&lcSizTmp AND (M.&lcQtyVal # 0)
          REPLACE &lcTempInv..&lcQtyTmp WITH &lcTempInv..&lcQtyTmp + M.&lcQtyVal
          EXIT
        ENDIF
      ENDFOR
    ENDFOR
    REPLACE &lcTempInv..TotQTY WITH &lcTempInv..TotQTY + M.TotQTY
  ELSE
    APPEND BLANK
    GATHER MEMVAR MEMO
    lnScalPOs = ASUBSCRIPT(laInvScal,ASCAN(laInvScal,"SCALE_" + LEFT(&lcTempInv..SCALE,2)),1)
    
    FOR lnScl = 1 TO 9
      lcSizFld = "Sz" + ALLTRIM(STR(lnScl))
      IF !EMPTY(laInvScal[lnScalPOs , lnScl+1])
        REPLACE &lcTempInv..&lcSizFld WITH laInvScal[lnScalPOs,lnScl+1]
      ENDIF
    ENDFOR
  ENDIF
ENDSCAN

SELECT (lcTempInv)
SET ORDER TO TAG &lcoldOrd

=SEEK(lcRcNoHdr)
SELECT(lcAlias)

*--End of lfCollData.
*!*************************************************************
*! Name      : lfAddField
*! Developer : BASSEM RAFAAT ERNEST(BWA)
*! Date      : 19/04/2005
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec

lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.