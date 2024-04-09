*:***************************************************************************
*: Program file  : ARPINVGM.PRG
*: Program desc. : ACCOUNT RECEIVABLE INVOICE FOR GMA ACCOSORIES
*: 				   The program is a conversion from 2.6 ..GMA\INV810Z.PRG	
*! Date          : 03/13/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C101805
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVGM
*:**************************************************************************
*:Modifications :
*: B603705,1 BWA 06/26/2000 Fix the error in the selection grid for this form
*: B603705,1                one of the selection grid disappear and overwrite
*: B603705,1                with other one. [SYREPUVR]
*: B803420,1 BWA 07/19/2000 1)The screen of the invoice broken after printing.
*: B803420,1                2)Print the color desciption instead of the color code as A2.6. 
*: B803420,1                3)Change the factor text.
*: B803420,1                4)The invoice doesn't print in case the invoice made by packs.
*: B803692,1 BWA 09/24/2000 1)Add wait window to let the user know that the program is working.
*: B803692,1                2)Comment the line of codes for the factor address since there is no factor address in the invoice.
*: B803692,1                3)Modify the restoring of the order of the ORDLINE file.
*: B803705,1 BWA 10/01/2000 Fix the bug of some lines in the invoice doesn't print if the invoice has Packs and regular styles in the same time.
*: B803705,1                Fix the bug of when print the invoices the prtflag doesn't change to Yes.[START]
*: B803921,1 BWA 12/20/2000 1) Fix the bug of style discreption print in the same line of the pieces in case there is no scale in the invline file.
*: B803921,1                2) Modify the check of the packid from the spkline file to be in the invline file.
*: B604100,1 BWA 01/10/2001 Invoice form doesn't print Pack description.
*: C200184,1 BWA 04/22/2001 Add the HST tax.
*: C102347,1 BWA 05/13/2001 Repeat printing the total pieces to print it in the last copy.
*: B604546,1 BWA 06/13/2001 Increase the width of the Custpo field.
*: B604625,1 BWA 07/26/2001 Print the company address and comment the printing of the division in the address.
*: B605110,1 SSE 11/19/2001 Fix bug of not consolidating the packs qty of same pack id if its lines are separated
*: C200329,1 HBG 07/05/2002 Nodifications to use the pack with the new key "Pack ID - Color - Size - Version"   
*: HBG 8/12/2002 Print the detail in case the price of the pack = 0 or it is range pack or 
*:               price per pieac, just as if the invoice is created from style not from pack.
*: C200479,1 HBG 31/12/2002 Use 'lRange' & 'lPckPrPiec' fields from INVLINE instead of SPCK_HSR 
*:***************************************************************************

*--Section of the relations & open the files that does not exist in the report generator --*
IF !USED(lcSpckHdr)
  *C200329,1 HBG 07/05/2002 Open SPCK_HDR with new index [Begin]
  *=gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SPCK_HDR','SH' , @lcSpckHdr , .T.)
  =gfOpenFile(gcDataDir+'SPCK_HDR',gcDataDir+'SPCK_HDRVR','SH' , @lcSpckHdr , .T.)
  *C200329,1 [End]
ENDIF

IF !USED(lcSysZone)
  = gfOpenFile(gcSysHome+'Syszones' ,gcSysHome+'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

IF !USED(lcOrdLine)
  *B803420,1 BWA 07/19/2000 Change the index to be the same in the scan.[START]
  *= gfOpenFile(gcDataDir+'Ordline' ,gcDataDir+'Ordlinst', 'SH', @lcOrdLine, .T.)
  = gfOpenFile(gcDataDir+'Ordline' ,gcDataDir+'Ordline', 'SH', @lcOrdLine, .T.)
  *B803420,1 [END]
ENDIF

SELECT InvLine

*B803420,1 BWA 07/19/2000 Change the relation to be the same as the index.[START]
*SET RELATION TO 'O'+order+store+style+STR(lineno,6) INTO &lcOrdLine ADDITIVE
SET RELATION TO 'O'+order+STR(lineno,6) INTO &lcOrdLine ADDITIVE
*B803420,1 [END]

              *-- End of Section of open files --*

*B803420,1 BWA 07/19/2000 Print the color desciption instead of the color code as A2.6. [START]
*--The color length.
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
*B803420,1 [END]

*--Section to get the FromZone
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',gcAct_Comp))
IF gfGetMemVar('M_WareHouse',gcAct_Comp)='Y'
  IF !USED(lcWarHous)
    = gfOpenFile(gcDataDir+'WAREHOUS' ,gcDataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous ),&lcWarHous..UPS,lcFromZone)
ENDIF

*--End Section to get the FromZone --*

           *-- Section of initial the program variables --*

xTax      = gfGetMemVar("M_TAX",gcAct_Comp)='Y'
XTAX_DESC = gfGetMemVar('M_TAX_DESC',gcAct_Comp)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
XTAX_METH = gfGetMemVar('M_TAX_METH',gcAct_Comp)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',gcAct_Comp)
XINVNAME  = lcPrnComp
PRINTFCTR = llPrnFact
STORE .F. TO llNoRec

*B803921,1 BWA 12/20/2000 Initial variables to be used [START]
lcScale   = SPACE(0)
STORE .T. TO llScale
*B803921,1 [END]


HLINE1    = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
STORE lcCompPhon                                           TO HLINE5

IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'

lcInv      = " "     && variable used in Reset the page # after eash invoice.
lnPageNo   = 0
NEWDOC     = .T.
MAXROW     = 50
XNOTE_LOOP = .F.
lnNotLine  = 1

*-- Section of the fixed notes --*
lcNote1  = "PAY ONLY IN U.S FUNDS TO"
lcNote2  = "THE CIT GROUP"

*B803420,1 BWA 07/19/2000 Change the factor text.[START]
*lcNote3  = "P.O. BOX. 7247-8292"
*lcNote4  = "PHILA, PA 19170-8292"
lcNote3  = "P.O. BOX. 1036"
lcNote4  = "CHARLOTTE NC 28201-1036"
*B803420,1 (END)

*-- End Section of the fixed notes --*

SELECT INVHDR
LOCATE FOR &lcRpExp

IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF
lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
SET DEVICE TO PRINT

*B605110,1 Create a temp file for collecting packs ID for same invoice. [Begin]
*C200329,1 HBG 07/05/2002 Add the fields and change the index [Begin] 
*CREATE TABLE (gcWorkDir+lcPackTmp) (Invoice C(6) , Pack_ID C(16) )
*INDEX ON Invoice + Pack_ID TAG (lcPackTmp)
CREATE TABLE (gcWorkDir+lcPackTmp) (Invoice C(6) , Pack_ID C(16) , cpkcolor C(6) , cpcksize C(3) , cPkSzDsc C(5) , cpkversion C(4))
INDEX ON Invoice + Pack_ID + cPkColor + cPckSize + cPkVersion TAG (lcPackTmp)
*-- Temp file to get the total invoiced Qty from each pack
CREATE TABLE (gcWorkDir+lcInvPkTmp) (Invoice C(6) , Pack_ID C(16) , cpkcolor C(6) , cpcksize C(3) , cpkversion C(4) , PackQty N(6))
INDEX ON Invoice + Pack_ID + cPkColor + cPckSize + cPkVersion TAG (lcInvPkTmp)
SELECT InvHdr
SCAN FOR &lcASExp
  SELECT InvLine
  =SEEK(InvHdr.Invoice)
  SCAN REST WHILE Invoice+STR(lineno,6) = InvHdr.Invoice
    IF SEEK(Invoice+Pack_id+cPkColor+cPckSize+cPkVersion,lcInvPkTmp)
      SELECT (lcInvPkTmp) 
      REPLACE &lcInvPkTmp..PackQty WITH &lcInvPkTmp..PackQty + InvLine.TotQty
    ELSE
      INSERT INTO (lcInvPkTmp) (Invoice,Pack_ID,cPkColor,cPckSize,cPkVersion,PackQty);
                        VALUES (InvLine.Invoice,InvLine.Pack_ID,InvLine.cPkColor,;
                                InvLine.cPckSize,InvLine.cPkVersion,InvLine.TotQty)
    ENDIF  
  ENDSCAN  
ENDSCAN
*C200329,1 [End]

SELECT InvHdr
*B605110,1 Create a temp file for collecting packs ID for same invoice. [End]

SCAN FOR &lcASExp
  *B803692,1 BWA 09/24/2000 Add wait window to let the user know that the program is working.[START]
  WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  *B803692,1 [END]
  
  IF lcInv <> Invoice
    lnPageNo   = 0
    lcInv = Invoice
  ENDIF
  XINVOICE = INVOICE
  
  *-----------------------------------------------------------------
  * Get invoice header, line items, and financial history records.
  * If any records are not found, skip to next invoice.
  * Initialize document totals.
  *-----------------------------------------------------------------

  IF NEWDOC
    STORE 0.00 TO XPIECES, XSUBTOTAL
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
      
    =lfSolSpAdr()                && function in the main program (ARPINV) to get the adrress.
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

    *** GET THE DESCRIPTION ABOUT THE CODES
    SELECT CODES
    SET ORDER TO CODES IN CODES
    PTERMS     = gfCodDes(INVHDR.CTERMCODE,'CTERMCODE')
    PSPCINST   = gfCodDes(INVHDR.SPCINST,'SPCINST')
    PSHIPVIA   = gfCodDes(INVHDR.SHIPVIA,'SHIPVIA')
    cDivision  = gfCodDes(Customer.cDivision,'cDivision')
    = gfRltFld(INVHDR.SHIPVIA , @laZone , 'SHIPVIA')
    DO CASE
      CASE 'G' $ lcUpsType
        XZN = gfGetZone(lcUpsType,lcFromZone,SUBSTR(CUSTOMER.cAddress5,1,3))
        XZN = IIF(!EMPTY(XZN),'('+XZN+')',XZN)
      CASE '2' $ lcUpsType
        XZN  = '(12)'
      CASE 'N' $ lcUpsType
        XZN  = '(22)'
      OTHERWISE
        XZN  = ''
    ENDCASE
    PSHIPVIA = IIF(TRIM(PSHIPVIA) = "N/A" , "" ,TRIM(PSHIPVIA)) +XZN

    *----------------------------------------------
    * [FACTOR] NAME & ADDRESS
    *----------------------------------------------
    *B803692,1 Comment the line of codes for the factor address
    *B803692,1 due to there is no factor address in the invoice.[START]
    *STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
    *DO WHILE PRINTFCTR
    *  SELECT SYCFACT
    *  IF SEEK(INVHDR.CFACCODE)
    *    XFNAME  = CFACCOMP
    *    XFADDR1 = CADDRESS1
    *    XFADDR2 = CADDRESS2
    *    XFADDR3 = TRIM(CADDRESS3)+' '+CADDRESS4+' '+CADDRESS5
    *    IF CADDRESS2 = ' '
    *      CADDRESS2 = CADDRESS3
    *      CADDRESS3 = ' '
    *    ENDIF
    *  ELSE
    *    STORE ' ' TO XFNAME,XFADDR1,XFADDR2,XFADDR3
    *  ENDIF
    *  EXIT
    *ENDDO
    *B803692,1 [END]

    CURLOOP = '1'
    STORE 0.00 TO SWEIGHT, SCARTONS, SFREIGHT, SCOD, SINSURANCE, SPIECES, SDOLLARS
  ENDIF
  ****** END NEWDOC
  *-----------------
  * START PRINT
  *-----------------
  =lfGetInHdr()

  *-- [1] LINE ITEM PRINT LOOP
  SELECT INVLINE
  XSTORE = STORE
  XSCALE =  ' '
  ROW    = 27
  STORE 0 TO lnRequire,lnShipped,lnBackOrd
  *HBG Add variable to get shipped Qty and ordered Qty from nPackNo field in case of reguler packs [Begin]
  lnPackNo  = 0
  *HBG [End]
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
     lnBackOrd = 0
     
     *B803420,1 BWA 07/19/2000 Print the color desciption instead of the color code as A2.6. [START]
     =SEEK('N'+SUBSTR(STYLE,lnClrPos,lnClrLen)+'N'+'COLOR','CODES')
     *B803420,1 [END]

     lnRequire=TOTQTY + lfGetBack()
     lnShipped=TOTQTY
     lcOrder = ORDER("SPCK_LIN")
     *C200329,1 HBG 07/05/2002 Change the order for the new index[Begin]  
     *SET ORDER TO TAG SPCK_LIN IN SPCK_LIN
     SET ORDER TO TAG SPCK_LINVR IN SPCK_LIN
     *C200329,1 [End]
     llPack = .F.
     *HBG 8/12/2002 Set the Index befor enter the case [Begin]
     SET ORDER TO SPCK_HDRVR IN SPCK_HDR
     *HBG [END]
     DO CASE

       *B803921,1 BWA 12/20/2000 2) Modify the check of the packid from the spkline file to be in the invline file.[START]
       *CASE !EMPTY(&lcOrdLine..Pack_id) AND;
            (SEEK("P"+Account+&lcOrdLine..Pack_Id+Style,"SPCK_LIN") OR;
             SEEK("P"+"*****"+&lcOrdLine..Pack_Id+Style,"SPCK_LIN"))
         *C200329,1 HBG 07/05/2002 Change the Seek expresion[Begin]   
         *=SEEK("P"+SPCK_LIN.Account+SPCK_LIN.Pack_Id, lcSpckHdr )
         =SEEK("P"+SPCK_LIN.Account+SPCK_LIN.Pack_Id+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize+;
                   SPCK_LIN.cPkVersion, lcSpckHdr )
       *CASE !EMPTY(&lcOrdLine..Pack_id) AND;
            INVLINE.PACK_ID = &lcOrdLine..Pack_Id
       CASE !EMPTY(&lcOrdLine..Pack_id) AND;
            INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion =;
            &lcOrdLine..Pack_Id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion            

         SET ORDER TO SPCK_HDRVR IN SPCK_HDR
         llGetPack = SEEK('P'+INVLINE.Account+INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion,'SPCK_HDR')
         IF !llGetPack 
           llGetPack = SEEK('P*****'+INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion,'SPCK_HDR')
         ENDIF
         llCalcPric = .F.
         IF llGetPack 
           lnPckQty = SPCK_HDR.nPckQty
           *C200479,1 HBG 31/12/2002 Use 'lRange' & 'lPckPrPiec' fields from INVLINE instead of SPCK_HSR [Begin]
           *IF (SPCK_HDR.lRange OR SPCK_HDR.lPckPrPiec OR INVLINE.nPkSlPrice = 0)
           IF (INVLINE.lRange OR INVLINE.lPckPrPiec OR INVLINE.nPkSlPrice = 0)
           *C200479,1 [End]
             llCalcPric = .T.
           ENDIF  
         ENDIF
       *C200329,1 [End]
       
            *B605110,1 Check if this pack id is found in Temp Packs file. [Begin]
            *C200329,1 HBG 07/05/2002 Change the Seek expresion[Begin]    
            *IF SEEK(Invoice+Pack_id,lcPackTmp)
            IF SEEK(Invoice+Pack_id+cPkColor+cPckSize+cPkVersion,lcPackTmp)
            *C200329,1 [End]
              IF !EOF()
                SKIP
                LOOP
              ENDIF              
            ELSE
              *C200329,1 HBG 07/05/2002 Update the new fields[Begin]    
              *INSERT INTO (lcPackTmp) (Invoice,Pack_ID) VALUES (InvLine.Invoice,InvLine.Pack_ID)
              IF SEEK('S'+LEFT(InvLine.cPckSize,1),'SCALE')
                lcSz = RIGHT(InvLine.cPckSize,1)
                lcpkSzDsc = SCALE.Sz&lcSz
              ELSE
                lcpkSzDsc = '*****'    
              ENDIF  
              INSERT INTO (lcPackTmp) (Invoice,Pack_ID,cPkColor,cPckSize,cPkVersion,cpkSzDsc);
                               VALUES (InvLine.Invoice,InvLine.Pack_ID,InvLine.cPkColor,;
                                       InvLine.cPckSize,InvLine.cPkVersion,lcpkSzDsc)
              *C200329,1 [End]   
            ENDIF  
            *B605110,1 Check if this pack id is found in Temp Packs file. [End]
         *C200329,1 HBG 07/05/2002 Change Seek expresion [Begin]     
         *=SEEK("P"+INVLINE.Account+INVLINE.Pack_Id, lcSpckHdr )
         =SEEK("P"+INVLINE.Account+INVLINE.Pack_Id+INVLINE.cPkColor+;
                   INVLINE.cPckSize+INVLINE.cPkVersion, lcSpckHdr )
         *C200329,1 [End]
         *B604100,1 BWA 01/10/2001 Invoice form doesn't print Pack description.[START]
         IF EOF(lcSpckHdr)
           *C200329,1 HBG 07/05/2002 Change Seek expresion [Begin]     
           *=SEEK("P"+"*****"+INVLINE.Pack_Id, lcSpckHdr )
           =SEEK("P"+"*****"+INVLINE.Pack_Id+INVLINE.cPkColor+INVLINE.cPckSize+;
                             INVLINE.cPkVersion,lcSpckHdr)
           *C200329,1 [End]
         ENDIF
         *B604100,1 [END]
       *B803921,1 [END]

         llPack = .T.
         *C200329,1 HBG 07/05/2002 USe the full key expresion [Begin]
         *lcPack_id = &lcOrdLine..Pack_id
         lcPack_id  = &lcOrdLine..Pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion
         IF SEEK('S'+LEFT(&lcOrdLine..cPckSize,1),'SCALE')
           lcSz = RIGHT(&lcOrdLine..cPckSize,1)
           lcpkSzDsc = SCALE.Sz&lcSz
         ELSE
           lcpkSzDsc = '*****'    
         ENDIF  
         lcPack_Dsc = ALLTRIM(&lcOrdLine..Pack_id)+'-'+ALLTRIM(&lcOrdLine..cPkColor)+'-'+ALLTRIM(lcpkSzDsc)+'-'+ALLTRIM(&lcOrdLine..cPkVersion)
         *C200329,1 [End]
         STORE 0 TO lnRequire,lnShipped,lnPrice,lnTotPack,lnAmt,lnTot
         *HBG Add variable to get shipped Qty and ordered Qty from nPackNo field in case of reguler packs [Begin]
         lnShpAmt = 0
         lnPackNo = 0
         *HBG [End]
         *B803705,1 BWA 10/01/2000 Fix the bug of some lines in the invoice doesn't print if the invoice has Packs and regular styles in the same time.[START]
         lcRecNo = RECNO()
         *B803705,1 [END]

         *B803921,1 BWA 12/20/2000 2) Modify the check of the packid from the spkline file to be in the invline file.[START]
         *SCAN WHILE Invoice = xInvoice AND SEEK("O"+INVHDR.Order+STR(Lineno,6),lcOrdLine);
                    AND &lcOrdLine..Pack_id = lcPack_id AND;
            (SEEK("P"+Account+&lcOrdLine..Pack_Id+Style,"SPCK_LIN") OR;
             SEEK("P"+"*****"+&lcOrdLine..Pack_Id+Style,"SPCK_LIN"))
         *C200329,1 HBG 07/05/2002 Change SCAN expresion [Begin]      
         *SCAN WHILE Invoice = xInvoice AND SEEK("O"+INVHDR.Order+STR(Lineno,6),lcOrdLine);
                    AND &lcOrdLine..Pack_id = lcPack_id AND;
                    INVLINE.PACK_ID = &lcOrdLine..Pack_Id
         
         SCAN WHILE Invoice = xInvoice AND SEEK("O"+INVHDR.Order+STR(Lineno,6),lcOrdLine);
            AND &lcOrdLine..Pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion = lcPack_id AND;
            INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion =;
            &lcOrdLine..Pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion
         *C200329,1 [End]
         *B803921,1 [END]

           *B803705,1 BWA 10/01/2000 Fix the bug of some lines in the invoice doesn't print if the invoice has Packs and regular styles in the same time.[START]
           lcRecNo = RECNO()
           *B803705,1 [END]

            lnshipped = lnshipped +Totqty
            lnRequire = lnRequire + Totqty + lfGetBack()
            *HBG Add variable to get shipped Qty and ordered Qty from nPackNo field in case of reguler packs [Begin]
            lnPackNo  = INVLINE.nPackNo
            *HBG [End]
            *B803921,1 BWA 12/20/2000 2) Modify the check of the packid from the spkline file to be in the invline file.[START]
            *lnTotPack = lnTotPack + SPCK_LIN.Totqty
            lnTotPack = lnTotPack + INVLINE.Totqty
            *B803921,1 [END]
            
            *C200329,1 HBG 07/05/2002 Change AMOUNT Calculation [Begin]       
            *lnPrice = IIF (Price > lnPrice,Price,lnPrice)  
            *lnAmt   = lnAmt + (Price * TotQty)
            *hbg
            IF llCalcPric 
              lnShpAmt = lnShpAmt + (Price * TotQty)
            ELSE
              lnPrice  = IIF(nPkSlPrice = 0,Price,nPkSlPrice)
            ENDIF  
            *hbg
            =SEEK(INVLINE.Invoice+INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion,lcInvPkTmp) 
            lnInvPkQty  = &lcInvPkTmp..PackQty 
            *hbg
            *IF nPkSlPrice = 0
            *  lnPrice = IIF (Price > lnPrice,Price,lnPrice)  
            *  llCalcPric = .T.
            *ELSE
            *  IF !llCalcPric
            *    lnPrice  = IIF(nPkSlPrice = 0,Price,nPkSlPrice)
            *  ELSE
            *    lnPrice  = lnPrice + ROUND(ROUND(TOTQTY/lnInvPkQty ,2) * Price,2)
            *  ENDIF  
            *ENDIF  
            *hbg
            *C200329,1[End]
            lnTot = lnTot + Totqty
         ENDSCAN

         *B605110,1 Scan all invline for same pack ID. [Begin]
         SCAN REST WHILE Invoice = xInvoice
           *C200329,1 HBG 07/05/2002 Use Full key expression [Begin]        
           *IF Pack_ID = lcPack_id
           IF Pack_id+cPkColor+cPckSize+cPkVersion = lcPack_id
           *C200329,1 [End]      
             lnshipped = lnshipped +Totqty
             lnRequire = lnRequire + Totqty+ lfGetBack()
             lnTotPack = lnTotPack + INVLINE.Totqty
             *HBG Add variable to get shipped Qty and ordered Qty from nPackNo field in case of reguler packs [Begin]
             lnPackNo  = INVLINE.nPackNo
             *HBG [End]
             *C200329,1 HBG 07/05/2002 Change Amount calculation [Begin]         
             *lnPrice = IIF (Price > lnPrice,Price,lnPrice)  
             *lnAmt = lnAmt + (Price * TotQty)
             *hbg
             IF llCalcPric 
               lnShpAmt = lnShpAmt + (Price * TotQty)
             ELSE
               lnPrice  = IIF(nPkSlPrice = 0,Price,nPkSlPrice)
             ENDIF  
             *hbg
             =SEEK(INVLINE.Invoice+INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion,lcInvPkTmp) 
             lnInvPkQty  = &lcInvPkTmp..PackQty 
             *hbg
             *IF nPkSlPrice = 0
             *  lnPrice = IIF (Price > lnPrice,Price,lnPrice)  
             *  llCalcPric = .T.
             *ELSE
             *  IF !llCalcPric
             *    lnPrice  = IIF(nPkSlPrice = 0,Price,nPkSlPrice)
             *  ELSE
             *    lnPrice  = lnPrice + ROUND(ROUND(TOTQTY/lnInvPkQty ,2) * Price,2)
             *  ENDIF  
             *ENDIF  
             *hbg
             *C200329,1 [End]
             lnTot = lnTot + Totqty
           ENDIF
         ENDSCAN
         *B605110,1 Scan all invline for same pack ID. [End]
         
         *B803705,1 BWA 10/01/2000 Fix the bug of some lines in the invoice doesn't print if the invoice has Packs and regular styles in the same time.[START]
         GOTO lcRecNo IN INVLINE
         *B803705,1 [END]
         *C200329,1 HBG 07/05/2002 Change Shipped an requied calculation [Begin]         
         IF !llCalcPric
           *HBG Get shipped Qty and ordered Qty from nPackNo field in case of reguler packs [Begin]
           *lnshipped = lnshipped / lnPckQty
           *lnRequire = lnRequire / lnPckQty
           lnshipped = lnPackNo
           lnRequire = lnPackNo
           *HBG [End]
         ENDIF  
         *C200329,1 [End]
         @ ROW,0 SAY lnRequire PICTURE '99999.99'
         @ ROW,10 SAY lnShipped PICTURE '99999.99'
          
       CASE lcDzn_Pice='D'
         @ ROW,02 SAY lnRequire/12 PICTURE '999.99'
         @ ROW,12 SAY lnShipped/12 PICTURE '999.99'
          
       OTHERWISE
         @ ROW,02 SAY lnRequire PICTURE '999999'
         @ ROW,12 SAY lnShipped PICTURE '999999'
     ENDCASE
     SET ORDER TO TAG &lcOrder IN SPCK_LIN
     IF !llPack 
       lnTot = TotQty
     ENDIF

     *B803420,1 BWA 07/19/2000 Move the color discreption 6 chrs to left by printing the first 12 chr in style.[START]
     *@ ROW,23 SAY IIF(!llPack,Style + SPACE(2) + LEFT(CODES.cDiscrep,15),lcPack_Id)
     *C200329,1 HBG 07/05/2002 Print the full Pack ID [Begin]         
     *@ ROW,23 SAY IIF(!llPack,LEFT(Style,12) + SPACE(2) + LEFT(CODES.cDiscrep,15),lcPack_Id)
     @ ROW,23 SAY IIF(!llPack,LEFT(Style,12)  + SPACE(2) + LEFT(CODES.cDiscrep,15),lcPack_Dsc)
     *C200329,1 [End]   
     *B803420,1 [END]

     IF llPack
       *C200329,1 HBG 07/05/2002 Print the Alltrim of full Pack ID and calculate the 
       *C200329,1                amount, And Change the position of Pack Desacription[Begin]   
       *@ ROW,39 SAY &lcSpckHdr..Desc
       *@ ROW,51 SAY SPACE(2) + SUBSTR(&lcSpckHdr..Desc,1,15)
       IF !llCalcPric
         lnAmt  = (lnInvPkQty /lnPckQty) * lnPrice  
       ELSE
         *hbg
         *lnAmt  = lnInvPkQty * lnPrice 
         lnAmt   = lnShpAmt
         lnPrice = lnAmt / lnInvPkQty
         *hbg
       ENDIF
       *C200329,1 [End]
       @ ROW,59 SAY lnPrice PICTURE '9999.99'
       @ ROW,70 SAY lnAmt PICTURE '999999.99'
       ROW = ROW + 1
       @ ROW,2 SAY "Pieces per pack : " 
       @ ROW,20 SAY lnTotPack PICTURE '9999999'
       *C200329,1 HBG 07/05/2002 Change the position of Pack Desacription[Begin]   
       @ ROW,30 SAY SPACE(2) + SUBSTR(&lcSpckHdr..Desc,1,15)
       *C200329,1 [End]
     ELSE
       =lpPrtBreak()
     ENDIF
     IF !llPack
       IF EMPTY(&lcOrdLine..DESC1)
         IF SEEK(Style,"STYLE")
           @ ROW,23 SAY SUBSTR(STYLE.DESC1,1,57)
         ENDIF
       ELSE
         @ ROW,23 SAY SUBSTR(&lcOrdLine..DESC1,1,57)
       ENDIF
       *C200329,1 HBG 07/05/2002 Move this block of code to get the SKU in both cases to style and pack [Begin]
       *ROW = ROW + 1
       *@ ROW,23 SAY "SKU# : " + lfGetSku()
       *C200329,1 [End]
     ENDIF
     *C200329,1 HBG 07/05/2002 Move this block of code to get the SKU in both cases to style and pack [Begin]
     ROW = ROW + 1
     @ ROW,23 SAY "SKU# : " + lfGetSku()
     *C200329,1 [End]
     XPIECES    = XPIECES + lnTot
     XSUBTOTAL  = XSUBTOTAL+IIF(llPack,lnAmt,(Price*TotQty))
     ROW = ROW + 1
     
     SELECT INVLINE

     *B803420,1 BWA 07/19/2000 Get the variable its initials value.[START]
     llPack = .F.
     *B803420,1 [END]

     IF !llPack
       IF !EOF()
         SKIP
       ENDIF
     ENDIF
  ENDDO
   *** END LINE PROCESSING
   *---------------------------------------------------------------
   * CURRENTLY PROCESSING REGULAR LINE ITEMS
   *---------------------------------------------------------------
   ENDPAGE = IIF(INVLINE->INVOICE = XINVOICE ,'1','0')

   SELECT INVHDR
   IF ENDPAGE = '1' .AND. !XNOTE_LOOP
      @ 58,20 SAY 'C O N T I N U E D ...'
      @ 59,02 SAY 'Page '+ ALLTRIM(STR(lnPageNo))
      LOOP
   ENDIF

   *** Print invoice Notepad,and check if the user wanted to print the notepad or not.
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
         SELECT INVHDR
         @ 58,20 SAY 'C O N T I N U E D ...'
         @ 59,02 SAY 'Page '+ ALLTRIM(STR(lnPageNo))
         LOOP
       ENDIF
     ENDIF

     SET MEMOWIDTH TO lnOldMemW
  ENDIF

   SELECT INVHDR
   ROW = ROW + 1
   @ ROW,12 SAY 'Total - M E R C H A N D I S E'

   *C102347,1 BWA 05/13/2001 Repeat printing the total pieces to print it in the last copy.[START]
   @ ROW,46 SAY INVHDR.Ship PICTURE '999999'
   @ ROW,52 SAY 'PCS'
   *C102347,1 [END]

   @ ROW,51 SAY INVHDR.Ship PICTURE '999999'
   @ ROW,69 SAY INVHDR.ShipAmt PICTURE '9999999.99'
   ROW=ROW+1

   IF DISCOUNT<>0
      @ ROW,12 SAY 'Total - D I S C O U N T'
      @ ROW,67 SAY DISCOUNT         PICTURE '99999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'M' 
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY SUBSTR(lcTaxRefr,1,25)
     ENDIF  
     @ ROW,61 SAY XSTRING_RATE + '%'
     @ ROW,68 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,61 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,68 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  

     *C200184,1 BWA 04/22/2001 Add the HST tax.[START]
     IF InvHdr.nHstamt <> 0
       @ Row,12 SAY 'H S T   T A X'
       @ Row,61 SAY STR(InvHdr.nHstrate,5,2)+'%'
       @ Row,68 SAY InvHdr.nHstamt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF
     *C200184,1 [END]

   ENDIF  
   
   WKAMT = FREIGHT + INSUR + COD
   IF WKAMT <> 0
      @ ROW,12 SAY 'Total - F R E I G H T'
      @ ROW,68 SAY WKAMT            PICTURE '9999999.99'
      ROW=ROW+1
   ENDIF

   *** Print the tax rate and tax amount 
   IF XTAX .AND. XTAX_METH = 'A'
     XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
     @ ROW,12 SAY XTAX_DESC
     IF !EMPTY(lcTaxRefr)
       @ Row,33 SAY SUBSTR(lcTaxRefr,1,25)
     ENDIF  
     @ ROW,61 SAY XSTRING_RATE + '%'
     @ ROW,68 SAY INVHDR->TAX_AMT  PICTURE '9999999.99'
     ROW = ROW + 1

     IF InvHdr.nPSTAmt <> 0
       @ Row,12 SAY 'P S T   T A X'
       @ Row,61 SAY STR(InvHdr.nPSTRate,5,2)+'%'
       @ Row,68 SAY InvHdr.nPSTAmt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  

     *C200184,1 BWA 04/22/2001 Add the HST tax.[START]
     IF InvHdr.nHstamt <> 0
       @ Row,12 SAY 'H S T   T A X'
       @ Row,61 SAY STR(InvHdr.nHstrate,5,2)+'%'
       @ Row,68 SAY InvHdr.nHstamt PICTURE '9999999.99'
       Row = Row + 1
     ENDIF  
     *C200184,1 [END]

   ENDIF  

   @ 59,02 SAY 'Page '+ ALLTRIM(STR(lnPageNo))
   @ 59,62 SAY 'Total '
   @ 59,69 SAY INVHDR.TOTALCHG      PICTURE '9999999.99'

   *B803420,1 BWA 07/19/2000 To get the new data when changing the invoice.[START]
   SELECT INVHDR
   IF EOF()
     NEWDOC = .F.
     SET DEVICE TO SCREEN
     RETURN
   ELSE
      NEWDOC = .T.
   ENDIF
   *B803420,1 [END]
   
   *B803705,1 BWA Fix the bug of when print the invoices the prtflag doesn't change to Yes.[START]
   IF gcDevice <> 'SCREEN'
     INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (INVHDR.INVOICE)
   ENDIF
   *B803705,1 [END]

ENDSCAN

*B803692,1 To clear the window after finishing the invoice.[START]
WAIT CLEAR
*B803692,1 [END]

*B803420,1 BWA 07/19/2000 The screen of the invoice broken after printing.[START]
SET DEVICE TO SCREEN
*B803420,1 [END]

*B605110,1 Clear the temp packs file. [Begin]
IF USED(lcPackTmp)
  USE IN (lcPackTmp)
  ERASE (gcWorkDir+lcPackTmp+".DBF")
  ERASE (gcWorkDir+lcPackTmp+".CDX")  
ENDIF
*C200329,1 HBG 07/05/2002 Close the Temp file of total invoiced Qty from each pack[Begin] 
IF USED(lcInvPkTmp)
  USE IN (lcInvPkTmp)
  ERASE (gcWorkDir+lcInvPkTmp+".DBF")
  ERASE (gcWorkDir+lcInvPkTmp+".CDX")  
ENDIF
*C200329,1 [End]
*B605110,1 Clear the temp packs file. [End]

RETURN

*!*************************************************************
*! Name        : lpPrtBreak
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 03/13/2000
*! Purpose     : Print the breakdoun of the pieases
*!*************************************************************
*! Called from : ARPINVGM
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lpPrtBreak()
*!*************************************************************
FUNCTION lpPrtBreak

SELECT SCALE

*B803921,1 BWA 12/20/2000 1) Fix the bug of style discreption print in the same line f the pieaces in case there is no scale in the invline file.[START]
*IF SEEK('S'+INVLINE.SCALE)
lcScale = INVLINE.SCALE
IF EMPTY(INVLINE.SCALE)
  lcScale = STYLE.SCALE
  IF EMPTY(STYLE.SCALE)
    llScale = .T.
  ENDIF
ENDIF

IF !EMPTY(lcScale) OR llScale
  IF !EMPTY(lcScale)
    =SEEK('S'+lcScale)
  ENDIF
*B803921,1 [END]

  IF lcDzn_Pice='D'
    @ ROW,59  SAY INVLINE.Price*12 PICTURE '9999.99'
  ELSE
    @ ROW,59  SAY INVLINE.Price  PICTURE '9999.99'
  ENDIF

  IF lcDzn_Pice = 'D'
    lnNewPrce = INVLINE.Price*12
    lnNewQty  = INVLINE.TotQty/12
    @ ROW,70  SAY (lnNewPrce * lnNewQty)  PICTURE '999999.99'
  ELSE
    @ ROW,70  SAY (INVLINE.Price*INVLINE.TotQty)  PICTURE '999999.99'
  ENDIF  

  ROW = ROW + 1
  IF lcDzn_Pice = 'D'
    @ ROW,12 SAY lnBackOrd/12  PICTURE '@Z 999.99'
  ELSE
    @ ROW,12 SAY lnBackOrd     PICTURE '@Z 999999'
  ENDIF

  IF CNT<>1
     @ ROW,47 SAY IIF(lcDzn_Pice='D','DZ','PC')
     Row = Row + 2
  ENDIF
ENDIF
SELECT INVLINE
RETURN

*!*************************************************************
*! Name        : lfGetSku
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 03/13/2000
*! Purpose     : Get the Sku.
*!*************************************************************
*! Called from : ARPINVGM
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Pack_id
*!*************************************************************
*! Example     : =lfGetSku()
*!*************************************************************
FUNCTION lfGetSku

*C200329,1 HBG 07/05/2002 Use Full Key expression[Begin]    
*IF !EMPTY(&lcOrdLine..Pack_id)
  *RETURN Pack_Id
SET ORDER TO Spck_linst IN SPCK_LIN 
IF llPack 
  IF SEEK("S"+INVLINE.Account+PADR(INVLINE.Pack_Id,19)+INVLINE.cPkColor+INVLINE.cPckSize,"SPCK_LIN")
    RETURN SPCK_LIN.Pack_Id
  ELSE
    RETURN " "
  ENDIF         
*C200329,1 [End]
ELSE
  IF SEEK("S"+INVLINE.Account+INVLINE.Style,"SPCK_LIN")
    RETURN SPCK_LIN.Pack_id
  ELSE
    RETURN " "
  ENDIF
ENDIF

*!*************************************************************
*! Name        : lfGetBack
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 03/13/2000
*! Purpose     : Get Backorder
*!*************************************************************
*! Called from : ARPINVGM
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lnBackOrd
*!*************************************************************
*! Example     : =lfGetBack()
*!*************************************************************

FUNCTION lfGetBack
PRIVATE lcAlias , lcOrder

lcAlias    = ALIAS()
SELECT (lcOrdLine)
lcOrder    = ORDER()
SET ORDER TO TAG Ordline

IF INVHDR.Consol = 'Y'
  SELECT CONSINVL
  SEEK xInvoice
  SCAN WHILE Invoice=xInvoice FOR Style = INVLINE.Style
    =SEEK('O' + INVHDR.Order + STR(InvLine.Lineno,6) , lcOrdLine )
    lnBackOrd = lnBackOrd + &lcOrdLine..TotQty
  ENDSCAN
  SELECT INVLINE
ELSE
  =SEEK('O' + INVHDR.Order + STR(InvLine.Lineno,6) , lcOrdLine )
  lnBackOrd = &lcOrdLine..TotQty
ENDIF
*B803692,1 Select lcOrdLine to restore its order.[START]
SELECT (lcOrdLine)
*B803692,1 [END]
SET ORDER TO &lcOrder

SELECT (lcAlias)

RETURN lnBackOrd

*!*************************************************************
*! Name      : lfGetcont
*! Developer : BASSEM RAFAAT 
*! Date      : 03/13/2000
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


@ 58,20 SAY 'C O N T I N U E D ...'
@ 59,02 SAY 'Page '+ ALLTRIM(STR(lnPageNo))

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : BASSEM RAFAAT 
*! Date      : 03/13/2000
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

lnPageNo = lnPageNo + 1

* LINE 1
SELECT INVHDR

*B604625,1 BWA 07/26/2001 Print the company address and comment the printing of the division in the address.[START]
*IF !EMPTY(Customer.cDivision) .AND. SEEK('D'+Customer.cDivision,'CODES')
IF .F.
*B604625,1 BWA 07/26/2001[END]

  IF !EMPTY(cDivision)
    @ 03,05 SAY LEFT(cDivision,22)
    @ 03,29 SAY IIF(!EMPTY(INVHDR.cFaccode),LEFT(lcNote1,28),"")
    @ 03,65 SAY XINVOICE
    @ 04,29 SAY IIF(!EMPTY(INVHDR.cFaccode),lcNote2,"")
    @ 05,29 SAY IIF(!EMPTY(INVHDR.cFaccode),lcNote3,"")
    @ 05,65 SAY Account
    @ 06,29 SAY IIF(!EMPTY(INVHDR.cFaccode),lcNote4,"")
  ELSE
    @ 03,05 SAY IIF(XINVNAME='Y',LEFT(HLINE1,22),'')
    @ 03,29 SAY IIF(!EMPTY(INVHDR.cFaccode),LEFT(lcNote1,28),"")
    @ 03,65 SAY XINVOICE
    @ 04,05 SAY IIF(XINVNAME='Y',LEFT(HLINE2,22),'')
    @ 04,29 SAY IIF(!EMPTY(INVHDR.cFaccode),lcNote2,"")
    @ 05,05 SAY IIF(XINVNAME='Y',LEFT(HLINE3,23),'')
    @ 05,29 SAY IIF(!EMPTY(INVHDR.cFaccode),LEFT(lcNote3,28),"")
    @ 05,65 SAY Account
    @ 06,05 SAY IIF(XINVNAME='Y',LEFT(HLINE4,22),'')
    @ 06,29 SAY IIF(!EMPTY(INVHDR.cFaccode),lcNote4,"")
    IF XINVNAME='Y' .AND. !EMPTY(HLINE5)
      @ 07,05 SAY 'Telephone '
      @ 07,15 SAY HLINE5
    ENDIF
  ENDIF
ELSE
  @ 03,05 SAY IIF(XINVNAME='Y',LEFT(HLINE1,22),'')
  @ 03,29 SAY IIF(!EMPTY(INVHDR.cFaccode),LEFT(lcNote1,28),"")
  @ 03,65 SAY XINVOICE
  @ 04,05 SAY IIF(XINVNAME='Y',LEFT(HLINE2,22),'')
  @ 04,29 SAY IIF(!EMPTY(INVHDR.cFaccode),LEFT(lcNote2,28),"")
  @ 05,05 SAY IIF(XINVNAME='Y',LEFT(HLINE3,22),'')
  @ 05,29 SAY IIF(!EMPTY(INVHDR.cFaccode),LEFT(lcNote3,28),"")
  @ 05,65 SAY Account
  @ 06,05 SAY IIF(XINVNAME='Y',LEFT(HLINE4,22),'')
  @ 06,29 SAY IIF(!EMPTY(INVHDR.cFaccode),LEFT(lcNote4,28),"")
  IF XINVNAME='Y' .AND. !EMPTY(HLINE5)
    @ 07,05 SAY 'Telephone '
    @ 07,15 SAY HLINE5
  ENDIF
ENDIF

@ 10,03 SAY "DUN# 557461522"
@ 13,03 SAY SUBSTR(XBTNAME,1,40)
@ 13,47 SAY IIF(CURLOOP = '1',XSTNAME,'')
@ 14,03 SAY SUBSTR(XBTADDR1,1,40)
@ 14,47 SAY IIF(CURLOOP = '1',XSTADDR1,'')
@ 15,03 SAY SUBSTR(XBTADDR2,1,40)
@ 15,47 SAY IIF(CURLOOP = '1',XSTADDR2,'')
@ 16,03 SAY SUBSTR(XBTADDR3,1,40)
@ 16,47 SAY IIF(CURLOOP = '1',XSTADDR3,'')
@ 20,00 SAY Invdate
@ 20,13 SAY PShipVia
@ 20,35 SAY Weight
@ 20,50 SAY Cartons
@ 20,64 SAY LEFT(PTerms,15)

*B604546,1 BWA 06/13/2001 Increase the width of the Custpo field.[START]
*@ 23,01 SAY DEPT+'-'
*@ 23,09 SAY LEFT(CustPo,10)+'- '
*@ 23,21 SAY STORE
*@ 23,32 SAY ordhdr.Entered

@ 23,00 SAY DEPT+'-'
@ 23,06 SAY CustPo+'-'
@ 23,22 SAY STORE
@ 23,31 SAY ordhdr.Entered
*B604546,1 BWA 06/13/2001 [END]

@ 23,50 SAY rep1
@ 23,65 SAY Order

ROW = 27
*-- End of lfGetInHdr.
*!*************************************************************
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 03/19/2000
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Called from : ARPINVF.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,lcSyszone),'',&lcSyszone..ZONE)
