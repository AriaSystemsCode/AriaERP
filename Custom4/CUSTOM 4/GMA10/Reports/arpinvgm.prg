*:***************************************************************************
*: Program file        : ARPINVGM.PRG
*: Program description : ACCOUNT RECEIVABLE INVOICE FOR GMA ACCOSORIES
*: Module              : ACCOUNT RECEIVABLE (AR)
*: Developer           : Heba Fathi (HFK)      
*: Tracking Job Number : C038272
*: Date                : 07/06/2004
*:***************************************************************************
*: Calls :             
*:             Programs: 
*:              Screens: 
*:      Global Function:
*:***************************************************************************
*: Called From: ARPINV.PRG
*:**********************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVGM
*:**************************************************************************
*! Modifications:
*! #B038981,2 HFK, 01/30/2005 change the way of opening a file.
*! E128427,1 MMT 06/16/2005,if invoice consalidated by DC print dist_ctr instead of Store[Start]
*: E128356,1 HFK Add L/C No, L/C Issuing Bank and  L/C Issuing Date to the PHK layout
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table [T20080611.0041]
*:**************************************************************************
#INCLUDE R:\a4xpdemo\Aria4xp\reports\arpinvgm.h
IF !USED(lcSpckHdr)
  =gfOpenFile(oAriaApplication.DataDir+'SPCK_HDR',oAriaApplication.DataDir+'SPCK_HDRVR','SH' , @lcSpckHdr , .T.)
ENDIF
IF !USED(lcSysZone)
  = gfOpenFile(oAriaApplication.SysPath +'Syszones' ,oAriaApplication.SysPath +'Frtzones', 'SH', @lcSysZone, .T.)
ENDIF

IF !USED(lcOrdLine)
  = gfOpenFile(oAriaApplication.DataDir+'Ordline' ,oAriaApplication.DataDir+'Ordline', 'SH', @lcOrdLine, .T.)
ENDIF

*-hfk, #038272, 07/11/2004,create arrayd to hold Temp. files structure
*--B128021,1 MMT 05/17/2005,fix bug of not updating the printed [Start]
*IF loOGScroll.llOGFltCh
*--B128021,1 MMT 05/17/2005,fix bug of not updating the printed [End]
=lfCreateTemp()
SELECT InvLine
SET RELATION TO 'O'+order+STR(lineno,6) INTO &lcOrdLine ADDITIVE

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
*--Section to get the FromZone
lcFromZone = ALLTRIM(gfGetMemVar('XUPSFROM',oAriaApplication.ActiveCompanyID))
IF gfGetMemVar('M_WareHouse',oAriaApplication.ActiveCompanyID)='Y'
  IF !USED(lcWarHous)
    = gfOpenFile(oAriaApplication.DataDir+'WAREHOUS' ,oAriaApplication.DataDir+'WAREHOUS', 'SH', @lcWarHous, .T.)
  ENDIF
  lcFromZone = IIF(SEEK(InvHdr.cWareCode, lcWarHous ),&lcWarHous..UPS,lcFromZone)
ENDIF

*--End Section to get the FromZone --*

           *-- Section of initial the program variables --*

xTax      = gfGetMemVar("M_TAX",oAriaApplication.ActiveCompanyID)='Y'
XTAX_DESC = gfGetMemVar('M_TAX_DESC',oAriaApplication.ActiveCompanyID)
XTAX_RATE = gfGetMemVar('M_TAX_RATE',oAriaApplication.ActiveCompanyID)
XTAX_METH = gfGetMemVar('M_TAX_METH',oAriaApplication.ActiveCompanyID)
lcTaxRefr = gfGetMemVar('M_TAX_REFE',oAriaApplication.ActiveCompanyID)
XINVNAME  = lcPrnComp
PRINTFCTR = llPrnFact
STORE .F. TO llNoRec
lcScale   = SPACE(0)
STORE .T. TO llScale

HLINE1    = IIF(EMPTY(lcDivLName) , lcCompName , lcDivLName)
STORE TRIM(laCompAdd[1])                                   TO HLINE2
STORE TRIM(laCompAdd[2])                                   TO HLINE3
STORE TRIM(laCompAdd[3])+' '+laCompAdd[4]+' '+laCompAdd[5] TO HLINE4
STORE ALLTRIM(lcCompPhon)                                           TO HLINE5

IF EMPTY(HLINE3)
   STORE HLINE4 TO HLINE3
   STORE HLINE5 TO HLINE4
   STORE ''     TO HLINE5
ENDIF

lcUpsType = ''
DECLARE laZone[1,2]
laZone[1,1] = 'CUPS'
laZone[1,2] = 'lcUpsType'

NEWDOC     = .T.
XNOTE_LOOP = .F.
lnNotLine  = 1

*- #B038981,3 HFK, 02/01/2005 [Start]
lcRpPrSt = IIF(oAriaApplication.ProcessID = 'ARPINV',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")
*- #B038981,3 HFK, 02/01/2005 [End]
SELECT INVHDR
LOCATE FOR &lcRpExp
IF !FOUND()
  *-WAIT WINDOW "No Records Selected"
  WAIT WINDOW LANG_ARPINVGM_NoRec
  llNoRec = .T.
  return .f.
ENDIF
*- #B038981,3 HFK, 02/01/2005 [Start]
lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
*- #B038981,3 HFK, 02/01/2005 [End]

lcASExp = IIF(EMPTY(lcRpExp) , .T. , lcRpExp)
lcPackTmp = loOGScroll.gfTempName()
lcInvPkTmp = loOGScroll.gfTempName()
gfCrtTmp(lcPackTmp,"(Invoice C(6) , Pack_ID C(16) , cpkcolor C(6) , cpcksize C(3) , cPkSzDsc C(5) , cpkversion C(4))","Invoice + Pack_ID + cPkColor + cPckSize + cPkVersion",lcPackTmp,.F.)
gfCrtTmp(lcInvPkTmp,"(Invoice C(6) , Pack_ID C(16) , cpkcolor C(6) , cpcksize C(3) , cpkversion C(4) , PackQty N(6))","Invoice + Pack_ID + cPkColor + cPckSize + cPkVersion",lcInvPkTmp,.F.)

SELECT InvHdr
*- #B038981,3 HFK, 02/01/2005 [Start]
lcRpPrSt = IIF(oAriaApplication.ProcessID = 'ARPINV',IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt),"")
*- #B038981,3 HFK, 02/01/2005 [End]
SCAN FOR &lcASExp
  SELECT InvLine
  =SEEK(InvHdr.Invoice)
  SCAN REST WHILE Invoice+STR(lineno,6) = InvHdr.Invoice
    IF SEEK(Invoice+Pack_id+cPkColor+cPckSize+cPkVersion,lcInvPkTmp)
      REPLACE &lcInvPkTmp..PackQty WITH &lcInvPkTmp..PackQty + InvLine.TotQty
    ELSE
      INSERT INTO (lcInvPkTmp) (Invoice,Pack_ID,cPkColor,cPckSize,cPkVersion,PackQty);
                        VALUES (InvLine.Invoice,InvLine.Pack_ID,InvLine.cPkColor,;
                                InvLine.cPckSize,InvLine.cPkVersion,InvLine.TotQty)
    ENDIF
  ENDSCAN
ENDSCAN
SELECT InvHdr

SCAN FOR &lcASExp
  *-WAIT WINDOW 'Selecting Records For The Report ...' + Invoice NOWAIT
  WAIT WINDOW LANG_ARPINVGM_Select + Invoice NOWAIT
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
  STORE 0 TO lnRequire,lnShipped,lnBackOrd
  lnPackNo  = 0
  DO WHILE CURLOOP = '1' .AND. !XNOTE_LOOP
    SELECT INVLINE
    IF EOF() .OR. INVOICE <> XINVOICE 
      EXIT
    ENDIF

    IF TOTQTY = 0
      SKIP
      LOOP
    ENDIF
    lnBackOrd = 0

    =SEEK('N'+SUBSTR(STYLE,lnClrPos,lnClrLen)+'N'+'COLOR','CODES')

    lnRequire=TOTQTY + lfGetBack()
    lnShipped=TOTQTY
    lcOrder = ORDER("SPCK_LIN")
    SET ORDER TO TAG SPCK_LINVR IN SPCK_LIN

    llPack = .F.

    SET ORDER TO SPCK_HDRVR IN SPCK_HDR
    DO CASE
      =SEEK("P"+SPCK_LIN.Account+SPCK_LIN.Pack_Id+SPCK_LIN.cPkColor+SPCK_LIN.cPckSize+SPCK_LIN.cPkVersion, lcSpckHdr )
      CASE !EMPTY(&lcOrdLine..Pack_id) AND INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion =;
        &lcOrdLine..Pack_Id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion
        SET ORDER TO SPCK_HDRVR IN SPCK_HDR
        llGetPack = SEEK('P'+INVLINE.Account+INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion,'SPCK_HDR')
        IF !llGetPack
          llGetPack = SEEK('P*****'+INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion,'SPCK_HDR')
        ENDIF
        llCalcPric = .F.
        IF llGetPack
          lnPckQty = SPCK_HDR.nPckQty
          IF (INVLINE.lRange OR INVLINE.lPckPrPiec OR INVLINE.nPkSlPrice = 0)
            llCalcPric = .T.
          ENDIF
        ENDIF
        IF SEEK(Invoice+Pack_id+cPkColor+cPckSize+cPkVersion,lcPackTmp)
          IF !EOF()
            SKIP
            LOOP
          ENDIF
        ELSE
          IF SEEK('S'+LEFT(InvLine.cPckSize,1),'SCALE')
            lcSz = RIGHT(InvLine.cPckSize,1)
            lcpkSzDsc = SCALE.Sz&lcSz
          ELSE
            lcpkSzDsc = '*****'
          ENDIF
          INSERT INTO (lcPackTmp) (Invoice,Pack_ID,cPkColor,cPckSize,cPkVersion,cpkSzDsc);
                            VALUES (InvLine.Invoice,InvLine.Pack_ID,InvLine.cPkColor,;
                                     InvLine.cPckSize,InvLine.cPkVersion,lcpkSzDsc)

        ENDIF
        =SEEK("P"+INVLINE.Account+INVLINE.Pack_Id+INVLINE.cPkColor+;
                  INVLINE.cPckSize+INVLINE.cPkVersion, lcSpckHdr )
        IF EOF(lcSpckHdr)
          =SEEK("P"+"*****"+INVLINE.Pack_Id+INVLINE.cPkColor+INVLINE.cPckSize+;
                           INVLINE.cPkVersion,lcSpckHdr)
        ENDIF
        llPack = .T.
        lcPack_id  = &lcOrdLine..Pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion
        IF SEEK('S'+LEFT(&lcOrdLine..cPckSize,1),'SCALE')
          lcSz = RIGHT(&lcOrdLine..cPckSize,1)
          lcpkSzDsc = SCALE.Sz&lcSz
        ELSE
          lcpkSzDsc = '*****'
        ENDIF
        lcPack_Dsc = ALLTRIM(&lcOrdLine..Pack_id)+'-'+ALLTRIM(&lcOrdLine..cPkColor)+'-'+ALLTRIM(lcpkSzDsc)+'-'+ALLTRIM(&lcOrdLine..cPkVersion)
        STORE 0 TO lnRequire,lnShipped,lnPrice,lnTotPack,lnAmt,lnTot
        lnShpAmt = 0
        lnPackNo = 0
        lcRecNo = RECNO()
        SCAN WHILE Invoice = xInvoice AND SEEK("O"+INVHDR.Order+STR(Lineno,6),lcOrdLine);
          AND &lcOrdLine..Pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion = lcPack_id AND;
          INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion =;
          &lcOrdLine..Pack_id+&lcOrdLine..cPkColor+&lcOrdLine..cPckSize+&lcOrdLine..cPkVersion
          lcRecNo = RECNO()
          lnshipped = lnshipped +Totqty
          lnRequire = lnRequire + Totqty + lfGetBack()
          lnPackNo  = INVLINE.nPackNo
          lnTotPack = lnTotPack + INVLINE.Totqty
          IF llCalcPric
            lnShpAmt = lnShpAmt + (Price * TotQty)
          ELSE
            lnPrice  = IIF(nPkSlPrice = 0,Price,nPkSlPrice)
          ENDIF
          =SEEK(INVLINE.Invoice+INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion,lcInvPkTmp)
          lnInvPkQty  = &lcInvPkTmp..PackQty
          lnTot = lnTot + Totqty
        ENDSCAN
        SCAN REST WHILE Invoice = xInvoice
          IF Pack_id+cPkColor+cPckSize+cPkVersion = lcPack_id
            lnshipped = lnshipped +Totqty
            lnRequire = lnRequire + Totqty+ lfGetBack()
            lnTotPack = lnTotPack + INVLINE.Totqty
            lnPackNo  = INVLINE.nPackNo
            IF llCalcPric
              lnShpAmt = lnShpAmt + (Price * TotQty)
            ELSE
              lnPrice  = IIF(nPkSlPrice = 0,Price,nPkSlPrice)
            ENDIF
            =SEEK(INVLINE.Invoice+INVLINE.PACK_ID+INVLINE.cPkColor+INVLINE.cPckSize+INVLINE.cPkVersion,lcInvPkTmp)
            lnInvPkQty  = &lcInvPkTmp..PackQty
            lnTot = lnTot + Totqty
          ENDIF
        ENDSCAN
        GOTO lcRecNo IN INVLINE
        IF !llCalcPric
          lnshipped = lnPackNo
          lnRequire = lnPackNo
        ENDIF
        INSERT INTO &lcLines (Invoice,Required ,Shipped) VALUES (XINVOICE,lnRequire,lnShipped)
        REPLACE &lcLines..llPack WITH IIF(llPack,.T.,.F.)
        lcReqMask = 'A'
      CASE lcDzn_Pice='D'
        INSERT INTO &lcLines (Invoice,Required ,Shipped) VALUES (XINVOICE,lnRequire/12,lnShipped/12)
        REPLACE &lcLines..llPack WITH IIF(llPack,.T.,.F.)          
        lcReqMask = 'B'
      OTHERWISE
        INSERT INTO &lcLines (Invoice,Required ,Shipped) VALUES (XINVOICE,lnRequire,lnShipped)
        REPLACE &lcLines..llPack WITH IIF(llPack,.T.,.F.)          
        lcReqMask = 'C'
    ENDCASE
    SET ORDER TO TAG &lcOrder IN SPCK_LIN
    IF !llPack
      lnTot = TotQty
    ENDIF
    cDescription = IIF(!llPack,LEFT(Style,12)  + SPACE(2) + LEFT(CODES.cDiscrep,15),lcPack_Dsc)
    *-SELECT &lcLines    
    REPLACE &lcLines..Dscrption WITH m.cDescription
    IF llPack
      IF !llCalcPric
        IF TYPE('lnPckQty')='U'
          lnPckQty = 0
        ENDIF 
        lnAmt  = IIF(lnPckQty = 0,0,(lnInvPkQty /lnPckQty) * lnPrice)
      ELSE
        lnAmt   = lnShpAmt
        lnPrice = lnAmt / lnInvPkQty

      ENDIF

      REPLACE &lcLines..Price   WITH m.lnPrice,;
              &lcLines..Amount  WITH m.lnAmt,;
              &lcLines..TotAmnt WITH m.lnTotPack
      cDesc2 = SPACE(2) + SUBSTR(&lcSpckHdr..Desc,1,15)
      REPLACE &lcLines..Dscrpt2 WITH m.cDesc2
    ELSE
      =lpPrtBreak()
    ENDIF
    IF !llPack
     IF EMPTY(&lcOrdLine..DESC1)
        IF SEEK(Style,"STYLE")
          cStyleDesc = SUBSTR(STYLE.DESC1,1,57)
        ENDIF
      ELSE
        cStyleDesc = SUBSTR(&lcOrdLine..DESC1,1,57)
      ENDIF
    ELSE 
      cStyleDesc = ""
    ENDIF
   
    REPLACE &lcLines..StyDesc WITH m.cStyleDesc
    REPLACE &lcLines..PackId WITH lfGetSku()
    
    XPIECES    = XPIECES + lnTot
    XSUBTOTAL  = XSUBTOTAL+IIF(llPack,lnAmt,(Price*TotQty))
    SELECT INVLINE
    llPack = .F.
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
  *** Print invoice Notepad,and check if the user wanted to print the notepad or not.
  IF llRpInvNot
    SELECT NOTEPAD
    lnOldMemW = SET("MEMOWIDTH")
    SET MEMOWIDTH TO 75
    IF TYPE + KEY <> 'C' + XINVOICE
      SEEK 'C' + XINVOICE
    ENDIF
    IF TYPE + KEY = 'C' + XINVOICE
      IF(MEMLINES(NOTEPAD.MNOTES)=0)
        REPLACE &lcHeader..HasNotes WITH 'F'
      ELSE 
        REPLACE &lcHeader..HasNotes WITH 'T'
        *--B128483,1 MMT 06/15/2005 fix bug of not printing complete Notes Field[Start]
        REPLACE &lcHeader..MNotes  WITH NOTEPAD.Mnotes
        *--B128483,1 MMT 06/15/2005 fix bug of not printing complete Notes Field[End]
      ENDIF 
    ENDIF
    SET MEMOWIDTH TO lnOldMemW
  ENDIF

  SELECT INVHDR
  REPLACE &lcHeader..Ship     WITH INVHDR.Ship
  REPLACE &lcHeader..ShipAmnt WITH INVHDR.ShipAmt
  IF DISCOUNT<>0
    REPLACE &lcHeader..Discount WITH INVHDR.DISCOUNT
  ENDIF
*-hfk, 05/13/2005
  *** Print the tax rate and tax amount
*!*      IF XTAX .AND. (XTAX_METH = 'M' .OR. XTAX_METH = 'A')
*!*        IF XTAX_METH = 'M'
*!*          WKAMT = FREIGHT + INSUR + COD
*!*          IF WKAMT <> 0
*!*            REPLACE &lcHeader..TotFr8 WITH WKAMT
*!*          ENDIF
*!*        ENDIF 
*!*        *** Print the tax rate and tax amount
*!*        XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
*!*        REPLACE &lcHeader..TaxDesc WITH XTAX_DESC
*!*        IF !EMPTY(lcTaxRefr)
*!*          REPLACE &lcHeader..TaxRefer WITH SUBSTR(lcTaxRefr,1,25)
*!*        ENDIF
*!*        REPLACE &lcHeader..StringRat WITH VAL(XSTRING_RATE)
*!*        REPLACE &lcHeader..TaxAmnt   WITH INVHDR->TAX_AMT
*!*        IF InvHdr.nPSTAmt <> 0
*!*          REPLACE &lcHeader..PstRate WITH STR(InvHdr.nPSTRate,5,2)
*!*          REPLACE &lcHeader..PstAmnt WITH InvHdr.nPSTAmt
*!*        ENDIF
*!*        IF InvHdr.nHstamt <> 0
*!*          REPLACE &lcHeader..HstRate WITH STR(InvHdr.nHstrate,5,2)
*!*          REPLACE &lcHeader..PstAmnt WITH InvHdr.nHstamt
*!*        ENDIF
*!*      ENDIF

*!*      REPLACE &lcHeader..TotCharge WITH INVHDR.TOTALCHG
*!*      SELECT INVHDR
  IF XTAX .AND. (XTAX_METH = 'M' .OR. XTAX_METH = 'A')
    XSTRING_RATE = STR(INVHDR->TAX_RATE,5,2)
    REPLACE &lcHeader..StringRat WITH VAL(XSTRING_RATE)      

    REPLACE &lcHeader..TaxDesc WITH XTAX_DESC
    IF !EMPTY(lcTaxRefr)
      REPLACE &lcHeader..TaxRefer WITH SUBSTR(lcTaxRefr,1,25)
    ENDIF

    REPLACE &lcHeader..TaxAmnt   WITH INVHDR->TAX_AMT
    IF InvHdr.nPSTAmt <> 0
      REPLACE &lcHeader..PstRate WITH VAL(STR(InvHdr.nPSTRate,5,2))
      REPLACE &lcHeader..PstAmnt WITH InvHdr.nPSTAmt
    ENDIF
    
    
    IF InvHdr.nHstamt <> 0
      REPLACE &lcHeader..HstRate WITH VAL(STR(InvHdr.nHstrate,5,2))
      REPLACE &lcHeader..HstAmnt WITH InvHdr.nHstamt
    ENDIF
  ENDIF 
  WKAMT = FREIGHT + INSUR + COD
  IF WKAMT <> 0
    REPLACE &lcHeader..TotFr8 WITH WKAMT
  ENDIF
  REPLACE &lcHeader..TotCharge WITH INVHDR.TOTALCHG
  SELECT INVHDR
*-hfk, 05/13/2005    
  IF EOF()
    NEWDOC = .F.
    RETURN
  ELSE
    NEWDOC = .T.
  ENDIF 
ENDSCAN
*- #B038981,3 HFK, 02/01/2005 [Start]
lcRpPrSt = IIF(lcRpPrSt =SPACE(1),'N',lcRpPrSt)
*- #B038981,3 HFK, 02/01/2005 [End]
DECLARE loOGScroll.laCRTables[2]
loOGScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcHeader  + ".DBF"
loOGScroll.laCRTables[2] = oAriaApplication.WorkDir +  lcLines   + ".DBF"
*- #B127927, HFK, 05/13/2005 [Start]
*!*    DECLARE loOGScroll.laCRParams[5,2]
*!*    loOGScroll.laCRParams[1,1] = 'Require'
*!*    loOGScroll.laCRParams[1,2] = lcReqMask
*!*    loOGScroll.laCRParams[2,1] = 'llPack'
*!*    loOGScroll.laCRParams[2,2] = IIF(llPack,'T','F')
*!*    loOGScroll.laCRParams[3,1] = 'InvNotes'
*!*    loOGScroll.laCRParams[3,2] = IIF(llRpInvNot,'T','F')
*!*    loOGScroll.laCRParams[4,1] = 'lcDozn'
*!*    loOGScroll.laCRParams[4,2] = lcDzn_Pice
*!*    loOGScroll.laCRParams[5,1] = 'llPrint'
*!*    loOGScroll.laCRParams[5,2] = IIF(llPrntComp,'T','F')
DECLARE loOGScroll.laCRParams[4,2]
loOGScroll.laCRParams[1,1] = 'Require'
loOGScroll.laCRParams[1,2] = lcReqMask
loOGScroll.laCRParams[2,1] = 'InvNotes'
loOGScroll.laCRParams[2,2] = IIF(llRpInvNot,'T','F')
loOGScroll.laCRParams[3,1] = 'lcDozn'
loOGScroll.laCRParams[3,2] = lcDzn_Pice
loOGScroll.laCRParams[4,1] = 'llPrint'
loOGScroll.laCRParams[4,2] = IIF(llPrntComp,'T','F')
*- #B127927, HFK, 05/13/2005 [End]  
*--B128021,1 MMT 05/17/2005,fix bug of not updating the printed [Start]
*ENDIF
*--B128021,1 MMT 05/17/2005,fix bug of not updating the printed [End]

lnSelect = SELECT()
*- #B038981,2 HFK, 01/30/2005 [Start]
IF !USED('&lcHeader')
  *- USE &lcPath.&lcHeader IN 0 EXCLUSIVE 
  USE oAriaApplication.WorkDir + lcHeader + ".DBF" IN 0 EXCLUSIVE 
ENDIF 
IF !USED('&lcLines')
  *- USE '"' + &lcPath.&lcLines + '"' IN 0 EXCLUSIVE 
  USE oAriaApplication.WorkDir + lcLines + ".DBF" IN 0 EXCLUSIVE 
ENDIF 
*- #B038981,2 HFK, 01/30/2005 [End]
SELECT &lcLines

INDEX ON Invoice TAG &lcLines
SELECT &lcHeader
*--B128021,1 MMT 05/17/2005,fix bug of not updating the printed [Start]
SCAN 
  INSERT INTO (lcInvPrtUp) (INVOICE) VALUES (&lcHeader..CINVOICE)
ENDSCAN
*--B128021,1 MMT 05/17/2005,fix bug of not updating the printed [End]

*-hfk, 05/10/2005
lcRecCount = LTRIM(STR(RECCOUNT(),7))
WAIT WINDOW 'Selected &lcRecCount RECORDS FOR REPORT...' TIMEOUT 1
*-hfk, 05/10/2005
INDEX ON cInvoice TAG &lcHeader
LOCATE 
IF RECCOUNT()=0
  *-WAIT WINDOW "No Records Selected"
  WAIT WINDOW LANG_ARPINVGM_NoRec
  RETURN .F.
ENDIF

SET RELATION TO &lcHeader..cInvoice INTO &lcLines ADDITIVE 
SELECT (lnSelect)
USE IN &lcHeader
USE IN &lcLines

WAIT CLEAR
IF USED(lcPackTmp)
  USE IN (lcPackTmp)
  ERASE (oAriaApplication.WorkDir+lcPackTmp+".DBF")
  ERASE (oAriaApplication.WorkDir+lcPackTmp+".CDX")
ENDIF
IF USED(lcInvPkTmp)
  USE IN (lcInvPkTmp)
  ERASE (oAriaApplication.WorkDir+lcInvPkTmp+".DBF")
  ERASE (oAriaApplication.WorkDir+lcInvPkTmp+".CDX")
ENDIF
loogScroll.cCROrientation = 'P'
gfdispre()

*!*************************************************************
*! Name        : lpPrtBreak
*! Developer   : Heba Fathi (HFK)
*! Date        : 07/12/2004
*! Purpose     : Print the breakdoun of the pieces
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lpPrtBreak

SELECT SCALE

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
  lnPriceDzn = IIF(lcDzn_Pice='D',INVLINE.Price*12,INVLINE.Price)
  lnNewTotQty = IIF(lcDzn_Pice = 'D',((INVLINE.Price*12)*(INVLINE.TotQty/12)),(INVLINE.Price*INVLINE.TotQty))
  lnTotBack = IIF(lcDzn_Pice = 'D',lnBackOrd/12,lnBackOrd)
  lcReqMask = IIF(lcDzn_Pice = 'D','B','C')
  IF CNT<>1
    lcTag = IIF(lcDzn_Pice='D','DZ','PC')
    REPLACE &lcLines..Dscrpt2 WITH m.lcTag
  ENDIF
  REPLACE &lcLines..Count WITH CNT
  REPLACE &lcLines..Price WITH m.lnPriceDzn,;
         &lcLines..Amount WITH m.lnNewTotQty,;
        &lcLines..TotAmnt WITH lnTotBack
ENDIF
SELECT INVLINE
RETURN

*!*************************************************************
*! Name        : lfGetSku
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 03/13/2000
*! Purpose     : Get the Sku.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Pack_id
*!*************************************************************
FUNCTION lfGetSku

SET ORDER TO Spck_linst IN SPCK_LIN
IF llPack
  IF SEEK("S"+INVLINE.Account+PADR(INVLINE.Pack_Id,19)+INVLINE.cPkColor+INVLINE.cPckSize,"SPCK_LIN")
    RETURN SPCK_LIN.Pack_Id
  ELSE
    RETURN " "
  ENDIF

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
*! Passed Parameters : None
*!*************************************************************
*! Return      : lnBackOrd
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
SELECT (lcOrdLine)
SET ORDER TO &lcOrder
SELECT (lcAlias)
RETURN lnBackOrd

*!*************************************************************
*! Name      : lfGetInHdr
*! Developer : Heba Fathi
*! Date      : 07/14/2004
*! Purpose   : PRINT THE HEADER OF THE INVOICE
*!*************************************************************
*! Passed Parameters :
*!*************************************************************
*! Return      : None
*!*************************************************************
*-hfk, #038272, 07/06/2004
FUNCTION lfGetInHdr
SELECT INVHDR



*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
IF !EMPTY(INVHDR.cFaccode)
  =SEEK(INVHDR.cFaccode,'SYCFACT')
ENDIF 
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

HdrLine3 = IIF(XINVNAME='Y',LEFT(HLINE1,22),'')
*-Note3 = IIF(!EMPTY(INVHDR.cFaccode),"PAY ONLY IN U.S FUNDS TO","")

*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
*Note3 = IIF(!EMPTY(INVHDR.cFaccode),LANG_ARPINVGM_Note1,"")
Note3 = IIF(!EMPTY(INVHDR.cFaccode),SYCFACT.CFACCOMP,"")
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

HdrLine4 = IIF(XINVNAME='Y',LEFT(HLINE2,22),'')
*-Note4 = IIF(!EMPTY(INVHDR.cFaccode),"THE CIT GROUP","")

*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
*Note4 = IIF(!EMPTY(INVHDR.cFaccode),LANG_ARPINVGM_Note2,"")
Note4 = IIF(!EMPTY(INVHDR.cFaccode),SYCFACT.CADDRESS1,"")
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

HdrLine5 = IIF(XINVNAME='Y',LEFT(HLINE3,22),'')
*-Note5 = IIF(!EMPTY(INVHDR.cFaccode),"P.O. BOX. 1036","")

*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
*Note5 = IIF(!EMPTY(INVHDR.cFaccode),LANG_ARPINVGM_Note3,"")
Note5 = IIF(!EMPTY(INVHDR.cFaccode),IIF(EMPTY(SYCFACT.CADDRESS2),SYCFACT.CADDRESS3,SYCFACT.CADDRESS2),"")
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

HdrLine6 = IIF(XINVNAME='Y',LEFT(HLINE4,22),'')
*-Note6 = IIF(!EMPTY(INVHDR.cFaccode),"CHARLOTTE NC 28201-1036","")

*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
*Note6 = IIF(!EMPTY(INVHDR.cFaccode),LANG_ARPINVGM_Note4,"")
Note6 = IIF(!EMPTY(INVHDR.cFaccode),TRIM(SYCFACT.CADDRESS3)+' '+TRIM(SYCFACT.CADDRESS4)+' '+TRIM(SYCFACT.CADDRESS5),"")
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

*-HdrLine7 = IIF(XINVNAME='Y' .AND. !EMPTY(HLINE5),'Telephone'+SPACE(10)+HLINE5,"")
HdrLine7 = IIF(XINVNAME='Y' .AND. !EMPTY(HLINE5),LANG_ARPINVGM_Tel+SPACE(10)+HLINE5,"")
HdrLine13 = SUBSTR(XBTNAME,1,40)
HdrLin132 = IIF(CURLOOP = '1',XSTNAME,'')
HdrLine14 = SUBSTR(XBTADDR1,1,40)
HdrLin142 = IIF(CURLOOP = '1',XSTADDR1,'')
HdrLine15 = SUBSTR(XBTADDR2,1,40)
HdrLin152 = IIF(CURLOOP = '1',XSTADDR2,'')
HdrLine16= SUBSTR(XBTADDR3,1,40) 
HdrLin162 = IIF(CURLOOP = '1',XSTADDR3,'')
ShipVia = PShipVia
PTermCode = LEFT(PTerms,15)
SELECT &lcheader
INSERT INTO &lcHeader (cInvoice ,hdrline3   ,note3   ,hdrline4   ,note4   ,hdrline5   ,note5   ,hdrline6   ,note6   ,hdrline7   ,hdrline13   ,hdrlin132   ,hdrline14   ,hdrlin142   ,hdrline15   ,hdrlin152   ,hdrline16   ,hdrlin162   ,ShipVia  );
  VALUES              (XINVOICE ,m.HdrLine3 ,m.Note3 ,m.HdrLine4 ,m.Note4 ,m.HdrLine5 ,m.Note5 ,m.HdrLine6 ,m.Note6 ,m.HdrLine7 ,m.HdrLine13 ,m.HdrLin132 ,m.HdrLine14 ,m.HdrLin142 ,m.HdrLine15 ,m.HdrLin152 ,m.HdrLine16 ,m.HdrLin162 ,m.ShipVia)

REPLACE PTermCode  WITH m.PTermCode;
        InvDate    WITH Invhdr.InvDate;
        Weight     WITH InvHdr.Weight;
        Cartons    WITH Invhdr.Cartons;
        Department WITH Invhdr.Dept
*E128427,1 MMT 06/16/2005,if invoice consalidated by DC print dist_ctr instead of Store[Start]
*REPLACE CustPO     WITH Invhdr.CustPO;
        Store      WITH Invhdr.Store;
        Rep1       WITH Invhdr.Rep1;
        Order      WITH InvHdr.Order;
        EnterDate  WITH ORDHDR.Entered;
        Account    WITH INVHDR.ACCOUNT;
        HasNotes   WITH 'F'
        
REPLACE CustPO     WITH Invhdr.CustPO;
        Store      WITH IIF(INVHDR.Consol = 'Y',Invhdr.dist_ctr,Invhdr.Store);
        Rep1       WITH Invhdr.Rep1;
        Order      WITH InvHdr.Order;
        EnterDate  WITH ORDHDR.Entered;
        Account    WITH INVHDR.ACCOUNT;
        HasNotes   WITH 'F'
*E128427,1 MMT 06/16/2005,if invoice consalidated by DC print dist_ctr instead of Store[End]


RETURN 
*-hfk, #038272, 07/06/2004
*-- End of lfGetInHdr.
*!*************************************************************
*! Name      : gfGetZone
*! Developer : Bassem Rafaat (BWA)
*! Date      : 03/19/2000
*! Purpose   : Get the zone to be printed in the invoice format.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*!
FUNCTION gfGetZone
PARAMETERS  lcUpsType,lcUpsFrom,lcToZip

RETURN IIF(!SEEK(lcUpsType+lcUpsFrom+lcToZip,lcSyszone),'',&lcSyszone..ZONE)
*!*************************************************************
*! Name      : lfCreateTemp
*! Developer : Heba Fathi (HFK)
*! Date      : 11/11/2004
*! Purpose   : create temporary files.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*!
FUNCTION lfCreateTemp
*-hfk,05/13/2005
*-DIMENSION laLines[11,4]
DIMENSION laLines[12,4]
*-hfk,05/13/2005
laLines[1,1]='Invoice'
laLines[1,2]='C'
laLines[1,3]=8
laLines[1,4]=0

laLines[2,1]='Required'
laLines[2,2]='N'
laLines[2,3]=8
laLines[2,4]=2

laLines[3,1]='Shipped'
laLines[3,2]='N'
laLines[3,3]=8
laLines[3,4]=2

laLines[4,1]='Price'
laLines[4,2]='N'
laLines[4,3]=8
laLines[4,4]=2

laLines[5,1]='Amount'
laLines[5,2]='N'
laLines[5,3]=19
laLines[5,4]=2

laLines[6,1]='TotAmnt'
laLines[6,2]='N'
laLines[6,3]=8
laLines[6,4]=2

laLines[7,1]='Dscrption'
laLines[7,2]='C'
laLines[7,3]=30
laLines[7,4]=0

laLines[8,1]='Dscrpt2'
laLines[8,2]='C'
laLines[8,3]=30
laLines[8,4]=0

laLines[9,1]='StyDesc'
laLines[9,2]='C'
laLines[9,3]=60
laLines[9,4]=0

laLines[10,1]='PackId'
laLines[10,2]='C'
laLines[10,3]=16
laLines[10,4]=0

laLines[11,1]='Count'
laLines[11,2]='N'
laLines[11,3]=1
laLines[11,4]=0

*-#B127927, 05/13/2005 HFK , add this field
laLines[12,1] = 'llPack'
laLines[12,2] = 'L'
laLines[12,3] = 1
laLines[12,4] = 0

*-#B127927, 05/13/2005 HFK , add this field
lcLines = loOGScroll.gfTempName()
gfCrtTmp(lcLines,@laLines,"Invoice",lcLines,.F.)

DIMENSION laHeader[45,4]
STORE '' TO laHeader
laHeader[1,1]='cInvoice'
laHeader[1,2]='C'
laHeader[1,3]=6
laHeader[1,4]=0

laHeader[2,1]='hdrline3'
laHeader[2,2]='C'
laHeader[2,3]=22
laHeader[2,4]=0

laHeader[3,1]='note3'
laHeader[3,2]='C'

*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
*laHeader[3,3]=28
laHeader[3,3]=50
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

laHeader[3,4]=0

laHeader[4,1]='hdrline4'
laHeader[4,2]='C'
laHeader[4,3]=22
laHeader[4,4]=0

laHeader[5,1]='note4'
laHeader[5,2]='C'

*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
*laHeader[5,3]=28
laHeader[5,3]=50
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

laHeader[5,4]=0

laHeader[6,1]='hdrline5'
laHeader[6,2]='C'
laHeader[6,3]=22
laHeader[6,4]=0

laHeader[7,1]='note5'
laHeader[7,2]='C'

*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
*laHeader[7,3]=28
laHeader[7,3]=50
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

laHeader[7,4]=0

laHeader[8,1]='hdrline6'
laHeader[8,2]='C'
laHeader[8,3]=22
laHeader[8,4]=0

laHeader[9,1]='note6'
laHeader[9,2]='C'

*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[Start]
*laHeader[9,3]=28
laHeader[9,3]=50
*! C201049,1 MMT 08/27/2008 Change Factor address to be read from sycfact table[End]

laHeader[9,4]=0

laHeader[10,1]='hdrline7'
laHeader[10,2]='C'
laHeader[10,3]=40
laHeader[10,4]=0

laHeader[11,1]='hdrline13'
laHeader[11,2]='C'
laHeader[11,3]=40
laHeader[11,4]=0

laHeader[12,1]='hdrlin132'
laHeader[12,2]='C'
laHeader[12,3]=30
laHeader[12,4]=0

laHeader[13,1]='hdrline14'
laHeader[13,2]='C'
laHeader[13,3]=40
laHeader[13,4]=0

laHeader[14,1]='hdrlin142'
laHeader[14,2]='C'
laHeader[14,3]=30
laHeader[14,4]=0

laHeader[15,1]='hdrline15'
laHeader[15,2]='C'
laHeader[15,3]=40
laHeader[15,4]=0

laHeader[16,1]='hdrlin152'
laHeader[16,2]='C'
laHeader[16,3]=30
laHeader[16,4]=0

laHeader[17,1]='hdrline16'
laHeader[17,2]='C'
laHeader[17,3]=40
laHeader[17,4]=0

laHeader[18,1]='hdrlin162'
laHeader[18,2]='C'
laHeader[18,3]=30
laHeader[18,4]=0

laHeader[19,1]='ShipVia'
laHeader[19,2]='C'
laHeader[19,3]=30
laHeader[19,4]=0

laHeader[20,1]='PTermCode'
laHeader[20,2]='C'
laHeader[20,3]=15
laHeader[20,4]=0

laHeader[21,1]='InvDate'
laHeader[21,2]='D'
laHeader[21,3]=10
laHeader[21,4]=0

laHeader[22,1]='Weight'
laHeader[22,2]='N'
laHeader[22,3]=8
laHeader[22,4]=2

laHeader[23,1]='Cartons'
laHeader[23,2]='N'
laHeader[23,3]=5
laHeader[23,4]=0

laHeader[24,1]='Department'
laHeader[24,2]='C'
laHeader[24,3]=5
laHeader[24,4]=0

laHeader[25,1]='CustPO'
laHeader[25,2]='C'
laHeader[25,3]=15
laHeader[25,4]=0

laHeader[26,1]='Store'
laHeader[26,2]='C'
laHeader[26,3]=8
laHeader[26,4]=0

laHeader[27,1]='Rep1'
laHeader[27,2]='C'
laHeader[27,3]=3
laHeader[27,4]=0

laHeader[28,1]='Order'
laHeader[28,2]='C'
laHeader[28,3]=6
laHeader[28,4]=0

laHeader[29,1]='EnterDate'
laHeader[29,2]='D'
laHeader[29,3]=10
laHeader[29,4]=0

laHeader[30,1]='Account'
laHeader[30,2]='C'
laHeader[30,3]=5
laHeader[30,4]=0

laHeader[31,1]='HasNotes'
laHeader[31,2]='C'
laHeader[31,3]=1
laHeader[31,4]=0
*--B128483,1 MMT 06/15/2005 fix bug of not printing complete Notes Field[Start]
laHeader[32,1]='MNotes'
laHeader[32,2]='M'
laHeader[32,3]=4
*--B128483,1 MMT 06/15/2005 fix bug of not printing complete Notes Field[Start]
laHeader[32,4]=0

laHeader[33,1]='TotCharge'
laHeader[33,2]='N'
laHeader[33,3]=14
laHeader[33,4]=2

laHeader[34,1]='Ship'
laHeader[34,2]='N'
laHeader[34,3]=7
laHeader[34,4]=0

laHeader[35,1]='ShipAmnt'
laHeader[35,2]='N'
laHeader[35,3]=14
laHeader[35,4]=2

laHeader[36,1]='Discount'
laHeader[36,2]='N'
laHeader[36,3]=13
laHeader[36,4]=2

laHeader[37,1]='TaxDesc'
laHeader[37,2]='C'
laHeader[37,3]=20
laHeader[37,4]=0

laHeader[38,1]='StringRat'
laHeader[38,2]='N'
laHeader[38,3]=5
laHeader[38,4]=2

laHeader[39,1]='TaxAmnt'
laHeader[39,2]='N'
laHeader[39,3]=13
laHeader[39,4]=2

laHeader[40,1]='PstRate'
laHeader[40,2]='N'
laHeader[40,3]=5
laHeader[40,4]=2

laHeader[41,1]='PstAmnt'
laHeader[41,2]='N'
laHeader[41,3]=13
laHeader[41,4]=2

laHeader[42,1]='HstRate'
laHeader[42,2]='N'
laHeader[42,3]=5
laHeader[42,4]=2

laHeader[43,1]='HstAmnt'
laHeader[43,2]='N'
laHeader[43,3]=13
laHeader[43,4]=2

laHeader[44,1]='TotFr8'
laHeader[44,2]='N'
laHeader[44,3]=14
laHeader[44,4]=2

laHeader[45,1]='TaxRefer'
laHeader[45,2]='C'
laHeader[45,3]=25
laHeader[45,4]=0

lcHeader = loOGScroll.gfTempName()
gfCrtTmp(lcHeader,@laHeader,"cInvoice",lcHeader,.F.)


