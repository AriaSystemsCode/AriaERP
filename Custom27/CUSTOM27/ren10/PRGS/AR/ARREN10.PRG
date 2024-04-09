*!*************************************************************************
*! Program file  : ARREN10.PRG
*! Program desc. : ASCII Invoice Importing Program for Rena Lange.
*! System        : Aria Advantage Series
*! Module        : Accounts Receivable
*! Date          : 11/28/2002
*! Developer     : Sameh Saiid Ezzat (SSE)
*! Refere To     : C102761
*!*************************************************************************
*B606793,1 SSE 12/18/2002 Fix bug of removing account validations.
*B606941,1 NAD 02/06/2003 File Pack_Hdr Not Found 
*B606970,1 NAD 03/18/2003 should be 12 character not 9 
*B607002,1 ABD 03/16/2003 The program should not reject the whole invoice if 
*B607002,1 ABD            The quanties in the order for one store doesn't 
*B607002,1 ABD            Match the quantities in the asci file.
*B607471,1 ABD 08/05/2003 Fix problem that the program duplicates the 
*B607471,1 ABD            Invoiced qty if the style entered in the order more than once. 
*B119562,1 SSE 09/08/2003 Fix bug of wrong summation when getting 2 consol. invoices for same account.
*!*************************************************************************
  
IF !gfSetup()
ENDIF
DO (gcScrDir + gcWinAppl + "\ARREN10.SPX")
*-- End of program.

*!*************************************************************************
*! Name      : lfvImport
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : To import an ASCII text file.
*!*************************************************************************
*! Example   :  lfvImport()
*!*************************************************************************
*
FUNCTION lfvImport
*** First we need to get style structure.
PRIVATE lnStylePos , lnStyleLen , lnClrPos , lnClrLen , lcSeparatr
STORE 0 TO lnStylePos , lnStyleLen , lnClrPos , lnClrLen
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'   && Free (Style)
    lnStyleLen = LEN(laItemSeg[lnCount,3])
    lnStylePos = laItemSeg[lnCount,4]
    lcSeparatr = laItemSeg[lnCount,6]
  ENDIF
  IF laItemSeg[lnCount,1]='C'   && Color
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR



PRIVATE lcTmpRejct , lcTmpLogRp , lcTmpOrder , lnAsciHand , lcOrdHdTag
lcTmpRejct = gfTempName()
lcTmpLogRp = gfTempName()
lcTmpOrder = gfTempName()

*** Create the Temp Text file that will be used in Log Report.
lnAsciHand = FOPEN(lcPathName)
IF lnAsciHand < 0  
  *-- Message : < Cannot open Ascii file. >
  *-- Buttons : <           OK             >
  =gfModalGen("TRM000000B00000","DIALOG",'','','Cannot open Ascii file.')
  RETURN
ENDIF

*** Create the temp table that will hold all invoice line to be imported.
*-- cInQty is used to hold the number that we will add the TotQty in it 
*-- It ranges from 1 to 8 for Example if cInQty is 5 this means we will 
*-- Add the TotQty in Qty5 in Invoice line.
CREATE TABLE (gcWorkDir + lcTmpRejct);
   (Invoice C(6), Order C(6), Account C(5), Store C(8), cCustPack C(8), CustPO C(15), ;
    ShipDate D(8), Style C(lnStyleLen), Color C(lnClrLen), Size C(5), ;
    Price N(12,2), TotQty N(9), cReject C(1), cReason C(150), cInQty C(1), mASCIILine M(10))
INDEX ON cReject + Invoice + Order + Store + Style + Color + Size TAG (lcTmpRejct)

*** Create the temp table that will hold all rejected lines for Log Report.
CREATE TABLE (gcWorkDir + lcTmpLogRp) (mStrRep M(10))

*** Loop on ASCII file to get Invoice line one by one.
PRIVATE lcInvLine , lcSetCent , lnSizeCnt , lcSizeCnt , llSizeOk , lcInv_Hold , lnCountLin
lnCountLin = 0
lcInv_Hold = gfGetMemVar('M_INVHOLD' ,gcAct_Comp)    && Invoice on hold orders
lcSetCent = SET('CENTURY')
SET CENTURY ON    && to make ship date with long format in CTOD()
DO WHILE !FEOF(lnAsciHand)
  lcInvLine = FGETS(lnAsciHand)
  
  lnCountLin = lnCountLin + 1   && Count lines in ASCII file.
  
  *** Get all the data from this Invoice line except Ship date
  m.ShipDate   = {}
  m.cReason    = ''
  m.Invoice    = PADL(SUBSTR(lcInvLine,1,ATC('|',lcInvLine)-1),6,'0')
  m.Order      = SUBSTR(lcInvLine,ATC('|',lcInvLine)+1,ATC('|',lcInvLine,2)-ATC('|',lcInvLine,1)-1)
  m.Store      = SUBSTR(lcInvLine,ATC('|',lcInvLine,2)+1,ATC('|',lcInvLine,3)-ATC('|',lcInvLine,2)-1)
  m.Store      = IIF(EMPTY(m.Store),SPACE(8),PADR(m.Store,8,' '))
  m.cCustPack  = SUBSTR(SUBSTR(lcInvLine,ATC('|',lcInvLine,3)+1,ATC('|',lcInvLine,4)-ATC('|',lcInvLine,3)-1),1,8)
  m.Account    = SUBSTR(lcInvLine,ATC('|',lcInvLine,4)+1,ATC('|',lcInvLine,5)-ATC('|',lcInvLine,4)-1)
  m.CustPO     = SUBSTR(lcInvLine,ATC('|',lcInvLine,5)+1,ATC('|',lcInvLine,6)-ATC('|',lcInvLine,5)-1)  
  *B606970,1 Style should be12 characters not 9 (Start)          
  *m.Style      = SUBSTR(SUBSTR(lcInvLine,ATC('|',lcInvLine,15)+1,ATC('|',lcInvLine,16)-ATC('|',lcInvLine,15)-1),1,9)
  m.Style      = SUBSTR(SUBSTR(lcInvLine,ATC('|',lcInvLine,15)+1,ATC('|',lcInvLine,16)-ATC('|',lcInvLine,15)-1),1,12)
  *B606970,1 Style should be12 characters not 9  (End)
  m.Color      = SUBSTR(SUBSTR(lcInvLine,ATC('|',lcInvLine,16)+1,ATC('|',lcInvLine,17)-ATC('|',lcInvLine,16)-1),1,6)
  m.Size       = ALLTRIM(SUBSTR(lcInvLine,ATC('|',lcInvLine,18)+1,ATC('|',lcInvLine,19)-ATC('|',lcInvLine,18)-1))
  m.Price      = VAL(SUBSTR(lcInvLine,ATC('|',lcInvLine,19)+1,ATC('|',lcInvLine,20)-ATC('|',lcInvLine,19)-1))/100
  *m.TotQty     = VAL(SUBSTR(lcInvLine,ATC('|',lcInvLine,20)+1))
  m.TotQty     = VAL(SUBSTR(lcInvLine,ATC('|',lcInvLine,20)+1,ATC('|',lcInvLine,21)-ATC('|',lcInvLine,20)-1))
  m.mASCIILine = lcInvLine
  
  *** Validate line total quantity.
  *-- Check for total if zero skip this line.
  *IF m.TotQty <= 0 OR (EMPTY(m.Invoice) AND EMPTY(m.Order) AND EMPTY(m.Account))
  IF m.TotQty <= 0 OR (EMPTY(m.Invoice) AND EMPTY(m.Order))
    LOOP
  ENDIF

  *** Validate Invoice number.
  *-- Check for this invoice if imported to Permenant Invoice Header file.
  IF SEEK(m.Invoice,'InvHdr')
    m.cReject = 'Y'
    m.cReason = 'This invoice exists in invoice header file.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF

  *-- Check for this invoice if imported to Temporary Invoice Header file.
  IF SEEK(m.Invoice,'InvHdrs')
    m.cReject = 'Y'
    m.cReason = 'This invoice exists in temporary invoice header file.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF

  *** Validate Order number.
  *-- Check for empty order.
  IF EMPTY(m.Order)
    m.cReject = 'Y'
    m.cReason = 'The order for Invoice # ' + m.Invoice + ' is empty.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF  

  *-- Check for Order in Order Header file.  
  SET ORDER TO TAG OrdHdr In OrdHdr
  IF SEEK('O' + m.Order,'OrdHdr')
    *-- Check if order is not opened.
    IF OrdHdr.Status <> 'O'
      IF OrdHdr.Status = 'H'
        IF lcInv_Hold = 'N'
          m.cReject = 'Y'
          m.cReason = 'Order# ' + m.Order + ' is on Hold, and Invoice on hold orders in AR setup is set to NO.'
          DO lpRejctAll   && Reject previous approved invoice lines.
          INSERT INTO (lcTmpRejct) FROM MEMVAR
          LOOP          
        ENDIF
      ELSE   
        m.cReject = 'Y'
        m.cReason = 'Order# ' + m.Order + ' is not open.'
        DO lpRejctAll   && Reject previous approved invoice lines.
        INSERT INTO (lcTmpRejct) FROM MEMVAR
        LOOP
      ENDIF  
    ENDIF
    
    *-- Check for lines in Order Line file.
    IF !SEEK('O'+OrdHdr.Order,'OrdLine')
      m.cReject = 'Y'
      m.cReason = 'Order# ' + m.Order + ' has no lines in Order Line file.'
      DO lpRejctAll   && Reject previous approved invoice lines.
      INSERT INTO (lcTmpRejct) FROM MEMVAR
      LOOP
    ENDIF

  ELSE
    m.cReject = 'Y'
    m.cReason = 'Order# ' + m.Order + ' does not exist.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF
  
  *B606793,1 Get Customer Account from Order Header according to Customer's mail. [Begin]
  m.Account = OrdHdr.Account
  *B606793,1 Get Customer Account from Order Header according to Customer's mail. [End]
  
  *B606793,1 Remove Account validations. [Begin]
  *** Validate Account.
  *IF EMPTY(m.Account)    && if found in customer file
  *  m.cReject = 'Y'
  *  m.cReason = 'The account code for Invoice # ' + m.Invoice + ' is empty.'
  *  DO lpRejctAll   && Reject previous approved invoice lines.
  *  INSERT INTO (lcTmpRejct) FROM MEMVAR
  *  LOOP
  *ENDIF

  *-- Check for account in customer file.
  *IF !SEEK('M' + m.Account,'Customer')    && if found in customer file
  *  m.cReject = 'Y'
  *  m.cReason = 'Account ' + m.Account + ' does not exist in Customer file.' 
  *  DO lpRejctAll   && Reject previous approved invoice lines.
  *  INSERT INTO (lcTmpRejct) FROM MEMVAR
  *  LOOP
  *ENDIF

  *-- Check for account in order header file.
  *lcOrdHdTag = ORDER('OrdHdr')
  *SET ORDER TO TAG OrdAcct IN OrdHdr
  *IF !SEEK(m.Account+'O'+m.Order,'OrdHdr')    && if found in customer file
  *  m.cReject = 'Y'
  *  m.cReason = 'Account ' + m.Account + ' differs from Sales Order Account.' 
  *  DO lpRejctAll   && Reject previous approved invoice lines.
  *  INSERT INTO (lcTmpRejct) FROM MEMVAR
  *  LOOP
  *ENDIF
  *SET ORDER TO TAG (lcOrdHdTag) IN OrdHdr
  *B606793,1 Remove Account validations. [End]

  *** Validate Store.  
  IF !EMPTY(m.Store) AND !SEEK('S' + m.Account + m.Store,'Customer')    && if found in customer file
    m.cReject = 'Y'
    m.cReason = 'Store# ' + m.Store + ' for Account# ' + m.Account + ' does not exist in Customer file.' 
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF
  
  IF !EMPTY(m.Store) AND SEEK('O'+m.Order,'OrdLine') AND !SEEK('O'+m.Order+m.Store,'OrdLine')
    m.cReject = 'Y'
    m.cReason = 'Store# ' + m.Store + ' does not exist in order lines for Order# ' + m.Order
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF

  *** Validate Ship Date.
  m.ShipDate  = SUBSTR(lcInvLine,ATC('|',lcInvLine,13)+1,ATC('|',lcInvLine,14)-ATC('|',lcInvLine,13)-1)
  IF LEN(SUBSTR(m.ShipDate,ATC('/',m.ShipDate,2)+1)) < 4
    m.cReject = 'Y'
    m.cReason = 'This date ' + m.ShipDate + ' is not a long format date.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    m.ShipDate = {}
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF
  
  IF EMPTY(CTOD(m.ShipDate))
    m.cReject = 'Y'
    m.cReason = 'This date ' + m.ShipDate + ' is not a valid date.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    m.ShipDate = {}
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF
  m.ShipDate = CTOD(m.ShipDate)

  *** Validate Style+Color.
  *-- Check for empty of color code.
  IF EMPTY(m.Color)
    m.cReject = 'Y'
    m.cReason = 'The Color code for Invoice # ' + m.Invoice + ' is empty.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP  
  ENDIF

  *-- Check if Color exist in Codes file.
  IF !SEEK('N'+PADR('COLOR',10)+m.Color,'Codes')
    m.cReject = 'Y'
    m.cReason = 'Color ' + m.Color + ' does not exist in codes file.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF

  *-- Check for empty of style
  IF EMPTY(m.Style)
    m.cReject = 'Y'
    m.cReason = 'The Style code for Invoice # ' + m.Invoice + ' is empty.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP  
  ENDIF

  *-- Check if style exist in Style file.  
  IF !SEEK(m.Style,'Style')
    m.cReject = 'Y'
    m.cReason = 'Style major ' + m.Style + ' does not exist in Style file.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF

  *-- Check if style + Color exist in Style file.
  IF !SEEK(PADR(m.Style,lnStyleLen) + lcSeparatr + PADR(m.Color,lnClrLen),'Style')
    m.cReject = 'Y'
    m.cReason = 'Style/Color ' + PADR(m.Style,lnStyleLen) + lcSeparatr + PADR(m.Color,lnClrLen) + ' does not exist in Style file.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF

  *-- Check if Style + Color exist in Order line for that order and store.
  IF !SEEK('O'+m.Order+m.Store+PADR(m.Style,lnStyleLen)+lcSeparatr+PADR(m.Color,lnClrLen),'OrdLine')
    m.cReject = 'Y'
    m.cReason = 'Style/Color ' + PADR(m.Style,lnStyleLen) + lcSeparatr + PADR(m.Color,lnClrLen) + ' does not exist in Order line file ' + ;
                'for Order# ' + m.Order + IIF(EMPTY(m.Store),'',' Store# ' + m.Store)
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF 
  
  *** Validate the Scale.
  *-- Check for empty of scale
  IF EMPTY(m.Size)
    m.cReject = 'Y'
    m.cReason = 'The size for Invoice # ' + m.Invoice + ' is empty.'
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP  
  ENDIF
    
  *-- Check if this scale exist in scale file  
  llSizeOk = .F.
  m.cInQty = ''
  IF SEEK(PADR(m.Style,lnStyleLen) + lcSeparatr + PADR(m.Color,lnClrLen),'Style')      
    IF SEEK('S' + Style.Scale,'Scale')
      FOR lnSizeCnt = 1 TO Scale.Cnt
        lcSizeCnt = STR(lnSizeCnt,1)
        IF m.Size = ALLTRIM(Scale.Sz&lcSizeCnt)
          llSizeOk = .T.
          m.cInQty = lcSizeCnt
          EXIT
        ENDIF
      ENDFOR

      IF !llSizeOk    && If size is not found in scale file
        m.cReject = 'Y'
        m.cReason = 'Size ' + m.Size + ' for Style/Color ' + Style.Style + ;
                    ' does not exist in all sizes for Scale ' + Style.Scale
        DO lpRejctAll   && Reject previous approved invoice lines.
        INSERT INTO (lcTmpRejct) FROM MEMVAR
        LOOP
      ENDIF      
    ELSE
      m.cReject = 'Y'
      m.cReason = 'Scale ' + Style.Scale + ' does not exist in Scale file.'
      DO lpRejctAll   && Reject previous approved invoice lines.
      INSERT INTO (lcTmpRejct) FROM MEMVAR
      LOOP
    ENDIF
  ENDIF
  
  *** Validate the price.
  IF SEEK('O'+m.Order+m.Store+PADR(m.Style,lnStyleLen)+lcSeparatr+PADR(m.Color,lnClrLen),'OrdLine') ;
     AND m.Price <> OrdLine.Price 
    m.cReject = 'Y'
    m.cReason = 'Price ' + ALLTRIM(STR(m.Price)) + ' does not match Order line price which is ' + ;
                ALLTRIM(STR(OrdLine.Price))
    DO lpRejctAll   && Reject previous approved invoice lines.
    INSERT INTO (lcTmpRejct) FROM MEMVAR
    LOOP
  ENDIF

  *** If all validations passed, add this invoice line as APPROVED...
  IF SEEK('N' + m.Invoice + m.Order + m.Store + m.Style + m.Color + m.Size,(lcTmpRejct))
    SELECT (lcTmpRejct)
    REPLACE TotQty WITH TotQty + m.TotQty
  ELSE
    m.cReject = 'N'
    INSERT INTO (lcTmpRejct) FROM MEMVAR
  ENDIF

  *** Reject previous approved invoice lines if we have rejected lines
  *** for this approved invoice line.
  DO lpRejctAll WITH 'Y'
ENDDO

*** At this point we need to build the Temp Invoice Header and Temp Invoice Line.
PRIVATE lcInvHdr, lcInvLine, lcConsInvH, lcConsInvD, lcUpsBox, lcPackHdr, lcInstHdr,;
        lcInstLin, lcAppCrdt
PRIVATE lcSadInvH, lcSadInvL, lcSadUpsB, lcSadInsH, lcSadInsL, lcSadOrdC
PRIVATE lcPackLine, lcOrdCanLn, lcCrtTmp, lcUpsTmp
STORE '' TO lcInvHdr, lcInvLine, lcConsInvH, lcConsInvD, lcUpsBox, lcPackHdr, lcInstHdr,;
        lcInstLin, lcAppCrdt
STORE '' TO lcSadInvH, lcSadInvL, lcSadUpsB, lcSadInsH, lcSadInsL, lcSadOrdC
STORE '' TO lcPackLine, lcOrdCanLn, lcCrtTmp, lcUpsTmp

PRIVATE llPack_Hdr , llPack_Lin , llStyDye
STORE .F. TO llPack_Hdr , llPack_Lin , llStyDye

IF 'AL' $ gcCmpModules
  llPikTkt  = gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'Ordpik','SH')
  SELECT PIKTKT
  SET FILTER TO STATUS <> 'X' AND STATUS <> 'C'
ENDIF

*** Add all the variables that are defined at the beginning of the Invoice Sales Order screen.
*STORE {}  TO ldDefInvDate,ldDefPstDate      && Invoice Date Posting Date
STORE gdSysDate TO ldDefInvDate,ldDefPstDate

STORE 0   TO lnCartons,lnSouComm1,lnSouComm2,lnActFolder,lnStyLngth
STORE 1   TO lnShipAddr,lnPstRule,lnWareHouse,lnShipName
STORE ''  TO lcShipName,lcShipAdd1,lcShipAdd2,lcShipAdd3,lcShipAdd4,lcShipAdd5,;
             lcBillName,lcBillAdd1,lcBillAdd2,lcBillAdd3,lcBillAdd4,lcBillAdd5,;
             lcFolder,lcTaxTitle,lcTaxBreak,lcSession,lcUpsType,lcOrdStat
STORE .F. TO llBrowse,llZoom,llIsCanada,llIsEngland,llMulCurr,llEditExRt,;
             llCod,llContinue,llSysDate,llNoShow,laSetups,llInstTerm,;
             llAddLine,llUpCnsLine,llOpnCrdt
STORE ""  TO lcInvLine,lcInvHdr,lcPackHdr,lcUpsBox,lcEngChrg,lcFlFields,;
             lcScFields,lcWinChrg,laWareHouses,lcStyHdr,lcEmptySty,;
             lcInstLin,lcInstHdr,lcAppCrdt,lcMjrMsk,lcStyMjr,lcConsInvH,lcConsInvD

STORE '' TO lcTEOM,lcTCod,lcOldValue,lcUpsTrack
STORE 0  TO lnTDaysDue,lnTerDiscR,lnLastMode
STORE 20 TO lnEomDay

STORE .F. TO m.lUpsIns,m.LNEWLINE,m.LPACKED,m.LBACKORD,llCnRemain
STORE 0   TO m.nTaxRate,m.nChrgTax,m.nMerchTax

lnSessNo = gnProgCopy
STORE 1 TO lnTerms,lnShipVia,lnSpcInst,lnSeason,lnDivision

lcorder = space(6)

STORE .T. TO llGoAndChk,llChkUnCom
STORE ''  TO lcAdTrnSeq,lcAcTrnSeq,lcHisSeq,lcGlSess,lcRepBat,lnUnCmSeRc
STORE .T. TO llUpdGlDif,llUpdMstGL

DECLARE laEngStyTax[1,2]
STORE 0 TO lnTaxRate  && Tax Rate at Style level  
STORE '' TO laEngStyTax

DIMENSION laSetups[26,2], laTRltFld[6,2]
laSetups[1,1]  = 'M_PACK'        && Use Style packs/Sku
laSetups[2,1]  = 'M_STY_COM'     && Commision at style level
laSetups[3,1]  = 'M_LN_NOTE'     && Add note to the invoice line  
laSetups[4,1]  = 'M_LINK_GL'     && Check for gl link
laSetups[5,1]  = 'M_WareHouse'   && Use maltiple locations
laSetups[6,1]  = 'M_GenOrNum'    && Enter order # manually
laSetups[7,1]  = 'M_COST_METH'   && Cost method
laSetups[8,1]  = 'M_DYELOT'      && Use dylot  
laSetups[9,1]  = 'M_TAX'         && Use taxes
laSetups[10,1] = 'M_TAX_RATE'    && Tax 
laSetups[11,1] = 'M_TAX_METH'    && Tax Method 
laSetups[12,1] = 'M_UPC_USE'     && use upc numbers 
laSetups[13,1] = 'M_DIV_LINK'    && gl link codes as devision level 
laSetups[14,1] = 'M_REP_COMM'    && Sales rep commission  
laSetups[15,1] = 'M_UPSBOX'      && track boxes for ups manifacturing
laSetups[16,1] = 'XAGINGTYPE'    && Aging ar by date / terms   
laSetups[17,1] = 'XPOSTFINV'     && post factored invoice to customer
laSetups[18,1] = 'XUPSFROM'      && UPS Shipper Code         
laSetups[19,1] = 'M_CRDT_LMT'    && warn above credit limit
laSetups[20,1] = 'M_EDTPRICE'    && edit style price Y/N 
laSetups[21,1] = 'M_INVBULK'     && invoice bulk orders
laSetups[22,1] = 'M_INVHOLD'     && invoice hold order 
laSetups[23,1] = 'M_BACKORD'     && Back orders 
laSetups[24,1] = 'M_TAX_DESC'    && Tax Name 
laSetups[26,1] = 'M_HST_RATE' && HST Tax Rate
=gfGetMemVar(@laSetups,gcAct_Comp)

*-- Payment Terms Related Fields
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnTerDiscR'
laTRltFld[2,1] = 'EOM'
laTRltFld[2,2] = 'lcTEOM'
laTRltFld[3,1] = 'NTERDUED'
laTRltFld[3,2] = 'lnTDaysDue'
laTRltFld[4,1] = 'CODYN'
laTRltFld[4,2] = 'lcTCod'
laTRltFld[5,1] = 'LINSTALLM'
laTRltFld[5,2] = 'llInstTerm'
laTRltFld[6,1] = 'EOMDAY'
laTRltFld[6,2] = 'lnEomDay'

*** This procedure to create all temp files exactly like Invoice Sales Order screen
DO lpCreatFls
  
IF 'AL' $ gcCmpModules
  llPack_Hdr = gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
  llPack_Lin = gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
ENDIF
llStyDye = gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
  
SELECT Style
SET RELATION TO 'S' + Scale INTO Scale
SELECT (lcInvLine)
SET RELATION TO Style INTO Style
SET RELATION TO Style + cWareCode + Dyelot INTO StyDye ADDITIVE

*** We need to have a file that have records for each 
*** Invoice + Order + Store + Style + Color + all sizes in one line
PRIVATE lcTmpAllSz
lcTmpAllSz = gfTempName()
CREATE TABLE (gcWorkDir + lcTmpAllSz);
   (Invoice C(6), Order C(6), Account C(5), Store C(8), cCustPack C(8), CustPO C(15), ;
    ShipDate D(8), Style C(lnStyleLen), Color C(lnClrLen), Price N(12,2), TotQty N(9), ;
    Qty1 N(6), Qty2 N(6), Qty3 N(6), Qty4 N(6), Qty5 N(6), Qty6 N(6), Qty7 N(6), ;
    Qty8 N(6), cReject C(1), cReason C(150))
INDEX ON cReject + Order + Store + Style + Color TAG OrderTag
INDEX ON cReject + Invoice + Order + Store + Style + Color TAG (lcTmpAllSz)

SELECT (lcTmpRejct)
IF SEEK('N')     && Get all approved records
  SCAN REST WHILE cReject + Invoice + Order + Store + Style + Color + Size = 'N'
    SCATTER MEMVAR MEMO
    lcInQty = m.cInQty
    IF SEEK('N' + Invoice + Order + Store + Style + Color,(lcTmpAllSz))
      SELECT (lcTmpAllSz)
      REPLACE Qty&lcInQty WITH Qty&lcInQty + m.TotQty , TotQty WITH TotQty + m.TotQty
    ELSE
      SELECT (lcTmpAllSz)
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE Qty&lcInQty WITH Qty&lcInQty + m.TotQty
    ENDIF
  ENDSCAN  
ENDIF

*** Compare all approved line quantity with ordline qtys
DIMENSION laOrdLnQty[8] , laInvLnQty[8] , laTmpLnQty[8]
PRIVATE lcInvLnTag , lcInvLnKey , lcStyleKey , lcOrderNum , lcInvNum


*B607002,1 ABD - Define Variable For the Store. [Begin]
PRIVATE lcStoreOrd
*B607002,1 ABD - [End]

lcInvLnTag = ORDER('InvLines')
SET ORDER TO TAG OrdLines IN InvLines
SELECT (lcTmpAllSz)
SET ORDER TO TAG OrderTag
=SEEK('N')
SCAN REST WHILE cReject + Order + Store + Style + Color = 'N'
  lcInvNum   = Invoice
  lcOrderNum = Order
  
  *B607002,1 ABD - Define Variable For the Store. [Begin]
  lcStoreOrd = Store
  *B607002,1 ABD - [End]
  
  lcInvLnKey = Order+Store+PADR(Style,lnStyleLen) + lcSeparatr + PADR(Color,lnClrLen)
  lcStyleKey = Order + Store + Style + Color
  laTmpLnQty = 0
  SCAN REST WHILE cReject + Order + Store + Style + Color = 'N' + lcStyleKey
    FOR lnSizeCnt = 1 TO 8
      lcSizeCnt = STR(lnSizeCnt,1)
      laTmpLnQty[lnSizeCnt] = laTmpLnQty[lnSizeCnt] + &lcTmpAllSz..Qty&lcSizeCnt
    ENDFOR  
  ENDSCAN
  IF !BOF()
    SKIP -1
  ENDIF
  
  laOrdLnQty = 0
  IF SEEK('O'+lcInvLnKey,'OrdLine')    
    SELECT OrdLine
    SCAN REST WHILE cOrdType + Order + Store + Style + STR(LineNo,6) = 'O' + lcInvLnKey
      FOR lnSizeCnt = 1 TO 8
        lcSizeCnt = STR(lnSizeCnt,1)
        laOrdLnQty[lnSizeCnt] = laOrdLnQty[lnSizeCnt] + OrdLine.Qty&lcSizeCnt
      ENDFOR
    ENDSCAN
    
    FOR lnSizeCnt = 1 TO 8
      lcSizeCnt = STR(lnSizeCnt,1)
      IF laOrdLnQty[lnSizeCnt] < laTmpLnQty[lnSizeCnt]
        SELECT (lcTmpAllSz)
        
        *B607002,1 ABD - Remark the next lines and change the messgae. [Begin]
        * Invoice #001 on order # 001 for store # has bee rejected, because the invoice quantity exceeds the order quantity.
        *REPLACE cReject WITH 'Y' , cReason WITH 'All invoices for Order# ' + lcOrderNum + ;
        *' are rejected, because quantities in invoices exceed order quantities'            
        *DO lpOrdrRjct WITH lcOrderNum
        *DO lpScndRjct WITH lcInvNum
        REPLACE cReject WITH 'Y' , cReason WITH 'Invoice # '+ lcInvNum +' on Order# ' + lcOrderNum + ' for store # '+lcStoreOrd +;
                ' has been rejected, because the invoice quantity exceeds the order quantity.'
        DO lpOrdrRjct WITH lcOrderNum , lcStoreOrd
        DO lpScndRjct WITH lcInvNum , lcOrderNum , lcStoreOrd
        *B607002,1 ABD - [End]

        =SEEK('N',(lcTmpAllSz))
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  
  laInvLnQty = 0
  IF SEEK(lcInvLnKey + 'N','InvLines')
    SELECT InvLines
    SCAN REST WHILE Order + Store + Style + Consol = lcInvLnKey + 'N'
      FOR lnSizeCnt = 1 TO 8
        lcSizeCnt = STR(lnSizeCnt,1)
        laInvLnQty[lnSizeCnt] = laInvLnQty[lnSizeCnt] + InvLines.Qty&lcSizeCnt
      ENDFOR
    ENDSCAN  

    FOR lnSizeCnt = 1 TO 8
      lcSizeCnt = STR(lnSizeCnt,1)
      IF laInvLnQty[lnSizeCnt] + laTmpLnQty[lnSizeCnt] > laOrdLnQty[lnSizeCnt]
        SELECT (lcTmpAllSz)
        REPLACE cReject WITH 'Y' , cReason WITH 'All invoices for Order# ' + lcOrderNum + ;
                ' are rejected, because quantities in invoices and temporary invoices ' + ;
                'exceed order quantities'
        *B607002,1 ABD - Remark the next comming code. [Begin]
        *DO lpOrdrRjct WITH lcOrderNum
        *DO lpScndRjct WITH lcInvNum
        DO lpOrdrRjct WITH lcOrderNum , lcStoreOrd
        DO lpScndRjct WITH lcInvNum , lcOrderNum , lcStoreOrd
        *B607002,1 ABD - [End]
        
        =SEEK('N',(lcTmpAllSz))
        EXIT
      ENDIF
    ENDFOR
  ENDIF
ENDSCAN
SET ORDER TO TAG (lcTmpAllSz) IN (lcTmpAllSz)
SET ORDER TO TAG (lcInvLnTag) IN InvLines
*** Compare all Invoice quantity with Temp Invoice for same orders.
PRIVATE lnInvQty , lcInvoice , lcOldTag , lnOrderQty , lnOpenQty
STORE 0 TO lnOrderQty , lnInvQty , lnOpenQty
lcOldTag = ORDER('InvHdrs')
SET ORDER TO TAG SDOrder IN InvHdrS
SELECT (lcTmpAllSz)
=SEEK('N')
lcInvoice = Invoice
lcOrderNum = Order

DO WHILE .T.
  SELECT (lcTmpAllSz)

  *B607002,1 ABD - Check if EOF. [Begin]
  IF EOF()
    EXIT
  ENDIF
  lcStoreOrd = Store
  *B607002,1 ABD - [End]
  
  IF Invoice == lcInvoice
    lnInvQty = lnInvQty + TotQty
    
    IF !(Order == lcOrderNum)
      IF SEEK(lcOrderNum+'N','InvHdrS')
        SELECT InvHdrs
        SCAN REST WHILE Order+Consol = lcOrderNum + 'N'
          lnOrderQty = lnOrderQty + Ship
        ENDSCAN
      ENDIF

      IF SEEK('O'+lcOrderNum,'OrdHdr')
        lnOpenQty = lnOpenQty + OrdHdr.Open
        IF lnOrderQty + lnInvQty > lnOpenQty
          SELECT (lcTmpAllSz)
          IF EOF()
            =SEEK('N'+lcInvoice,(lcTmpAllSz))
          ENDIF
          REPLACE cReject WITH 'Y' , cReason WITH 'Invoice# ' + lcInvoice + ;
                  ' is rejected, because its quantity exceeds order quantity ' + ;
                  'when added to the temporary Invoices that have the same Order# ' + lcOrderNum
                  
          *B607002,1 ABD - Send new parmater to the new function. [Begin]
          *DO lpScndRjct WITH lcInvoice
          DO lpScndRjct WITH lcInvoice , lcOrderNum , lcStoreOrd
          *B607002,1 ABD - [End]
          
          =SEEK('N',(lcTmpAllSz))
        ENDIF  
      ENDIF
     
      lcInvoice = &lcTmpAllSz..Invoice
      lcOrderNum = &lcTmpAllSz..Order
      STORE 0 TO lnInvQty , lnOrderQty , lnOpenQty
    ENDIF
  ELSE  
    IF SEEK(lcOrderNum+'N','InvHdrS')
      SELECT InvHdrs
      SCAN REST WHILE Order+Consol = lcOrderNum + 'N'
        lnOrderQty = lnOrderQty + Ship
      ENDSCAN
    ENDIF
    
    IF SEEK('O'+lcOrderNum,'OrdHdr')
      lnOpenQty = lnOpenQty + OrdHdr.Open
      IF lnOrderQty + lnInvQty > lnOpenQty
        SELECT (lcTmpAllSz)
        IF EOF()
          =SEEK('N'+lcInvoice,(lcTmpAllSz))
        ENDIF
        REPLACE cReject WITH 'Y' , cReason WITH 'Invoice# ' + lcInvoice + ;
                ' is rejected, because its quantity exceeds order quantity ' + ;
                'when added to the temporary Invoices that have the same Order# ' + lcOrderNum

          *B607002,1 ABD - Send new paramter to the function. [begin]
          *DO lpScndRjct WITH lcInvoice
          DO lpScndRjct WITH lcInvoice , lcOrderNum , lcStoreOrd
          *B607002,1 ABD - [End]

        =SEEK('N',(lcTmpAllSz))
      ENDIF  
    ENDIF
    
    lcInvoice = &lcTmpAllSz..Invoice
    lcOrderNum = &lcTmpAllSz..Order
    STORE 0 TO lnInvQty , lnOrderQty
  ENDIF
  SELECT (lcTmpAllSz)
  IF EOF()
    EXIT
  ELSE
    SKIP
  ENDIF
ENDDO
SET ORDER TO TAG (lcOldTag) IN InvHdrS

*** We need to have all orders that are approved for each invoice added to Temp Orders file.
SELECT DISTINCT Invoice,Order,Store,ShipDate,'N' AS Consol ;
       FROM (lcTmpAllSz) ;
       WHERE cReject + Invoice + Order + Store + Style + Color = 'N' ;
       INTO TABLE (lcTmpOrder)
INDEX ON Invoice TAG (lcTmpOrder)
*** This scan to select whether these invoices are consolidated or multi store.
LOCATE
PRIVATE lcStoreNum , lnInvRecNo
lcInvoice = Invoice
lcOrderNum = Order
lcStoreNum = STORE
SCAN
  IF Invoice == lcInvoice
    IF Order = lcOrderNum
      IF !(Store == lcStoreNum)
        lnInvRecNo = RECNO(lcTmpOrder)
        IF SEEK(lcInvoice,(lcTmpOrder))
          REPLACE REST Consol WITH 'Y' WHILE Invoice = lcInvoice
        ENDIF
        IF BETWEEN(lnInvRecNo,1,RECCOUNT(lcTmpOrder))
          GOTO lnInvRecNo IN (lcTmpOrder)
        ENDIF
        lcStoreNum = STORE
      ENDIF
    ELSE
      lnInvRecNo = RECNO(lcTmpOrder)
      IF SEEK(lcInvoice,(lcTmpOrder))
        REPLACE REST Consol WITH 'Y' WHILE Invoice = lcInvoice
      ENDIF
      IF BETWEEN(lnInvRecNo,1,RECCOUNT(lcTmpOrder))
        GOTO lnInvRecNo IN (lcTmpOrder)
      ENDIF
      lcOrderNum = Order
    ENDIF
  ELSE
    lcInvoice = Invoice
    lcOrderNum = Order
    lcStoreNum = STORE  
  ENDIF  
ENDSCAN

LOCATE
lcInvoice = ''
lcOrderNum = ''
SCAN
  IF Invoice == lcInvoice AND Order == lcOrderNum
    LOOP
  ENDIF
  lcInvoice = Invoice
  lcOrderNum = Order      
  =lfGetOrder()
ENDSCAN
=lpShow()

*** Close all files that was opened while updating Temp Invoice Header and Lines.
SELECT Style
SET RELATION TO 
SELECT (lcInvLine)
SET RELATION TO 

IF llPack_Hdr
  USE IN Pack_Hdr
ENDIF
IF llPack_Lin
  USE IN Pack_Lin
ENDIF
IF llStyDye
  USE IN StyDye
ENDIF

*** We have to add all errors to Log Report file.
SELECT (lcTmpRejct)
lcInvoice = ''
IF SEEK('Y')     && Get all rejected records
  m.mStrRep = ''
  INSERT INTO (lcTmpLogRp) FROM MEMVAR
  SCAN REST WHILE cReject + Invoice + Order + Store + Style + Color + Size = 'Y'
    *IF !(Invoice == lcInvoice)
    *  SELECT (lcTmpLogRp)
    *  REPLACE mStrRep WITH mStrRep + 'Invoice ' + &lcTmpRejct..Invoice + CHR(13) + CHR(13)
    *  lcInvoice = &lcTmpRejct..Invoice
    *ENDIF
    
    IF !EMPTY(&lcTmpRejct..cReason)

      IF !(&lcTmpRejct..Invoice == lcInvoice)
        SELECT (lcTmpLogRp)
        REPLACE mStrRep WITH mStrRep + 'Invoice ' + &lcTmpRejct..Invoice + CHR(13) + CHR(13)
        lcInvoice = &lcTmpRejct..Invoice
      ENDIF

      SELECT (lcTmpLogRp)
      REPLACE mStrRep WITH mStrRep + ALLTRIM(SUBSTR(&lcTmpRejct..mASCIILine,1,255)) + ;
              CHR(13) + &lcTmpRejct..cReason + CHR(13) + CHR(13)
    ENDIF 
  ENDSCAN  
ENDIF
SELECT (lcTmpAllSz)
lcInvoice = ''
IF SEEK('Y')     && Get all rejected records
  IF !SEEK('Y',(lcTmpRejct))
    m.mStrRep = ''
    INSERT INTO (lcTmpLogRp) FROM MEMVAR
  ENDIF  
  SCAN REST WHILE cReject + Invoice + Order + Store + Style + Color + Size = 'Y'
    *IF !(Invoice == lcInvoice)
    *  SELECT (lcTmpLogRp)
    *  REPLACE mStrRep WITH mStrRep + 'Invoice ' + &lcTmpAllSz..Invoice + CHR(13) + CHR(13)
    *  lcInvoice = &lcTmpAllSz..Invoice
    *ENDIF
    
    IF !EMPTY(&lcTmpAllSz..cReason)
      IF !(Invoice == lcInvoice)
        SELECT (lcTmpLogRp)
        REPLACE mStrRep WITH mStrRep + 'Invoice ' + &lcTmpAllSz..Invoice + CHR(13) + CHR(13)
        lcInvoice = &lcTmpAllSz..Invoice
      ENDIF
    
      SELECT (lcTmpLogRp)
      REPLACE mStrRep WITH mStrRep + &lcTmpAllSz..cReason + CHR(13) + CHR(13)
    ENDIF 
  ENDSCAN  
ENDIF

*** Check if ASCII file is empty.
IF RECCOUNT(lcTmpLogRp)=0 AND RECCOUNT(lcTmpAllSz)=0 AND lnCountLin > 0
  m.mStrRep = 'ASCII text file either contains lines with zero quantity or not a valid lines.'
  INSERT INTO (lcTmpLogRp) FROM MEMVAR
ENDIF

*** Display the Report Log screen.
SELECT (lcTmpLogRp)
LOCATE
IF !EOF()
  lcWinTitl = 'ASCII File Rejection Report'
  DO (gcScrDir + gcWinAppl + "\ARREN20.SPX")
ENDIF  

*** We need to display the Imported Invoice screen to enable user to select Invoices to import
SELECT (lcInvHdr)
SET ORDER TO TAG Consol

*** Copy Invoice Header file to another file for browsing.
=SEEK('N')
SCAN REST WHILE Consol+Account+cSadInv+cDivision+cCurrCode+CDIVISION = 'N'
  lcInvKey = Account+cSadInv+cDivision+cCurrCode+CDIVISION
  lnInvRecNo = RECNO()    
  IF SEEK('Y'+lcInvKey,(lcInvHdr))
    lcInvNum = cSadInv
    REPLACE Choice WITH 'Y'
    REPLACE cSelect WITH '»' FOR cSadInv = lcInvNum
  ELSE
    IF SEEK('N'+lcInvKey,(lcInvHdr))
      lcInvNum = cSadInv
      REPLACE Choice WITH 'Y'
      REPLACE cSelect WITH '»' FOR cSadInv = lcInvNum
    ENDIF
  ENDIF
  IF BETWEEN(lnInvRecNo,1,RECCOUNT(lcInvHdr))
    GOTO lnInvRecNo IN (lcInvHdr)
  ENDIF
ENDSCAN
SUM Ship,ShipAmt TO lnTotShp , lnToShpAmt FOR Choice = 'Y' AND cSelect = '»'

LOCATE
IF !EOF()
  DECLARE laTerms[1,2] , laCodes[1,10]
  PRIVATE lnMark , lcCurInvNo , lnApproved , lnTerms , lnTotShp , lnToShpAmt , llProceed
  llProceed = .F.
  STORE 1 TO lnApproved , lnTerms
  STORE '' TO lcCurInvNo , laCodes
  STORE 0 TO lnTotShp , lnToShpAmt , lnMark   && lnMark hold the browse record pointer.

  *--Payment Terms Code
  laCodes[1,1]  = 'CTERMCODE'
  laCodes[1,2]  = 'laTerms'
  laCodes[1,3]  = 'lnTerms'
  laCodes[1,4]  = ''
  laCodes[1,5]  = .F.
  laCodes[1,6]  = .F.
  laCodes[1,7]  = lcInvHdr
  laCodes[1,8]  = lcInvHdr
  laCodes[1,9]  = 'Account+Order+cSadInv+Store+PikTkt'
  laCodes[1,10] = 'cTermCode'
  
  PUSH KEY
  ON KEY LABEL ALT+A ACTIVATE WINDOW (lcBrTtl1)  
  DO (gcScrDir + gcWinAppl + "\ARREN30.SPX")
  ON KEY LABEL ALT+A
  POP KEY
  IF llProceed
    =lfSadSav()
  ENDIF  
ENDIF

SET CENTURY &lcSetCent    && Restore century settings.

*-- Close all Temp file and erase them all.
IF USED(lcTmpRejct)
  USE IN (lcTmpRejct)
ENDIF
ERASE (gcWorkDir+lcTmpRejct+".DBF")
ERASE (gcWorkDir+lcTmpRejct+".CDX")
ERASE (gcWorkDir+lcTmpRejct+".FPT")  

IF USED(lcTmpLogRp)
  USE IN (lcTmpLogRp)
ENDIF
ERASE (gcWorkDir+lcTmpLogRp+".DBF")
ERASE (gcWorkDir+lcTmpLogRp+".CDX")
ERASE (gcWorkDir+lcTmpLogRp+".FPT")  

IF USED(lcTmpOrder)
  USE IN (lcTmpOrder)
ENDIF
ERASE (gcWorkDir+lcTmpOrder+".DBF")
ERASE (gcWorkDir+lcTmpOrder+".CDX")

IF USED(lcTmpAllSz)
  USE IN (lcTmpAllSz)
ENDIF
ERASE (gcWorkDir+lcTmpAllSz+".DBF")
ERASE (gcWorkDir+lcTmpAllSz+".CDX")
=FCLOSE(lnAsciHand)

*** The code below closes all Temp files that are used in Invoice Sales Order screen.
IF USED(lcInvHdr)
  USE IN (lcInvHdr)
ENDIF
ERASE (gcWorkDir+lcInvHdr+".DBF")
ERASE (gcWorkDir+lcInvHdr+".CDX")
  
IF USED(lcInvLine)
  USE IN (lcInvLine)
ENDIF
ERASE (gcWorkDir+lcInvLine+".DBF")
ERASE (gcWorkDir+lcInvLine+".CDX")
ERASE (gcWorkDir+lcInvLine+".FPT")
  
IF USED(lcUpsBox)
  USE IN (lcUpsBox)
ENDIF
ERASE (gcWorkDir+lcUpsBox+".DBF")
ERASE (gcWorkDir+lcUpsBox+".CDX")
  
IF USED(lcInstHdr)
  USE IN (lcInstHdr)
ENDIF
ERASE (gcWorkDir+lcInstHdr+".DBF")
ERASE (gcWorkDir+lcInstHdr+".CDX")
  
IF USED(lcInstLin)
  USE IN (lcInstLin)
ENDIF
ERASE (gcWorkDir+lcInstLin+".DBF")
ERASE (gcWorkDir+lcInstLin+".CDX")

IF USED(lcAppCrdt)
  USE IN (lcAppCrdt)
ENDIF
ERASE (gcWorkDir+lcAppCrdt+".DBF")
ERASE (gcWorkDir+lcAppCrdt+".CDX")
  
IF USED(lcOrdCanLn)
  USE IN (lcOrdCanLn)
ENDIF
ERASE (gcWorkDir+lcOrdCanLn+".DBF")
ERASE (gcWorkDir+lcOrdCanLn+".CDX")
  
IF USED(lcConsInvH)
  USE IN (lcConsInvH)
ENDIF
ERASE (gcWorkDir+lcConsInvH+".DBF")
ERASE (gcWorkDir+lcConsInvH+".CDX")

IF USED(lcConsInvD)
  USE IN (lcConsInvD)
ENDIF
ERASE (gcWorkDir+lcConsInvD+".DBF")
ERASE (gcWorkDir+lcConsInvD+".CDX")
ERASE (gcWorkDir+lcConsInvD+".FPT")

IF USED(lcSadInvH)
  USE IN (lcSadInvH)
ENDIF
ERASE (gcWorkDir+lcSadInvH+".DBF")
ERASE (gcWorkDir+lcSadInvH+".CDX")

IF USED(lcSadInvL)
  USE IN (lcSadInvL)
ENDIF
ERASE (gcWorkDir+lcSadInvL+".DBF")
ERASE (gcWorkDir+lcSadInvL+".CDX")

IF USED(lcSadUpsB)
  USE IN (lcSadUpsB)
ENDIF
ERASE (gcWorkDir+lcSadUpsB+".DBF")
ERASE (gcWorkDir+lcSadUpsB+".CDX")

IF USED(lcSadInsH)
  USE IN (lcSadInsH)
ENDIF
ERASE (gcWorkDir+lcSadInsH+".DBF")
ERASE (gcWorkDir+lcSadInsH+".CDX")

IF USED(lcSadInsL)
  USE IN (lcSadInsL)
ENDIF
ERASE (gcWorkDir+lcSadInsL+".DBF")
ERASE (gcWorkDir+lcSadInsL+".CDX")

IF USED(lcOrdCanLn)
  USE IN (lcOrdCanLn)
ENDIF
ERASE (gcWorkDir+lcOrdCanLn+".DBF")
ERASE (gcWorkDir+lcOrdCanLn+".CDX")

IF 'AL' $ gcCmpModules
  IF llPikTkt
    USE IN PIKTKT
  ELSE
    SELECT PIKTKT
    SET FILTER TO
  ENDIF
ENDIF  
*-- End of lfvImport.

*!*************************************************************************
*! Name      : lfvGetFile
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Function to get the Excel file data Dir & File.
*!*************************************************************************
*! Example   : lfvGetFile()
*!*************************************************************************
*
FUNCTION lfvGetFile
PARAMETERS lcVarNam

lcPathName  = GETFILE('TXT', 'ASCII Path : ','Select') 

*-- Check if File Is .TXT file.
IF !(".TXT" $ lcPathName)
  =gfModalGen("TRM000000B00000","ALERT",'','',"You have to select an ASCII file.")
  lcPathName = ''
  DO lpDispPath
  RETURN
ENDIF

*-- Check if File exist.
IF !FILE(lcPathName)
  =gfModalGen("TRM000000B00000","ALERT",'','',"This file does not exist.")
  lcPathName = ''
  DO lpDispPath
  RETURN
ENDIF

DO lpDispPath
*-- End Of lfvGetFile

*!*************************************************************************
*! Name      : lpDispPath
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Display the path.
*!*************************************************************************
*! Example   : DO lpDispPath
*!*************************************************************************
*
PROCEDURE lpDispPath
IF EMPTY(lcPathName)
  SHOW GET PbImport DISABLE
ELSE
  SHOW GET PbImport ENABLE
ENDIF
SHOW GET lcPathName
*-- End of lpDispPath

*!*************************************************************************
*! Name      : lpRejctAll
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Reject all previous approved Invoice lines in lcTmpRejct.
*!*************************************************************************
*! Example   : DO lpRejctAll
*!*************************************************************************
*
PROCEDURE lpRejctAll
PARAMETER lcRejected
PRIVATE lcALias
lcAlias = ALIAS()

IF !EMPTY(lcRejected)
  IF !SEEK('Y'+m.Invoice,(lcTmpRejct))
    RETURN
  ENDIF
ENDIF  

IF SEEK('N' + m.Invoice,(lcTmpRejct))
  DO WHILE SEEK('N' + m.Invoice,(lcTmpRejct))
    SELECT (lcTmpRejct)
    REPLACE cReject WITH 'Y'
  ENDDO
ENDIF

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of lpRejctAll.

*!*************************************************************************
*! Name      : lpScndRjct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Reject all previous approved Invoice lines in lcTmpAllSz.
*!*************************************************************************
*! Example   : DO lpScndRjct
*!*************************************************************************
*
PROCEDURE lpScndRjct

*B607002,1 ABD - Receive new paramter for this function [begin]
*PARAMETER lcInvNo
PARAMETER lcInvNo , lcOrdNum , lcStrOrd
lcOrdNum = IIF(Type('lcOrdNum') # 'C','',lcOrdNum)
lcStrOrd = IIF(Type('lcStrOrd') # 'C','',lcStrOrd)
*B607002,1 ABD - [End]

PRIVATE lcALias , lnRecNo , lcCurTag
lcAlias = ALIAS()
lnRecNo = RECNO(lcTmpAllSz)
lcCurTag = ORDER(lcTmpAllSz)
SET ORDER TO TAG (lcTmpAllSz) IN (lcTmpAllSz)

*B607002,1 ABD - Check with the store Value.[Begin]
*IF SEEK('N' + lcInvNo,(lcTmpAllSz))
*DO WHILE SEEK('N' + lcInvNo,(lcTmpAllSz))
IF SEEK('N' + lcInvNo + lcOrdNum + lcStrOrd,(lcTmpAllSz))
  DO WHILE SEEK('N' + lcInvNo + lcOrdNum + lcStrOrd,(lcTmpAllSz))
  *B607002,1 ABD - [End]
    SELECT (lcTmpAllSz)
    REPLACE cReject WITH 'Y'
  ENDDO
ENDIF

SET ORDER TO TAG (lcCurTag) IN (lcTmpAllSz)
IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpAllSz))
  GOTO lnRecNo IN (lcTmpAllSz)
ENDIF

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of lpScndRjct

*!*************************************************************************
*! Name      : lpOrdrRjct
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Reject all previous approved Invoice lines in lcTmpAllSz.
*!             for specific order
*!*************************************************************************
*! Example   : DO lpOrdrRjct
*!*************************************************************************
*
PROCEDURE lpOrdrRjct

*B607002,1 ABD - receive new paramter. [Begin]
*PARAMETER lcOrdNum
PARAMETER lcOrdNum , lcStrOrd
*B607002,1 ABD - [End]

PRIVATE lcALias , lnRecNo , lcCurTag
lcAlias = ALIAS()
lnRecNo = RECNO(lcTmpAllSz)
lcCurTag = ORDER(lcTmpAllSz)
SET ORDER TO TAG OrderTag IN (lcTmpAllSz)

*B607002,1 ABD - Check with the store value. [Begin]
*IF SEEK('N' + lcOrdNum,(lcTmpAllSz))
*DO WHILE SEEK('N' + lcOrdNum,(lcTmpAllSz))
IF SEEK('N' + lcOrdNum + lcStrOrd,(lcTmpAllSz))
  DO WHILE SEEK('N' + lcOrdNum + lcStrOrd,(lcTmpAllSz))
    *B607002,1 ABD - [End]
    SELECT (lcTmpAllSz)
    REPLACE cReject WITH 'Y'
  ENDDO
ENDIF

SET ORDER TO TAG (lcCurTag) IN (lcTmpAllSz)
IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpAllSz))
  GOTO lnRecNo IN (lcTmpAllSz)
ENDIF

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of lpScndRjct

*!*************************************************************************
*! Name      : lfvPrnt
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Print the Log Report.
*!*************************************************************************
*! Example   : =lfvPrnt()
*!*************************************************************************
*
FUNCTION lfvPrnt
lcOGPlatForm = 'DOS'
lcRepMode='DOS'
IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO &lcTmpLogRp..mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF
*--End of lfvPrnt.

*!*************************************************************************
*! Name      : lfGetOrder
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Fill Temp Invoice Header & Line from Orders.
*!*************************************************************************
*! Example   : =lfGetOrder()
*!*************************************************************************
*
FUNCTION lfGetOrder
*PARAMETERS lcOrderNo,lcStoreNo,llPicked
PRIVATE lcSadInv , ldAllDates , lcImportPO , lnQty1 , lnQty2 , lnQty3 , ;
        lnQty4 , lnQty5 , lnQty6 , lnQty7 , lnQty8 , lnTotQty
ldAllDates = {}          
lcSadInv = ''
=SEEK('O' + &lcTmpOrder..Order,'OrdHdr')

*sameh
STORE &lcTmpOrder..ShipDate TO ldDefInvDate,ldDefPstDate

PRIVATE ldDueDate,lcStore,lcSaleRep1,lcSaleRep2,llPicked,;
        lcCurrCode,lnExRate,lnCurrUnit,llPacked,lnTaxQty,lnTaxRate,llBackOrd
llPicked = .F.
PRIVATE lnAllCartn
lnAllCartn = 0

*-- get the Term code related fields               
=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')

lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                GOMONTH(CTOD(SUBSTR(DTOC(ldDefInvDate),1,3)+'10'+;
                SUBSTR(DTOC(ldDefInvDate),6,5)),IIF(DAY(ldDefInvDate) > lnEOMDay,2,1))+lnTDaysDue)

lcCurrCode = IIF(EMPTY(OrdHdr.cCurrCode),gcBaseCurr,OrdHdr.cCurrCode)
STORE 1 TO lnExRate, lnCurrUnit
IF lcCurrCode <> gcBaseCurr
  lnExRate=gfChkRate('lnCurrUnit',lcCurrCode,ldDefInvDate,.T.,.F.,.F.,llEditExRt)
  IF lnExRate = 0
    IF llEditExRt
      *-- Message : 00262
      *-- A valid xxx to xxx exchange rate could not be found on xxx.
      *-- Button : 00000
      *-- Ok
      =gfModalGen('INM00262B00000','ALERT',ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldDefInvDate))
    ELSE
      lcCurrCode = gcBaseCurr
      STORE 1 TO lnExRate, lnCurrUnit
    ENDIF
  ENDIF
ENDIF
=SEEK('M'+OrdHdr.Account,'Customer')
IF EMPTY(OrdHdr.Store)  
  lcPhone = Customer.Phone1
ENDIF  

llBackOrd = INLIST(IIF(EMPTY(Customer.cBackOrd),laSetups[23,2],Customer.cBackOrd),'A','I')
SELECT OrdLine
SET ORDER TO TAG Ordlinst
=SEEK('O'+OrdHdr.Order)
DO WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+OrdHdr.Order
  lcStore = Store
  =IIF(EMPTY(Store),SEEK('M'+Account,'Customer'),SEEK('S'+Account+Store,'Customer'))
  lcSaleRep1 = Ordhdr.Rep1
  lnComm1    = OrdHdr.Comm1
  lcSaleRep2 = Ordhdr.Rep2
  lnComm2    = OrdHdr.Comm2
  IF OrdHdr.Multi='Y' AND EMPTY(OrdHdr.Rep1) AND EMPTY(OrdHDr.Rep2)
    lcSaleRep1 = Customer.SalesRep
    lnComm1    = Customer.Comm
    lcSaleRep2 = Customer.Rep2
    lnComm2    = Customer.Comm2
  ENDIF
  
  SCAN REST FOR TotQty > 0 ;
            WHILE cOrdType+Order+Store+Style+STR(LineNo,6)='O'+OrdHdr.Order+lcStore
    
    *sameh
    IF !SEEK('N' + &lcTmpOrder..Invoice + Order + Store + PADR(Style,lnStyleLen) + SUBSTR(Style,lnClrPos,lnClrLen),(lcTmpAllSz))
      LOOP
    ENDIF
    
    llPacked = .F.
    IF 'AL' $ gcCmpModules AND ;
      SEEK(OrdLine.Order+OrdLine.Store+OrdLine.PikTkt,'Pack_Hdr') AND ;
      SEEK(PACK_HDR.Pack_No,'Pack_Lin')
      
      IF !SEEK(OrdLine.PikTkt+OrdLine.Order,lcPackLine)
        =lfPackLin(OrdLine.Order,OrdLine.PikTkt)
      ENDIF
      llPacked = SEEK(OrdLine.PikTkt+OrdLine.Order+STR(OrdLine.LineNo,6),lcPackLine)

      SELECT OrdLine
    ENDIF
    SCATTER MEMVAR MEMO
    
    *sameh
    *B607471,1 ABD - Get the Min Qty from the converted ASCI file or the Order Line file. [Begin]
    *lnQty1   = &lcTmpAllSz..Qty1
    *lnQty2   = &lcTmpAllSz..Qty2
    *lnQty3   = &lcTmpAllSz..Qty3
    *lnQty4   = &lcTmpAllSz..Qty4
    *lnQty5   = &lcTmpAllSz..Qty5
    *lnQty6   = &lcTmpAllSz..Qty6
    *lnQty7   = &lcTmpAllSz..Qty7
    *lnQty8   = &lcTmpAllSz..Qty8
    *lnTotQty = &lcTmpAllSz..TotQty

    lnQty1   = MAX(MIN(&lcTmpAllSz..Qty1,M.Qty1),0)
    lnQty2   = MAX(MIN(&lcTmpAllSz..Qty2,M.Qty2),0)
    lnQty3   = MAX(MIN(&lcTmpAllSz..Qty3,M.Qty3),0)
    lnQty4   = MAX(MIN(&lcTmpAllSz..Qty4,M.Qty4),0)
    lnQty5   = MAX(MIN(&lcTmpAllSz..Qty5,M.Qty5),0)
    lnQty6   = MAX(MIN(&lcTmpAllSz..Qty6,M.Qty6),0)
    lnQty7   = MAX(MIN(&lcTmpAllSz..Qty7,M.Qty7),0)
    lnQty8   = MAX(MIN(&lcTmpAllSz..Qty8,M.Qty8),0)
    lnTotQty = MAX(MIN(&lcTmpAllSz..TotQty,M.TotQty),0)
    
    *B607471,1 ABD - [End]
    
    lnPrice  = &lcTmpAllSz..Price
    
    m.Gros_Price = IIF(m.Gros_Price=0,lnPrice,m.Gros_Price)
    =SEEK(m.Style,'Style')
    STORE 0 TO lnTaxQty,lnTaxRate
    m.lTaxable = Style.lTaxable

    IF llPacked
      m.Pik1 = &lcPackLine..Qty1
      m.Pik2 = &lcPackLine..Qty2
      m.Pik3 = &lcPackLine..Qty3
      m.Pik4 = &lcPackLine..Qty4
      m.Pik5 = &lcPackLine..Qty5
      m.Pik6 = &lcPackLine..Qty6
      m.Pik7 = &lcPackLine..Qty7
      m.Pik8 = &lcPackLine..Qty8
      m.TotPik = &lcPackLine..TotQty

      IF !EMPTY(m.Prepak)  
        =SEEK('P'+m.Scale+m.Prepak,'Scale')
        IF Scale.pp1/Scale.PPTot*m.TotPik <> m.Pik1 OR ;
           Scale.pp2/Scale.PPTot*m.TotPik <> m.Pik2 OR ;
           Scale.pp3/Scale.PPTot*m.TotPik <> m.Pik3 OR ;
           Scale.pp4/Scale.PPTot*m.TotPik <> m.Pik4 OR ;
           Scale.pp5/Scale.PPTot*m.TotPik <> m.Pik5 OR ;
           Scale.pp6/Scale.PPTot*m.TotPik <> m.Pik6 OR ;
           Scale.pp7/Scale.PPTot*m.TotPik <> m.Pik7 OR ;
           Scale.pp8/Scale.PPTot*m.TotPik <> m.Pik8
          m.Prepak = ''
          m.PPQty  = 0
        ELSE
          m.PPQty = IIF(Scale.PPTot=0,0,m.TotPik /Scale.PPTot)
        ENDIF
        =SEEK('S'+m.Scale,'Scale')
      ENDIF
    ENDIF
    
    lcSadInv = &lcTmpOrder..Invoice
    INSERT INTO (lcInvLine);
    (Order,Account,LineNo,Store,PikTkt,Style,Dyelot,Note_Mem,Comm1,Comm2,Book1,Book2,;
     Book3,Book4,Book5,Book6,Book7,Book8,TotBook,Flag,Pik1,Pik2,Pik3,Pik4,Pik5,;
     Pik6,Pik7,Pik8,TotPik,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
     Price,Pack_Id,Gros_Price,Disc_Pcnt,lPacked,lBackOrd,nTaxRate,Group,Prepak,;
     Desc1,Season,PPQty,Scale,cWareCode,Consol,cDivision,cCurrCode,lTaxable,cDyeFlag,cwarecode,cSadInv);
     VALUES (m.Order,m.Account,m.LineNo,m.Store,m.PikTkt,m.Style,m.Dyelot,;
     m.Note_Mem,m.Comm1,m.Comm2,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
     m.Qty7,m.Qty8,m.TotQty,IIF(llPicked,'B',' '),m.Pik1,m.Pik2,m.Pik3,m.Pik4,;
     m.Pik5,m.Pik6,m.Pik7,m.Pik8,m.TotPik,IIF(llPicked,m.Pik1,lnQty1),;
     IIF(llPicked,m.Pik2,lnQty2),IIF(llPicked,m.Pik3,lnQty3),IIF(llPicked,m.Pik4,lnQty4),;
     IIF(llPicked,m.Pik5,lnQty5),IIF(llPicked,m.Pik6,lnQty6),IIF(llPicked,m.Pik7,lnQty7),;
     IIF(llPicked,m.Pik8,lnQty8),IIF(llPicked,m.TotPik,lnTotQty),lnPrice,m.Pack_Id,;
     m.Gros_Price,m.Disc_Pcnt,llPacked,llBackOrd,lnTaxRate,m.Group,m.Prepak,;
     m.Desc1,m.Season,m.PPQty,m.Scale,m.cWareCode,'N',OrdHdr.cDivision,OrdHdr.cCurrCode,m.lTaxable,Style.cDye_Flg,m.cwarecode,lcSadInv)

    SELECT (lcInvLine)
    IF INLIST(laSetups[7,2],'F','L') AND SEEK(m.Style+m.cWareCode+m.Dyelot,'StyDye')
      REPLACE Qty1 WITH MAX(MIN(Qty1,StyDye.Stk1),0) ,;
              Qty2 WITH MAX(MIN(Qty2,StyDye.Stk2),0) ,;
              Qty3 WITH MAX(MIN(Qty3,StyDye.Stk3),0) ,;
              Qty4 WITH MAX(MIN(Qty4,StyDye.Stk4),0) ,;
              Qty5 WITH MAX(MIN(Qty5,StyDye.Stk5),0) ,;
              Qty6 WITH MAX(MIN(Qty6,StyDye.Stk6),0) ,;
              Qty7 WITH MAX(MIN(Qty7,StyDye.Stk7),0) ,;
              Qty8 WITH MAX(MIN(Qty8,StyDye.Stk8),0) ,;
              TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
    ENDIF

    SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    SELECT (lcInvHdr)
    llInvPck = !EMPTY(m.PikTkt) AND SEEK(m.Order+m.Store+m.PikTkt,'Pack_Hdr')
    
    *sameh
    *IF !SEEK(m.Account+m.Order+m.Store+m.PikTkt)
    lcImportPO = &lcTmpAllSz..CustPo
    lcCustPack = &lcTmpAllSz..cCustPack

    IF !SEEK(m.Account+m.Order+lcSadInv+m.Store+m.PikTkt)
      APPEND BLANK
      *REPLACE cSelect    WITH '»' ,;
      *        Order      WITH m.Order ,;
      *        Store      WITH m.Store ,;
      *        PikTkt     WITH m.PikTkt ,;
      *        CustPo     WITH IIF(EMPTY(m.CustPo),OrdHdr.CustPo,m.CustPo),;
      *        Dist_Ctr   WITH Customer.Dist_Ctr ,;
      *        Account    WITH OrdHdr.Account ,;
      *        cTermCode  WITH OrdHdr.cTermCode  ,;
      *        SpcInst    WITH OrdHdr.SpcInst ,;
      *        lUpsIns    WITH (OrdHdr.CINSUR='Y') ,;
      *        Rep1       WITH lcSaleRep1 ,;
      *        Comm1      WITH lnComm1    ,;
      *        Rep2       WITH lcSaleRep2 ,;
      *        Comm2      WITH lnComm2    ,;
      *        Note1      WITH OrdHdr.Note1 ,;
      *        Note2      WITH OrdHdr.Note2 ,;
      *        lCompUps   WITH .T. ,;
      *        Consol     WITH 'N'
      REPLACE cSelect    WITH '' ,;
              Order      WITH m.Order ,;
              Store      WITH m.Store ,;
              PikTkt     WITH m.PikTkt ,;
              CustPo     WITH IIF(EMPTY(m.CustPo),OrdHdr.CustPo,m.CustPo),;
              Dist_Ctr   WITH Customer.Dist_Ctr ,;
              Account    WITH OrdHdr.Account ,;
              cTermCode  WITH OrdHdr.cTermCode  ,;
              SpcInst    WITH OrdHdr.SpcInst ,;
              lUpsIns    WITH (OrdHdr.CINSUR='Y') ,;
              Rep1       WITH lcSaleRep1 ,;
              Comm1      WITH lnComm1    ,;
              Rep2       WITH lcSaleRep2 ,;
              Comm2      WITH lnComm2    ,;
              Note1      WITH OrdHdr.Note1 ,;
              Note2      WITH OrdHdr.Note2 ,;
              lCompUps   WITH .T. ,;
              Consol     WITH 'N' ,;
              cSadInv    WITH lcSadInv ,;
              cImportPO  WITH lcImportPO ,;
              cCustPack  WITH lcCustPack
                    
      REPLACE LastLine   WITH OrdHdr.LastLine   

      IF llInvPck AND !EMPTY(Pack_Hdr.Ship_Date)
        ldDueDate = IIF(lcTEOM <> 'Y',Pack_Hdr.Ship_Date+lnTDaysDue,;
                        GOMONTH(CTOD(SUBSTR(DTOC(Pack_Hdr.Ship_Date),1,3)+'10'+;
                        SUBSTR(DTOC(Pack_Hdr.Ship_Date),6,5)),IIF(DAY(Pack_Hdr.Ship_Date) > lnEOMDay,2,1))+lnTDaysDue)
      ENDIF
      ldAllDates = &lcTmpOrder..ShipDate
      REPLACE DiscPcnt   WITH OrdHdr.Disc                                       ,;
              InvDate    WITH ldAllDates                                        ,;
              ShipDate   WITH ldAllDates                                        ,;
              dPostDate  WITH ldAllDates                                        ,;
              DueDate    WITH ldDueDate                                         ,;
              Dept       WITH OrdHdr.Dept                                       ,; 
              cFacCode   WITH OrdHdr.cFacCode                                   ,;
              Approval   WITH OrdHdr.Approval                                   ,;
              Appramt    WITH OrdHdr.ApprAmt                                    ,;
              Season     WITH OrdHdr.Season                                     ,;
              cDivision  WITH OrdHdr.cDivision                                  ,;
              UpsZone    WITH Customer.UpsZone                                  ,;
              Phone      WITH IIF(EMPTY(Ordhdr.Store),lcPhone,Customer.Phone1)  ,;
              cWareCode  WITH IIF(llPicked .AND. SEEK(m.Order+m.PikTkt,'PikTkt'),;
                                  PikTkt.cWareCode,OrdHdr.cWareCode)            ,;
              Trde_Disc  WITH lnTerDiscR                                        ,;
              Tax_Rate   WITH IIF(laSetups[9,2] <> 'Y',0,                        ;
                              IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) ,;
              nPstRate   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                               Customer.nTaxRate,0) ,;
              cTaxRule   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                              Customer.cTaxRule,'') ,;
              Cod_Flag   WITH IIF(lcTCod='Y','Y','N'),;
              Status     WITH OrdHdr.Status ,;
              cCurrCode  WITH lcCurrCode ,;
              nExRate    WITH lnExRate   ,;
              nCurrUnit  WITH lnCurrUnit ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()     ,;
              cAdd_User  WITH gcUser_id

      REPLACE nHstRate   WITH IIF(laSetups[9,2] <> 'Y' OR !llIsCanada,0,laSetups[26,2])
      IF llInvPck
        REPLACE Weight    WITH Pack_Hdr.Tot_Wght ,;
                nCartons  WITH Pack_Hdr.Tot_Cart ,;
                Cartons   WITH Pack_Hdr.Tot_Cart
        REPLACE BOL_No WITH     Pack_Hdr.Bill_ladg    
      ENDIF
    ENDIF
    
    *sameh
    *lnCartons  = nCartons + IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
    lnCartons  = nCartons + IIF(Style.Qty_Ctn>0,lnTotQty/Style.Qty_Ctn,0)

    lnAllCartn = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
    
    *sameh 
    REPLACE Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
            Ship      WITH Ship     + lnTotQty ,;
            ShipAmt   WITH ShipAmt  + lnTotQty*lnPrice ,;
            Discount  WITH -ShipAmt * DiscPcnt/100,;
            Weight    WITH Weight   + IIF(llInvPck,0,lnTotQty*Style.nStyWeight) ,;
            nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
            Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
            Picked    WITH Picked  + m.TotPik ,;
            ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                           IIF(llInvPck,IIF(ALLTRIM(Pack_Hdr.ShipVia)='*',Customer.ShipVia,Pack_Hdr.ShipVia),IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia))),;
            nMerchTax WITH nMerchTax + lnTaxQty * lnPrice * lnTaxRate/100 ,;
            Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
            Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
            TotalChg  WITH IIF(llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,;
            nTaxDue   WITH nTaxDue + IIF(m.lTaxable,lnTotQty*lnPrice,0)
            
            *B606941,1 (Start)
            REPLACE TotalChg  WITH IIF(llIsEngland,ShipAmt+Tax_Amt+Discount,ShipAmt+Freight+Insur+COD+Discount+Tax_Amt) 
            *B606941,1 (End)

  ENDSCAN
ENDDO
RETURN
*-- End of lfGetOrder.

*!*************************************************************************
*! Name      : lfPackLin
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Compute order packed quantity.
*!*************************************************************************
*! Example   : =lfPackLin()
*!*************************************************************************
*
FUNCTION lfPackLin
PARAMETERS lcOrder,lcPackNO
*WAIT 'Computing packed quantity...' WINDOW NOWAIT
SELECT PACK_LIN
=SEEK(lcPackNO)
SCAN REST WHILE pack_no+STR(line_no,3)+style+cpackcolor = lcPackNO
  IF !SEEK(lcPackNO+lcOrder+STR(nordlineno,6),lcPackLine)
    INSERT INTO (lcPackLine) (Order,Pack_No,nOrdLineNo) VALUES ;
    (lcOrder,lcPackNO,Pack_Lin.nordlineno)
  ENDIF
  SELECT (lcPackLine)
  REPLACE Qty1 WITH Qty1 + Pack_Lin.Qty1 ,;
          Qty2 WITH Qty2 + Pack_Lin.Qty2 ,;
          Qty3 WITH Qty3 + Pack_Lin.Qty3 ,;
          Qty4 WITH Qty4 + Pack_Lin.Qty4 ,;
          Qty5 WITH Qty5 + Pack_Lin.Qty5 ,;
          Qty6 WITH Qty6 + Pack_Lin.Qty6 ,;
          Qty7 WITH Qty7 + Pack_Lin.Qty7 ,;
          Qty8 WITH Qty8 + Pack_Lin.Qty8 ,;
          TotQty WITH TotQty + Pack_Lin.TotQty
ENDSCAN
WAIT CLEAR
*-- End of lfPackLin.

*!*************************************************************************
*! Name      : lpCreatFls
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Create all the Temp files used in Invoice Sales Order screen.
*!*************************************************************************
*! Example   : DO lpCreatFls
*!*************************************************************************
*
PROCEDURE lpCreatFls
PRIVATE lnFileStru
lcInvHdr  = gfTempName()
lcInvLine = gfTempName()
lcConsInvH= gfTempName()
lcConsInvD= gfTempName()
lcUpsBox  = gfTempName()
lcPackHdr = gfTempName()
lcInstHdr = gfTempName()
lcInstLin = gfTempName()
lcAppCrdt = gfTempName()
  
lcSadInvH = gfTempName()  
lcSadInvL = gfTempName()
lcSadUpsB = gfTempName()
lcSadInsH = gfTempName()
lcSadInsL = gfTempName()
lcSadOrdC = gfTempName()
  
lcPackLine= gfTempName()
lcOrdCanLn= gfTempName()
lcCrtTmp  = gfTempName()
lcUpsTmp  = gfTempName()

SELECT InvHdr
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+18,4]
laFileStru[lnFileStru+1,1] = 'cSelect'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 1
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'Dist_Ctr'
laFileStru[lnFileStru+2,2] = 'C'
laFileStru[lnFileStru+2,3] = 8
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'Picked'
laFileStru[lnFileStru+3,2] = 'N'
laFileStru[lnFileStru+3,3] = 7
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'lUpsIns'
laFileStru[lnFileStru+4,2] = 'L'
laFileStru[lnFileStru+4,3] = 1
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'nSteps'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 6
laFileStru[lnFileStru+5,4] = 0
laFileStru[lnFileStru+6,1] = 'nChrgTax'
laFileStru[lnFileStru+6,2] = 'N'
laFileStru[lnFileStru+6,3] = 13
laFileStru[lnFileStru+6,4] = 2
laFileStru[lnFileStru+7,1] = 'nMerchTax'
laFileStru[lnFileStru+7,2] = 'N'
laFileStru[lnFileStru+7,3] = 13
laFileStru[lnFileStru+7,4] = 4
laFileStru[lnFileStru+8,1] = 'lCompUps'
laFileStru[lnFileStru+8,2] = 'L'
laFileStru[lnFileStru+8,3] = 1
laFileStru[lnFileStru+8,4] = 0
laFileStru[lnFileStru+9,1] = 'LastLine'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 6
laFileStru[lnFileStru+9,4] = 0
laFileStru[lnFileStru+10,1] = 'LKEYOFF'
laFileStru[lnFileStru+10,2] = 'L'
laFileStru[lnFileStru+10,3] = 0
laFileStru[lnFileStru+10,4] = 0
laFileStru[lnFileStru+11,1] = 'NTAXDUE'
laFileStru[lnFileStru+11,2] = 'N'
laFileStru[lnFileStru+11,3] = 17
laFileStru[lnFileStru+11,4] = 6
laFileStru[lnFileStru+12,1] = 'NCARTONS'
laFileStru[lnFileStru+12,2] = 'N'
laFileStru[lnFileStru+12,3] = 11
laFileStru[lnFileStru+12,4] = 5
laFileStru[lnFileStru+13,1] = 'Ordered'
laFileStru[lnFileStru+13,2] = 'N'
laFileStru[lnFileStru+13,3] = 7
laFileStru[lnFileStru+13,4] = 0
laFileStru[lnFileStru+14,1] = 'cConStore'
laFileStru[lnFileStru+14,2] = 'C'
laFileStru[lnFileStru+14,3] = 8
laFileStru[lnFileStru+14,4] = 0

laFileStru[lnFileStru+15,1] = 'cSadInv'
laFileStru[lnFileStru+15,2] = 'C'
laFileStru[lnFileStru+15,3] = 6
laFileStru[lnFileStru+15,4] = 0
laFileStru[lnFileStru+16,1] = 'NPICKED'
laFileStru[lnFileStru+16,2] = 'N'
laFileStru[lnFileStru+16,3] = 7
laFileStru[lnFileStru+16,4] = 0

laFileStru[lnFileStru+17,1] = 'CIMPORTPO'
laFileStru[lnFileStru+17,2] = 'C'
laFileStru[lnFileStru+17,3] = 15
laFileStru[lnFileStru+17,4] = 0

laFileStru[lnFileStru+18,1] = 'CHOICE'
laFileStru[lnFileStru+18,2] = 'C'
laFileStru[lnFileStru+18,3] = 1
laFileStru[lnFileStru+18,4] = 0

DECLARE laIndex[3,2]
*laIndex[1,1] = 'Account+Order+Store+PikTkt+CDIVISION'
laIndex[1,1] = 'Account+Order+cSadInv+Store+PikTkt+CDIVISION'
laIndex[1,2] = lcInvHdr
laIndex[2,1] = 'cSelect+Account+Order+Store+PikTkt+CDIVISION'
laIndex[2,2] = 'Select'
*laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+CDIVISION'
laIndex[3,1] = 'Consol+Account+cSadInv+cDivision+cCurrCode+CDIVISION'
laIndex[3,2] = 'Consol'

=gfCrtTmp(lcInvHdr,@laFileStru,@laIndex)
=gfCrtTmp(lcConsInvH,@laFileStru,[Consol+Account+cSadInv+cDivision+cCurrCode],lcConsInvH)
SELECT (lcInvHdr)
SET ORDER TO TAG (lcInvHdr)

SELECT OrdLine
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+14,4]
laFileStru[lnFileStru+1,1] = 'LNEWLINE'
laFileStru[lnFileStru+1,2] = 'L'
laFileStru[lnFileStru+1,3] = 0
laFileStru[lnFileStru+1,4] = 0
laFileStru[lnFileStru+2,1] = 'LPACKED'
laFileStru[lnFileStru+2,2] = 'L'
laFileStru[lnFileStru+2,3] = 0
laFileStru[lnFileStru+2,4] = 0
laFileStru[lnFileStru+3,1] = 'LBACKORD'
laFileStru[lnFileStru+3,2] = 'L'
laFileStru[lnFileStru+3,3] = 0
laFileStru[lnFileStru+3,4] = 0
laFileStru[lnFileStru+4,1] = 'nSteps'
laFileStru[lnFileStru+4,2] = 'N'
laFileStru[lnFileStru+4,3] = 2
laFileStru[lnFileStru+4,4] = 0
laFileStru[lnFileStru+5,1] = 'nTaxRate'
laFileStru[lnFileStru+5,2] = 'N'
laFileStru[lnFileStru+5,3] = 10
laFileStru[lnFileStru+5,4] = 2
laFileStru[lnFileStru+6,1] = 'cCurrCode'
laFileStru[lnFileStru+6,2] = 'C'
laFileStru[lnFileStru+6,3] = 3
laFileStru[lnFileStru+6,4] = 0
laFileStru[lnFileStru+7,1] = 'cDivision'
laFileStru[lnFileStru+7,2] = 'C'
laFileStru[lnFileStru+7,3] = 6
laFileStru[lnFileStru+7,4] = 0
laFileStru[lnFileStru+8,1] = 'Consol'
laFileStru[lnFileStru+8,2] = 'C'
laFileStru[lnFileStru+8,3] = 1
laFileStru[lnFileStru+8,4] = 0
laFileStru[lnFileStru+9,1] = 'nNetAmnt'
laFileStru[lnFileStru+9,2] = 'N'
laFileStru[lnFileStru+9,3] = 18
laFileStru[lnFileStru+9,4] = 10
laFileStru[lnFileStru+10,1] = 'nGrosAmnt'
laFileStru[lnFileStru+10,2] = 'N'
laFileStru[lnFileStru+10,3] = 18
laFileStru[lnFileStru+10,4] = 10
laFileStru[lnFileStru+11,1] = 'LTAXABLE'
laFileStru[lnFileStru+11,2] = 'L'
laFileStru[lnFileStru+11,3] = 0
laFileStru[lnFileStru+11,4] = 0
laFileStru[lnFileStru+12,1] = 'cDyeFlag'
laFileStru[lnFileStru+12,2] = 'C'
laFileStru[lnFileStru+12,3] = 1
laFileStru[lnFileStru+12,4] = 0
laFileStru[lnFileStru+13,1] = 'cConStore'
laFileStru[lnFileStru+13,2] = 'C'
laFileStru[lnFileStru+13,3] = 8
laFileStru[lnFileStru+13,4] = 0

laFileStru[lnFileStru+14,1] = 'cSadInv'
laFileStru[lnFileStru+14,2] = 'C'
laFileStru[lnFileStru+14,3] = 6
laFileStru[lnFileStru+14,4] = 0

DECLARE laIndex[4,2]
laIndex[1,1] = 'Account+Order+cSadInv+Store+PikTkt+STR(LineNo,6)'
laIndex[1,2] = lcInvLine
laIndex[2,1] = 'Account+Order+Store+PikTkt+Style'
laIndex[2,2] = 'Styles'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+Style'
laIndex[3,2] = 'Consol'
laIndex[4,1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
laIndex[4,2] = 'Dyelot'
=gfCrtTmp(lcInvLine,@laFileStru,@laIndex)

*=gfCrtTmp(lcConsInvD,@laFileStru,[Consol+Account+cDivision+cCurrCode+cWareCode+Style],lcConsInvD)
=gfCrtTmp(lcConsInvD,@laFileStru,[Consol+Account+cSadInv+cDivision+cCurrCode+cWareCode+Style],lcConsInvD)

SELECT (lcInvLine)
SET ORDER TO TAG (lcInvLine)

IF laSetups[15,2] = 'Y'
  =gfOpenFile(gcDataDir+'UPSBOX',gcDataDir+'UPSBOX','SH')
  =AFIELDS(laFileStru)
  =gfCloseFile('UPSBOX')
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+2,4]
  laFileStru[lnFileStru+1,1] = 'Order'
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  laFileStru[lnFileStru+2,1] = 'PikTkt'
  laFileStru[lnFileStru+2,2] = 'C'
  laFileStru[lnFileStru+2,3] = 6
  laFileStru[lnFileStru+2,4] = 0
  =gfCrtTmp(lcUpsBox,@laFileStru,[Order+Store+PikTkt+STR(CARTONS,5)],lcUpsBox)
ENDIF
=gfCrtTmp(lcInstHdr,[(Order C(6),Store C(8),PikTkt C(6),;
          cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
          nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
          [Order+Store+PikTkt],lcInstHdr)
=gfCrtTmp(lcInstLin,[(Order C(6),Store C(8),PikTkt C(6),;
            cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
            [Order+Store+PikTkt+cInstalNo],lcInstLin)
=gfCrtTmp(lcOrdCanLn,[(cOrdType C(1),Order C(6),LineNo N(6),Qty1 N(6),;
            Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
            Qty8 N(6),TotQty N(7),Cancelled D,cCancReson C(6),Price N(12,2))],;
            [CORDTYPE+ORDER+STR(LINENO,6)],lcOrdCanLn)
=gfCrtTmp(lcPackLine,[(Pack_No C(6),Order C(6),nOrdLineNo N(6),Qty1 N(6),;
            Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
            Qty8 N(6),TotQty N(7))],;
            [PACK_NO+ORDER+STR(nOrdLineNo,6)],lcPackLine)

*-- This code is taken from Sadimara main program lfCRTMP(). [Begin]
SELECT (lcInvHdr)
=AFIELDS(laFileStru)
*lnFileStru = ALEN(laFileStru,1)
*DIMENSION laFileStru[lnFileStru+2,4]
*laFileStru[lnFileStru+1,1] = 'cSadInv'
*laFileStru[lnFileStru+1,2] = 'C'
*laFileStru[lnFileStru+1,3] = 6
*laFileStru[lnFileStru+1,4] = 0
*laFileStru[lnFileStru+2,1] = 'NPICKED'
*laFileStru[lnFileStru+2,2] = 'N'
*laFileStru[lnFileStru+2,3] = 7
*laFileStru[lnFileStru+2,4] = 0

DECLARE laIndex[3,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt'
laIndex[1,2] =  lcSadInvH 
laIndex[2,1] = 'cSelect+Account+Order+Store+PikTkt'
laIndex[2,2] = 'Select'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode'
laIndex[3,2] = 'Consol'
=gfCrtTmp(lcSadInvH,@laFileStru,@laIndex)

SELECT (lcInvLine)
=AFIELDS(laFileStru)
*lnFileStru = ALEN(laFileStru,1)
*DIMENSION laFileStru[lnFileStru+1,4]
*laFileStru[lnFileStru+1,1] = 'cSadInv'
*laFileStru[lnFileStru+1,2] = 'C'
*laFileStru[lnFileStru+1,3] = 6
*laFileStru[lnFileStru+1,4] = 0

DECLARE laIndex[4,2]
laIndex[1,1] = 'Account+Order+Store+PikTkt+STR(LineNo,6)'
laIndex[1,2] = lcSadInvL
laIndex[2,1] = 'Account+Order+Store+PikTkt+Style'
laIndex[2,2] = 'Styles'
laIndex[3,1] = 'Consol+Account+cDivision+cCurrCode+Style'
laIndex[3,2] = 'Consol'
laIndex[4,1] = 'Account+Order+Store+PikTkt+cDyeFlag+Dyelot'
laIndex[4,2] = 'Dyelot'

=gfCrtTmp(lcSadInvL,@laFileStru,@laIndex)

IF USED(lcUpsBox)
  SELECT (lcUpsBox)
  =AFIELDS(laFileStru)
  lnFileStru = ALEN(laFileStru,1)
  DIMENSION laFileStru[lnFileStru+1,4]
  laFileStru[lnFileStru+1,1] = 'cSadInv'  
  laFileStru[lnFileStru+1,2] = 'C'
  laFileStru[lnFileStru+1,3] = 6
  laFileStru[lnFileStru+1,4] = 0
  =gfCrtTmp(lcSadUpsB,@laFileStru,[Order+Store+PikTkt+STR(CARTONS,5)],lcSadUpsB)
ENDIF

IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLin)  
  =gfCrtTmp(lcSadInsH,[(cSadInv C(6) ,Order C(6),Store C(8),PikTkt C(6),;
            cInstmType C(1), nInstmFreq N(3),nInstIAmnt N(11,2),dInstmStDt D,;
            nNoInstm N(3), cInstmRef C(30),nInstIPcnt N(6,2),nInstmAmnt N(10,2))],;
            [Order+Store+PikTkt],lcSadInsH)
  =gfCrtTmp(lcSadInsL,[(cSadInv C(6) ,Order C(6),Store C(8),PikTkt C(6),;
              cInstalNo C(3),nInstmAmnt N(10,2),DueDate D,nInstmPcnt N(6,2),cInstmNote C(30))],;
              [Order+Store+PikTkt+cInstalNo],lcSadInsL)

ENDIF
IF !EMPTY(lcOrdCanLn)
  =gfCrtTmp(lcSadOrdC ,[( cSadInv C(6),cOrdType C(1),Order C(6),LineNo N(6),Qty1 N(6),;
            Qty2 N(6),Qty3 N(6),Qty4 N(6),Qty5 N(6),Qty6 N(6),Qty7 N(6),;
            Qty8 N(6),TotQty N(7),Cancelled D,cCancReson C(6))],;
            [CORDTYPE+ORDER+STR(LINENO,6)],lcSadOrdC )

ENDIF
*-- This code was taken from Sadimara main program lfCRTMP(). [End]

*-- End of lpCreatFls.

*!*************************************************************************
*! Name      : lpShow
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Part of lpShow function from Invoice Sales Order screen.
*!*************************************************************************
*! Example   : =lpShow()
*!*************************************************************************
*
FUNCTION lpShow
PRIVATE llSalesRep , llSycFact
llSalesRep = gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
llSycFact = gfOpenFile(gcSysHome+'SYCFACT',gcSysHome+'CFACCODE','SH')

SELECT (lcInvHdr)
SET ORDER TO TAG CONSOL
LOCATE
DO WHILE !EOF()
  lcAccount = Account
  
  *sameh
  *lcKey = Consol+Account+cDivision+cCurrCode
  lcKey = Consol+Account+cSadInv+cDivision+cCurrCode

  lnInvoices = 0
  lcOrderNo  = Order
  lcTermCode = cTermCode
  lcShipvia = IIF(ordhdr.shipvia='*','*',Shipvia)
  
  *sameh
  *IF SEEK('M'+lcAccount,'Customer') AND Customer.CONSOL='Y'    
  *  SCAN REST WHILE Consol+Account+cDivision+cCurrCode = lcKey
  
  llConsInv = SEEK(&lcInvHdr..cSadInv,(lcTmpOrder)) AND &lcTmpOrder..Consol = 'Y'
  
  IF SEEK(&lcInvHdr..cSadInv,(lcTmpOrder)) AND &lcTmpOrder..Consol = 'Y'
    SCAN REST WHILE Consol+Account+cSadInv+cDivision+cCurrCode = lcKey
      lnInvoices = lnInvoices + 1
      lcOrderNo = IIF(Order=lcOrderNo,lcOrderNo,SPACE(6))
      lcTermCode =IIF(cTermCode=lcTermCode,lcTermCode,SPACE(6))
      lcShipvia = IIF(Shipvia=lcShipVia,lcShipVia,'*')
    ENDSCAN
  ENDIF

  *llConsInv = SEEK('M'+lcAccount,'Customer') AND Customer.CONSOL='Y' AND lnInvoices > 1
  *llConsInv = SEEK('M'+lcAccount,'Customer') AND Customer.CONSOL='Y'

  IF lnInvoices > 1 AND EMPTY(lcTermCode)
  ENDIF
  =SEEK(lcKey)
  
  *sameh
  *SCAN REST WHILE Consol+Account+cDivision+cCurrCode = lcKey FOR lnInvoices > 1
  SCAN REST WHILE Consol+Account+cSadInv+cDivision+cCurrCode = lcKey FOR llConsInv

    SCATTER MEMVAR
    m.ShipVia = lcShipVia
    SELECT (lcConsInvH)
    
    *sameh
    *IF !SEEK('Y'+m.Account+m.cDivision+m.cCurrCode)
    IF !SEEK('Y'+m.Account+m.cSadInv+m.cDivision+m.cCurrCode)

      APPEND BLANK
      REPLACE cSelect   WITH ''         ,;
              Consol    WITH 'Y'         ,;
              Account   WITH m.Account   ,;
              cDivision WITH m.cDivision,;
              cCurrCode WITH m.cCurrCode ,;
              nExRate   WITH m.nExRate   ,;
              nCurrUnit WITH m.nCurrUnit ,;
              Order     WITH lcOrderNo   ,;
              cTermCode WITH m.cTermCode ,;
              SpcInst   WITH m.SpcInst   ,;
              ShipVia   WITH m.Shipvia   ,;
              lUpsIns   WITH m.lUpsIns   ,;
              InvDate   WITH m.InvDate   ,;
              ShipDate  WITH m.ShipDate  ,;
              dPostDate WITH m.dPostDate ,;
              UpsZone   WITH m.UpsZone   ,;
              Phone     WITH m.Phone     ,;
              Tax_Rate  WITH m.Tax_Rate  ,;
              nPstRate  WITH m.nPstRate  ,;
              cTaxRule  WITH m.cTaxRule  ,;
              Status    WITH m.Status    ,;
              Note1     WITH m.Note1     ,;
              Note2     WITH m.Note2     ,;
              Rep1      WITH m.Rep1      ,;
              Comm1     WITH 0           ,;
              Rep2      WITH m.Rep2      ,;
              Comm2     WITH 0           ,;
              Dept      WITH m.Dept      ,; 
              cFacCode  WITH m.cFacCode  ,;
              cWareCode WITH m.cWareCode ,;
              DueDate   WITH m.DueDate   ,;
              Trde_Disc WITH m.Trde_Disc ,;
              Approval  WITH m.Approval  ,;
              Season    WITH m.Season    ,;
              Cod_Flag  WITH m.Cod_Flag  ,;
              lCompUps  WITH .T.         ,;
              dAdd_Date WITH gdSysDate   ,;
              cAdd_Time WITH TIME()      ,;
              cAdd_User WITH gcUser_id   ,;      
              CustPo    WITH m.CustPo          
       REPLACE nHstRate   WITH IIF(laSetups[9,2] <> 'Y' OR !llIsCanada,0,laSetups[26,2])
       
       *sameh
       REPLACE cSadInv WITH m.cSadInv
       
    ELSE  && if the CustPo Changed then blank the CustPo (MultiPo).
      REPLACE CustPo    WITH IIF(m.CustPo <> CustPo,'',m.CustPO)          
    ENDIF
    REPLACE Appramt   WITH Appramt+m.ApprAmt ,;
            Season    WITH IIF(Season=m.Season,Season,'*') ,;
            Ship      WITH Ship    + m.Ship    ,;
            Ordered   WITH Ordered + m.Ship    ,;
            ShipAmt   WITH ShipAmt + m.ShipAmt ,;
            Picked    WITH Picked  + m.Picked  ,;
            Discount  WITH Discount - m.ShipAmt*m.DiscPcnt/100,;
            DiscPcnt  WITH IIF(ShipAmt=0,0,ABS(Discount)*100/ShipAmt)  ,;
            Weight    WITH Weight  + m.Weight  ,;
            Cartons   WITH Cartons + m.Cartons ,;
            nMerchTax WITH nMerchTax + m.nMerchTax ,;
            Tax_Amt   WITH Tax_Amt  + m.Tax_Amt  ,;
            Cod_Amt   WITH Cod_Amt  + m.Cod_Amt  ,;
            TotalChg  WITH TotalChg + m.TotalChg ,;
            nTaxDue   WITH nTaxDue  + m.nTaxDue
    SELECT (lcInvLine)
   
    *sameh
    *=SEEK(m.Account+m.Order+m.Store+m.PikTkt)
    *SCAN REST WHILE Account+Order+Store+PikTkt+STR(LineNo,6) = ;
    *                m.Account+m.Order+m.cSadInv+m.Store+m.PikTkt
    =SEEK(m.Account+m.Order+m.cSadInv+m.Store+m.PikTkt)
    SCAN REST WHILE Account+Order+cSadInv+Store+PikTkt+STR(LineNo,6) = ;
                    m.Account+m.Order+m.cSadInv+m.Store+m.PikTkt

      SCATTER MEMVAR
      SELECT (lcConsInvD)
      
      *sameh
      *IF !SEEK('Y'+Account+cDivision+cCurrCode+m.cWareCode+m.Style)
      
      *B119562,1 Fix wrong of summation. [Begin]
      *IF !SEEK('Y'+Account+cSadInv+cDivision+cCurrCode+m.cWareCode+m.Style)
      IF !SEEK('Y'+m.Account+m.cSadInv+m.cDivision+m.cCurrCode+m.cWareCode+m.Style)
      *B119562,1 Fix wrong of summation. [End]

        APPEND BLANK
        REPLACE Consol    WITH 'Y'         ,;
                Account   WITH m.Account   ,;
                cDivision WITH m.cDivision ,;
                cCurrCode WITH m.cCurrCode ,;
                Style     WITH m.Style     ,;
                Order     WITH lcOrderNo   ,;
                Dyelot    WITH m.Dyelot    ,;
                Season    WITH m.Season    ,;
                Scale     WITH m.Scale     ,;
                cWareCode WITH m.cWareCode ,;
                lTaxable  WITH m.lTaxable
                
        *sameh
        REPLACE cSadInv WITH m.cSadInv
                
      ENDIF
      =RLOCK()
      REPLACE Book1   WITH Book1   + m.Book1 ,;
              Book2   WITH Book2   + m.Book2 ,;
              Book3   WITH Book3   + m.Book3 ,;
              Book4   WITH Book4   + m.Book4 ,;
              Book5   WITH Book5   + m.Book5 ,;
              Book6   WITH Book6   + m.Book6 ,;
              Book7   WITH Book7   + m.Book7 ,;
              Book8   WITH Book8   + m.Book8 ,;
              TotBook WITH TotBook + m.TotBook ,;
              Pik1   WITH Pik1   + m.Pik1 ,;
              Pik2   WITH Pik2   + m.Pik2 ,;
              Pik3   WITH Pik3   + m.Pik3 ,;
              Pik4   WITH Pik4   + m.Pik4 ,;
              Pik5   WITH Pik5   + m.Pik5 ,;
              Pik6   WITH Pik6   + m.Pik6 ,;
              Pik7   WITH Pik7   + m.Pik7 ,;
              Pik8   WITH Pik8   + m.Pik8 ,;
              TotPik WITH TotPik + m.TotPik ,;
              Qty1   WITH Qty1   + m.Qty1 ,;
              Qty2   WITH Qty2   + m.Qty2 ,;
              Qty3   WITH Qty3   + m.Qty3 ,;
              Qty4   WITH Qty4   + m.Qty4 ,;
              Qty5   WITH Qty5   + m.Qty5 ,;
              Qty6   WITH Qty6   + m.Qty6 ,;
              Qty7   WITH Qty7   + m.Qty7 ,;
              Qty8   WITH Qty8   + m.Qty8 ,;
              TotQty WITH TotQty + m.TotQty,;
              nNetAmnt   WITH nNetAmnt +m.TotQty*m.Price ,;
              nGrosAmnt  WITH nGrosAmnt+m.TotQty*m.Gros_Price ,;
              Price      WITH IIF(TotQty=0,0,nNetAmnt/TotQty)  ,;
              Gros_Price WITH IIF(TotQty=0,0,nGrosAmnt/TotQty) ,;
              Disc_Pcnt  WITH IIF(nGrosAmnt=0,0,(nGrosAmnt-nNetAmnt)*100/nGrosAmnt)
      UNLOCK
    ENDSCAN
  ENDSCAN
ENDDO
SELECT (lcConsInvH)
SCAN
  
  *sameh 
  *=SEEK(Consol+Account+cDivision+cCurrCode,lcConsInvD)
  =SEEK(Consol+Account+cSadInv+cDivision+cCurrCode,lcConsInvD)

  SELECT (lcConsInvD)
  lnLineNo = 0
  
  *sameh
  *SCAN REST WHILE Consol+Account+cDivision+cCurrCode+Style = ;
  *                &lcConsInvH..Consol+&lcConsInvH..Account+;
  *                &lcConsInvH..cDivision+&lcConsInvH..cCurrCode
  SCAN REST WHILE Consol+Account+cSadInv+cDivision+cCurrCode+Style = ;
                  &lcConsInvH..Consol+&lcConsInvH..Account+&lcConsInvH..cSadInv+;
                  &lcConsInvH..cDivision+&lcConsInvH..cCurrCode

    SCATTER MEMVAR
    lnLineNo = lnLineNo + 1
    m.LineNo = lnLineNo
    INSERT INTO (lcInvLine) FROM MEMVAR
  ENDSCAN
  SELECT (lcConsInvH)
  SCATTER MEMVAR
  m.LastLine = lnLineNo
  SELECT (lcInvHdr)
  
  *sameh 
  *=SEEK('N'+m.Account+m.cDivision+m.cCurrCode)
  *REPLACE REST FLAG      WITH IIF(EMPTY(m.Order),'A','O') ,;
  *             cTermCode WITH m.cTermCode ,;
  *             Trde_Disc WITH m.Trde_Disc ,; 
  *             Cod_Flag  WITH m.Cod_Flag  ,;
  *             DueDate   WITH m.DueDate   ,;
  *             ShipDate  WITH m.ShipDate  ;
  *WHILE Consol+Account+cDivision+cCurrCode='N'+m.Account+m.cDivision+m.cCurrCode

  =SEEK('N'+m.Account+m.cSadInv+m.cDivision+m.cCurrCode)
  REPLACE REST FLAG      WITH IIF(EMPTY(m.Order),'A','O') ,;
               cTermCode WITH m.cTermCode ,;
               Trde_Disc WITH m.Trde_Disc ,; 
               Cod_Flag  WITH m.Cod_Flag  ,;
               DueDate   WITH m.DueDate   ,;
               ShipDate  WITH m.ShipDate  ;
  WHILE Consol+Account+cSadInv+cDivision+cCurrCode='N'+m.Account+m.cSadInv+m.cDivision+m.cCurrCode
  INSERT INTO (lcInvHdr) FROM MEMVAR
ENDSCAN
IF llSalesRep
  USE IN SalesRep
ENDIF
IF llSycFact
  USE IN SycFact
ENDIF
SET ORDER TO TAG (lcInvHdr) IN (lcInvHdr)
llCUpdate  = .T.
*-- End of lpShow.

*!**************************************************************************
*! Name      : lfSadSav
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 11/28/2002
*! Purpose   : Save to InvHdrS & InvLineS
*!**************************************************************************
FUNCTION lfSadSav
PRIVATE lnInvCount
lnInvCount = 1 
SELECT (lcInvHdr)
SCAN FOR cSelect = '»'
  SCATTER MEMVAR MEMO
  M.NPICKED=&lcInvHdr..Picked
  INSERT INTO (lcSadInvH) FROM MEMVAR
ENDSCAN  
DIMENSION laInv[1]
laInv[1] = ''
SELECT (lcSadInvH) 
SCAN
  DIMENSION laInv[lnInvCount] 

  *-- Get The order number manualy or using gfsequence depend on the invoice setup
  IF EMPTY(&lcSadInvH..CSadInv)
  *-- Get The Invoice number on the factor sequence if the invoice was factored 
*    lcInvNo=IIF(EMPTY(&lcSadInvH..cFacCode),;
*            gfSequence('INVOICE','','',&lcSadInvH..cDivision),;
*            gfSequence('CFINVOICE' ,'','',&lcSadInvH..cDivision)) 
*    laInv[lnInvCount] = lcInvNo
*    lnInvCount = lnInvCount + 1  
  ELSE
    lcInvNo=&lcSadInvH..cSadInv
    laInv[lnInvCount] = lcInvNo
    lnInvCount = lnInvCount + 1
  ENDIF
  
  SELECT (lcSadInvH)
  *-- If the invoice was consolidated invoice give the lines the same invoice number
  *IF Consol='Y'
  *  lcAccount = Account
  *  REPLACE REST cSadInv WITH lcInvNo WHILE Account+Order+Store+PikTkt=lcAccount
  *  =SEEK(lcAccount)
  *ENDIF  
  REPLACE nExRate   WITH IIF(nExRate=0,1,nExRate) ,;
          nCurrUnit WITH IIF(nCurrUnit=0,1,nCurrUnit) ,;
          LastLine  WITH IIF(SEEK('O'+Order,'ORDLINE'),LastLine,0),;
          CSadInv   WITH lcInvNo 
  llConsSad = &lcSadInvH..Direct_Inv OR &lcSadInvH..Consol='Y' OR ;
              EMPTY(&lcSadInvH..Flag)        

  SELECT(lcInvLine)
  
  *sameh
  *=SEEK(&lcSadInvH..Account+&lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt)
  =SEEK(&lcSadInvH..Account+&lcSadInvH..Order+&lcSadInvH..cSadInv+&lcSadInvH..Store+&lcSadInvH..PikTkt)
  
  WAIT 'Updating Invoice Lines...' WINDOW NOWAIT
  
  *sameh
  *SCAN REST WHILE Account+Order+Store+PikTkt = ;
  *            &lcSadInvH..Account+&lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt
  SCAN REST WHILE Account+Order+cSadInv+Store+PikTkt = ;
              &lcSadInvH..Account+&lcSadInvH..Order+&lcSadInvH..cSadInv+&lcSadInvH..Store+&lcSadInvH..PikTkt

    SCATTER MEMVAR MEMO
    m.cSadInv= &lcSadInvH..cSadInv
    INSERT INTO (lcSadInvL) FROM MEMVAR
  ENDSCAN  
     
  IF !EMPTY(lcUpsBox) AND USED(lcUpsBox)
    WAIT 'Updating UPS Box File ' WINDOW NOWAIT
    SELECT (lcUpsBox)
    lcStoreFld = IIF(&lcSadInvH..Consol='Y',&lcSadInvH..cConStore,&lcSadInvH..Store)
    IF SEEK(&lcSadInvH..Order+lcStoreFld+&lcSadInvH..PikTkt)
      SCAN REST WHILE Order+Store+PikTkt+STR(CARTONS,5)=;
                    &lcSadInvH..Order+lcStoreFld+&lcSadInvH..PikTkt
        SCATTER MEMVAR
        m.cSadInv = lcInvNo
        INSERT INTO (lcSadUpsB) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDIF
  
  IF llConsSad
    IF !EMPTY(lcInstHdr) AND !EMPTY(lcInstLin) AND ;
      SEEK(&lcSadInvH..Order + &lcSadInvH..Store + &lcSadInvH..PikTkt,lcInstHdr) .AND. ;
          SEEK(&lcSadInvH..Order + &lcSadInvH..Store + &lcSadInvH..PikTkt,lcInstLin)
        SELECT (lcInstHdr)
        SCATTER MEMVAR MEMO
        m.cSadInv=lcInvNo
        INSERT INTO (lcSadInsH) FROM MEMVAR
        
        SELECT (lcInstLin)
        *-- update all the lines in the instalment detail file and the debit file 
        SCAN REST WHILE Order+Store+PikTkt+cInstalNo =;
                      &lcSadInvH..Order+&lcSadInvH..Store+&lcSadInvH..PikTkt
          SCATTER MEMVAR MEMO
          m.cSadInv=lcInvNo
          INSERT INTO (lcSadInsL) FROM MEMVAR
        ENDSCAN
    ENDIF
  ENDIF  
  
  IF !EMPTY(lcOrdCanLn) AND &lcSadInvH..Consol<>'Y' AND SEEK('O'+&lcSadInvH..ORDER,lcOrdCanLn)
    SELECT (lcOrdCanLn)
    SCAN REST WHILE cOrdType+Order+STR(LineNo,6) = 'O'+&lcSadInvH..ORDER
      *-- Update the order canceld Quantity
      SCATTER MEMVAR
      m.cSadInv=lcInvNo
      INSERT INTO (lcSadOrdC)  FROM MEMVAR
    ENDSCAN
  ENDIF
ENDSCAN

PRIVATE llInvHdrs, llInvLines, llUPSBoxs, llOrdCanSD, llSDINSTMH , llSDINSTMD
llInvHdrs = gfOpenFile(gcDataDir+'INVHDRS',gcDataDir+'INVHDRS','SH')
llInvLines = gfOpenFile(gcDataDir+'INVLINES',gcDataDir+'INVLINES','SH')
llUPSBoxs = gfOpenFile(gcDataDir+'UPSBOXS',gcDataDir+'UPSBOXS','SH')
llOrdCanSD = gfOpenFile(gcDataDir+'ORDCANSD',gcDataDir+'ORDCANSD','SH')
llSDINSTMH = gfOpenFile(gcDataDir+'SDINSTMH',gcDataDir+'SDINSTMH','SH')
llSDINSTMD = gfOpenFile(gcDataDir+'SDINSTMD',gcDataDir+'SDINSTMD','SH')

SELECT (lcSadInvH)
SCAN
  SCATTER MEMVAR MEMO
  SELECT INVHDRS
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcSadInvL)
SCAN
  SCATTER MEMVAR MEMO
  SELECT INVLINES
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

IF USED(lcSadUpsB)
  SELECT (lcSadUpsB)
  SCAN
    SCATTER MEMVAR MEMO
    SELECT UPSBOXS
    APPEND BLANK
    GATHER MEMVAR MEMO
  ENDSCAN
ENDIF

SELECT (lcSadOrdC) 
SCAN
  SCATTER MEMVAR MEMO
  SELECT ORDCANSD
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
  
SELECT(lcInstLin)
SCAN
  SCATTER MEMVAR MEMO
  SELECT SDINSTMD
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcSadInsH)
 SCAN
  SCATTER MEMVAR MEMO
  SELECT SDINSTMH
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

lcSydKey=''
lnInvCount = ALEN(laInv)

DO CASE
  CASE EMPTY(laInv)
    llCSave = .F.
  CASE lnInvCount=1
    =gfModalGen('INM00000B00000','ALERT','','', 'Pre_Billed invoice has been saved as '+laInv[1] )
  OTHERWISE
    =gfModalGen('INM00000B00000','ALERT','','', 'Pre_Billed invoices have been saved.')
ENDCASE

IF llInvHdrs
  USE IN INVHDRS
ENDIF
IF llInvLines
  USE IN InvLines
ENDIF
IF llUPSBoxs
 USE IN UPSBOXS
ENDIF
IF llOrdCanSD
  USE IN OrdCanSD
ENDIF
IF llSDINSTMH
  USE IN SDINSTMH
ENDIF
IF llSDINSTMD
  USE IN SDINSTMD
ENDIF

SELECT (lcInvHdr)
*-- End of lfSadSav.

*!*******************************************************************************
*! Name      : lfBrwAprvd
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 11/28/2002
*! Purpose   : Browse OrdLine file from Previous Orders screen.
*!*******************************************************************************
*! Example   : =lfBrwAprvd()
*!*******************************************************************************
*
FUNCTION lfBrwAprvd
PRIVATE lcAlias
lcAlias = ALIAS()

SELECT (lcInvHdr)
lnMark = RECNO()
*SUM Ship,ShipAmt TO lnTotShp , lnToShpAmt FOR cSelect = '»'
SUM Ship,ShipAmt TO lnTotShp , lnToShpAmt FOR Choice = 'Y' AND cSelect = '»'
IF BETWEEN(lnMark,1,RECCOUNT())
  GOTO lnMark IN (lcInvHdr)
ENDIF
lcCurInvNo = cSadInv
lnApproved = IIF(cSelect = '»',1,2)
SHOW GET lnApproved ENABLE
=gfwCodePop(@laCodes,'CTERMCODE','T')

*-- Brow the invoice header file.
BROWSE FOR Choice = 'Y' ;
       FIELDS cMarker = IIF(RECNO()=lnMark,'>',' '):H=' ':R:1 , ;
       cSelect = IIF(cSelect='»','Yes','No') :H='Accept' :R:8 , ;
       Account :R:9 , cSadInv :H='Invoice#' :R:11 , ;
       Store :H='Store#' :R :10, ;
       lcSpcInst = IIF(SEEK('NSPCINST   '+SpcInst,'Codes'),Codes.cDiscRep,'') :H='Special Instruction' :R:20, ;
       Order :H='Order#' :R :9, cImportPo :H='Imported Cust PO' :R:20, ; 
       CustPO :H='Original Cust PO' :R :20, Ship :H='Ship Units' :R :10, ;
       ShipAmt :H='Shipment' :R :12 ,; 
       lcTermCode = LOOKUP(Codes.cDiscRep,'N'+cTermCode+'NCTERMCODE',Codes.cDiscRep,'Codes') :H='Terms' :R:20 ;
       WHEN lfWBrTmpSz() ;
       VALID :F lfvBrTmpSz();
       NOEDIT            ;
       LOCK 0            ;
       NOMENU            ;
       NOAPPEND          ;
       NODELETE          ;
       NOWAIT            ;
       SAVE              ;
	   NOCLEAR           ;
       WINDOW ARREN30A   ;
       IN WINDOW ARREN30 ;
       TITLE lcBrTtl1
SHOW WINDOW (lcBrTtl1) REFRESH SAME
IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfBrwAprvd.

*!*******************************************************************************
*! Name      : lfWBrTmpSz
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 11/28/2002
*! Purpose   : When for Temp Approved in Imported invoice screen.
*!*******************************************************************************
*! Example   : =lfWBrTmpSz()
*!*******************************************************************************
*
FUNCTION lfWBrTmpSz
PRIVATE lcAlias
lcAlias = ALIAS()

lnMark = RECNO(lcInvHdr)
*SUM Ship,ShipAmt TO lnTotShp , lnToShpAmt FOR cSelect = '»'
SUM Ship,ShipAmt TO lnTotShp , lnToShpAmt FOR Choice = 'Y' AND cSelect = '»'
IF BETWEEN(lnMark,1,RECCOUNT())
  GOTO lnMark IN (lcInvHdr)
ENDIF
lcCurInvNo = &lcInvHdr..cSadInv
lnApproved = IIF(&lcInvHdr..cSelect = '»',1,2)
SHOW GET lnApproved ENABLE
=gfwCodePop(@laCodes,'CTERMCODE','T')
=lfRefresh('ARREN30B')

*-- Flag to know that the active window is the browse.
glFromBrow = .T.
SELECT (lcInvHdr)
SHOW WINDOW (lcBrTtl1) REFRESH SAME  && Refresh the browse.

IF !EMPTY(lcAlias)    && Restore old alias.
  SELECT (lcAlias)
ENDIF          
*-- End of lfWBrTmpSz.

*!*******************************************************************************
*! Name      : lfvBrTmpSz
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 11/28/2002
*! Purpose   : Valid for Temp Approved in Imported invoice screen.
*!*******************************************************************************
*! Example   : =lfvBrTmpSz()
*!*******************************************************************************
*
FUNCTION lfvBrTmpSz

*-- If exist from browse window.
IF !WONTOP(lcBrTtl1)
  *-- Set flag to say I'm coming from a browse.
  glFromBrow = .T.
  *-- Call global function to stop the browse.
  =gfStopBrow()
ENDIF
*-- End of lfvBrTmpSz.

*!*******************************************************************************
*! Name      : lfvProceed
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 11/28/2002
*! Purpose   : Validation for Proceed button in Imported invoice screen.
*!*******************************************************************************
*! Example   : =lfvProceed()
*!*******************************************************************************
*
FUNCTION lfvProceed
llProceed = .T.
CLEAR READ
*-- End of lfvProceed.

*!*******************************************************************************
*! Name      : lfvAccept
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 11/28/2002
*! Purpose   : Validation for Accept Popup in Imported invoice screen.
*!*******************************************************************************
*! Example   : =lfvAccept()
*!*******************************************************************************
*
FUNCTION lfvAccept
PRIVATE lcAlias , lnRecNo , lcInvoice
lcAlias = ALIAS()

SELECT (lcInvHdr)
lnRecNo = RECNO()
lcInvoice = cSadInv

REPLACE cSelect WITH IIF(lnApproved=1,'»','') FOR cSadInv = lcInvoice
SUM Ship,ShipAmt TO lnTotShp , lnToShpAmt FOR Choice = 'Y' AND cSelect = '»'
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo IN (lcInvHdr)
ENDIF
=lfRefresh('ARREN30B')

IF !EMPTY(lcAlias)
  SELECT (lcAlias)
ENDIF
*-- End of lfvAccept.

*!*******************************************************************************
*! Name      : lfvTerms
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 11/28/2002
*! Purpose   : Validation for Terms Popup in Imported invoice screen.
*!*******************************************************************************
*! Example   : =lfvTerms()
*!*******************************************************************************
*
FUNCTION lfvTerms

IF &lcInvHdr..cTermCode <> ALLTRIM(laTerms[lnTerms,2])
  m.cTermCode = ALLTRIM(laTerms[lnTerms,2])
  =lfGetDueDate()
  llCod  = (ALLTRIM(lcTCod)='Y')
  STORE .T. TO llCUpdate
  SELECT (lcInvHdr)
  REPLACE lCompUps  WITH .T.     ,;
          cTermCode WITH m.cTermCode ,;
          Cod_Flag  WITH IIF(llCod,'Y','N') ,;
          Trde_Disc WITH lnTerDiscR
  *IF CONSOL='Y'
  *  SET ORDER TO TAG CONSOL 
  *  =SEEK('N'+laData[4]+laData[13]+laData[14])
  *  REPLACE REST lCompUps  WITH .T. ,;
  *               cTermCode WITH m.cTermCode ,;
  *               Cod_Flag  WITH IIF(llCod,'Y','N') ,;
  *               Trde_Disc WITH lnTerDiscR  ;
  *  WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
  *  =SEEK('Y'+laData[4]+laData[13]+laData[14])
  *  SET ORDER TO TAG (lcInvHdr)
  *ENDIF
  *m.Trde_Disc = Trde_Disc
ENDIF
*-- End of lfvTerms.

*!*******************************************************************************
*! Name      : lfGetDueDate
*! Developer : Sameh Saiid Ezzat (SSE) 
*! Date      : 11/28/2002
*! Purpose   : Calculate the due date based on Terms.
*!*******************************************************************************
*! Example   : =lfGetDueDate()
*!*******************************************************************************
*
FUNCTION lfGetDueDate
PRIVATE ldNewDueDt
=gfRltFld(m.cTermCode,@laTRltFld,'CTERMCODE')
lcTEOM = ALLTRIM(lcTEOM)
lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
ldNewDueDt = IIF(lcTEOM <> 'Y',&lcInvHdr..ShipDate+lnTDaysDue,;
                GOMONTH(CTOD(SUBSTR(DTOC(&lcInvHdr..ShipDate),1,3)+'10'+;
                SUBSTR(DTOC(&lcInvHdr..ShipDate),6,5)),IIF(DAY(&lcInvHdr..ShipDate) > lnEOMDay,2,1))+lnTDaysDue)
SELECT (lcInvHdr)
*=RLOCK()
REPLACE DueDate  WITH ldNewDueDt
*UNLOCK
*IF CONSOL='Y'
*  SET ORDER TO TAG CONSOL 
*  =SEEK('N'+laData[4]+laData[13]+laData[14])
*  REPLACE REST DueDate  WITH m.DueDate ,;
*               ShipDate WITH m.ShipDate ;
*  WHILE Consol+Account+cDivision+cCurrCode = 'N'+laData[4]+laData[13]+laData[14]
*  =SEEK('Y'+laData[4]+laData[13]+laData[14])
*  SET ORDER TO TAG (lcInvHdr)
*ENDIF
*SHOW GET m.DueDate
*-- End of lfGetDueDate.