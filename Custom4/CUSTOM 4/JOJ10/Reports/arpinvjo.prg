*:***************************************************************************
*: Program file  : ARPINVRU
*: Program desc. : Custom Invoice Form For JO JOE (JOJ10)
*: For Report    : ....
*: System        : Aria 4XP
*: Module        : Account Receivable (AR)
*: Developer     : Mariam Mazhar  (MMT)
*: Date          : 06/30/2009
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:MOdifcations :
*:C201178,1 MMT 08/05/2009 Fix bug of repeating lines for same style maj.[T20090416.0007]
*:***************************************************************************
*: This Report Program is due to C201178(Aria4),C201179(Aria27)... [T20090416.0007]
*----------------------- Report Code Begin -----------------------------

lnStyMaj = LEN(gfItemMask('PM','', "0001"))
*:C201178,1 MMT 08/05/2009 Fix bug of repeating lines for same style maj.[Start]
DIMENSION laPrtStyles[1]
laPrtStyles = ''
lnTotStyQty = 0
llStyLine  = .T.
*:C201178,1 MMT 08/05/2009 Fix bug of repeating lines for same style maj.[End]
*!*************************************************************
*! Name      : lfGetLastLine
*! Developer : Mariam Mazhar  (MMT)
*! Date      : 06/30/2009
*! Purpose   : Function to Get last line in Invoice
*!*************************************************************
FUNCTION lfGetLastLine
PRIVATE lcThAlias,lnThRec,lcThStore
*:C201178,1 MMT 08/05/2009 Fix bug of repeating lines for same style maj.[Start]
DIMENSION laPrtStyles[1]
laPrtStyles = ''
lnTotStyQty = 0
llStyLine  = .T.
*:C201178,1 MMT 08/05/2009 Fix bug of repeating lines for same style maj.[End]
lcThAlias = ALIAS()           && Save Current Alias.
SELECT Invline
lnThRec = RECNO()    && Save Current record #.
LNTOTAMT = 0
LNTOTALQTY  = 0
=SEEK(INVHdr.Invoice)
SCAN REST WHILE  INVOICE+STR(LINENO,6)  =  INVHdr.Invoice
  LNLstLn =  lineno
  LNTOTAMT = LNTOTAMT + TotQty * Price
  LNTOTALQTY = LNTOTALQTY + TotQty
ENDSCAN 

IF BETWEEN(lnThRec,1,RECCOUNT('Invline'))
  GO lnThRec IN Invline    && Restore Record #
ELSE
  GO TOP IN Invline    && Restore Record #
ENDIF
SELECT (lcThAlias)            && Restore Alias.
RETURN ''

*:C201178,1 MMT 08/05/2009 Fix bug of repeating lines for same style maj.[Start]
*!*************************************************************
*! Name      : lfGetTOtQty
*! Developer : Mariam Mazhar  (MMT)
*! Date      : 08/05/2009
*! Purpose   : Function to Get Style maj totQty
*!*************************************************************
FUNCTION lfGetTOtQty
lcInvoic  = invline.invoice
lcCurStyleMaj = substr(invline.Style,1,lnStyMaj)
lnPriceCur = invline.Price
IF ASCAN(laPrtStyles,lcCurStyleMaj +STR(lnPriceCur ,13,2)) > 0
  llStyLine  = .F.
  RETURN .F.
ENDIF 
lcAlias = SELECT()
lnCurRec = RECNO('Invline')
SELECT Invline 
=SEEK(lcInvoic)
lnTotStyQty = 0
SCAN REST WHILE INVOICE+STR(LINENO,6) = lcInvoic  FOR Price = lnPriceCur AND Style = lcCurStyleMaj 
  lnTotStyQty = lnTotStyQty + Invline.TotQty
ENDSCAN 
IF BETWEEN(lnCurRec ,1,RECCOUNT('Invline'))
  GO RECORD lnCurRec IN Invline 
ENDIF 
SELECT (lcAlias)
llStyLine  = .T.
IF EMPTY(laPrtStyles[1])
  laPrtStyles[1] = lcCurStyleMaj +STR(lnPriceCur ,13,2)
ELSE
  DIMENSION laPrtStyles[ALEN(laPrtStyles,1)+1]
  laPrtStyles[ALEN(laPrtStyles,1)] = lcCurStyleMaj +STR(lnPriceCur ,13,2)
ENDIF 
RETURN .T.
*:C201178,1 MMT 08/05/2009 Fix bug of repeating lines for same style maj.[End]