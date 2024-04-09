*:***************************************************************************
*: Program file  : ARPINVVS
*: Program desc. : Custom Invoice Form For Vision (VIS20)
*: For Report    : ....
*: System        : Aria 4XP
*: Module        : Account Receivable (AR)
*: Developer     : Mariam Mazhar  (MMT)
*: Date          : 06/24/2009
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:MOdifcations :
*:B608992,1 MMT 09/14/2009 Fix bug of page orientation[T20090213.0021]
*:C201595,1 MMT 09/30/2013 Convert Custom Invoice Form and Order Conf. to graphic[T20130809.0003]
*:***************************************************************************
*: This Report Program is due to C201174(Aria4),C201171(Aria27)... [T20090213.0021]
*----------------------- Report Code Begin -----------------------------
*B608992,1 MMT 09/14/2009 Fix bug of page orientation[Start]
*:C201595,1 MMT 09/30/2013 Convert Custom Invoice Form and Order Conf. to graphic[Start]
*loogScroll.cCROrientation = 'L'
loOGScroll.cCROrientation = 'P'
*:C201595,1 MMT 09/30/2013 Convert Custom Invoice Form and Order Conf. to graphic[End]
*B608992,1 MMT 09/14/2009 Fix bug of page orientation[End]
*!*************************************************************
*! Name      : lfGetLastLine
*! Developer : Mariam Mazhar  (MMT)
*! Date      : 06/24/2009
*! Purpose   : Function to Get last line in Invoice
*!*************************************************************
FUNCTION lfGetLastLine
PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.
SELECT Invline
lnThRec = RECNO()    && Save Current record #.
LNTOTAMT = 0
=SEEK(INVHdr.Invoice)
SCAN REST WHILE  INVOICE+STR(LINENO,6)  =  INVHdr.Invoice
  LNLstLn =  lineno
  LNTOTAMT = LNTOTAMT + TotQty * Price
ENDSCAN 

IF BETWEEN(lnThRec,1,RECCOUNT('Invline'))
  GO lnThRec IN Invline    && Restore Record #
ELSE
  GO TOP IN Invline    && Restore Record #
ENDIF
SELECT (lcThAlias)            && Restore Alias.
RETURN ''