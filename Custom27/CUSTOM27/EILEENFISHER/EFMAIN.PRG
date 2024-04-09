*!**************************************************************************
*! Name      : EFMAIN.PRG
*! Developer : BASSEM RAFAAT
*! Date      : 03/20/2000
*! Purpose   : EILEEN FISHER Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process
*B603656,1                for the customized sales j. for (Fresh Perduce).
*C102390,1 TMI 07/30/2001 Change the default value of the dyelot check box to be checked by default 
*!**************************************************************************

PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!*************************************************************
*! Name      : lfGetPrice
*! Developer : BASSEM RAFAAT
*! Date      : 03/20/2000
*! Purpose   : EILEEN FISHER Custom calculate for gross and net amount.
*!           : from GROSS PROFIT REPORT.
*!*************************************************************
*! Called from  : EFMAIN.PRG
*! Program name : ARGROSSP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfGetPrice
*!*************************************************************
*C101712,1 BWA 03/20/2000 A custom process to calculate the gross and the net amount.FUNCTION lfGetPrice
*!*************************************************************

FUNCTION lfGetPrice

*B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process
*B603656,1                for the customized sales j. for (Fresh Perduce).
*lnGross = lnQty*IIF(llCallGfam,gfAmntDisp(Gros_Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Gros_Price)
*lnNet   = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)*(1 - (DiscPcnt/100)) * (1 - (Trde_Disc/100))

PRIVATE lcDiscunta,lcTradeDis,lcTotDisnt
STORE 0 TO lcDiscunta,lcTradeDis,lcTotDisnt

lnGross    = lnQty*IIF(llCallGfam,gfAmntDisp(STYLE.Pricea, lcRpCurr , ldRpExDate , lcRpTmpNam),STYLE.Pricea)
lnMerch    = lnQty*IIF(llCallGfam,gfAmntDisp(Price, lcRpCurr , ldRpExDate , lcRpTmpNam),Price)
lcDiscunta = (lnGross - lnMerch) + lnMerch*InvHdr.Discpcnt/100
lcTradeDis = (lnGross-lcDiscunta)*InvHdr.Trde_Disc/100
lcTotDisnt = lcDiscunta + lcTradeDis
lnNet      = lnGross - lcTotDisnt
*B603656,1 [END]
                  *-- End function lfGetPrice --*

*B603656,1 Make relations to the style and invhdr to get the sales price 
*B603656,1 and the trad discount value.
*!*************************************************************
*! Name      : lfSalesRel
*! Developer : BASSEM RAFAAT
*! Date      : 05/24/2000
*! Purpose   : EILEEN FISHER Custom calculate for gross and net amount.
*!           : for GROSS PROFIT REPORT.
*!*************************************************************
*! Called from  : EFMAIN.PRG
*! Program name : ARGROSSP.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfSalesRel
*!*************************************************************
*B603656,1 BWA 05/25/2000 Modifications on the calculations for the custom process
*B603656,1                for the customized gross profit for (Fresh Perduce).
*!*************************************************************
FUNCTION lfSalesRel

SELECT &INVLTEMP
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO INVOICE INTO INVHDR ADDITIVE
llRetuV = .T.
*B603656,1 [END]
                   *-- End function lfSalesRel --*
                  
*!*************************************************************
*! Name      : lfGetJoPrc
*! Developer : BASSEM RAFAAT
*! Date      : 03/20/2000
*! Purpose   : EILEEN FISHER Custom calculate for gross and net amount.
*!           : from the Sales Journal Report.
*!*************************************************************
*! Called from  : EFMAIN.PRG
*! Program name : ARSJOUR.PRG
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : DO lfGetJoPrc
*!*************************************************************
*C101713,1 A custom process to calculate the shipped amount and the Mrech. Discount.[START]
*!*************************************************************
FUNCTION lfGetJoPrc
PRIVATE lcAlias,lnShipAmt,lnLinesDisc
lcAlias = SELECT(0)

*B603656,1 BWA 05/25/2000 Open style file [START]
IF !USED(lcStyFile)
  = gfOpenFile(gcDataDir+'Style' ,gcDataDir+'Style', 'SH', @lcStyFile, .T.)
ENDIF
*B603656,1 [END]

IF !USED(lcInvLine)
  = gfOpenFile(gcDataDir+'Invline' ,gcDataDir+'Invline', 'SH', @lcInvLine, .T.)
ENDIF
SELECT (lcInvLine)

*B603656,1 BWA 05/25/2000 Set relation into style file [START]
SET RELATION TO STYLE INTO (lcStyFile) ADDITIVE
*B603656,1 [END]

=SEEK(InvHdr.Invoice)

*B603656,1 BWA 05/25/2000 Modifications on the calculations.[START]
*SUM REST TotQty * Gros_Price,TotQty*(Gros_Price-Price) TO m.ShipAmt,lnLinesDisc WHILE invoice+STR(lineno,6) = InvHdr.Invoice
*m.Discount = m.Discount - lnLinesDisc

SUM REST TotQty * &lcStyFile..PriceA ,TotQty*(&lcStyFile..PriceA-Price) TO lnShipAmt,lnLinesDisc ;
            WHILE invoice+STR(lineno,6) = InvHdr.Invoice
IF InvHdr.Status = 'V'
  m.vShipAmt  = lnShipAmt
  m.vDiscount = m.vDiscount - lnLinesDisc
ELSE
  m.ShipAmt = lnShipAmt
  m.Discount = m.Discount - lnLinesDisc
ENDIF
SET RELATION OFF INTO (lcStyFile)
*B603656,1 [END]

SELECT (lcAlias)
                  *-- End function lfGetJoPrc --*
                  
*:**************************************************************************
*:* Name        : lfInitDye
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/30/2001
*:* Purpose     : Initialize Dyelot flag  with 'Y' Only in Add mode for style 
*:*             : not copied from other styles
*:***************************************************************************
*:* Called from : ICSTYLE  // as a trigger
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfInitDye()
*:***************************************************************************
*:* ENTRY       : C102390,1 TMI 
*:***************************************************************************
FUNCTION lfInitDye

IF laScrMode[4] AND EMPTY(lcfrmstyle)
  laData[23] = 'Y'
ENDIF   

*-- end of lfInitDye.

