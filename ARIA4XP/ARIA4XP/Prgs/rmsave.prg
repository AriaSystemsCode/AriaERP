*!**********************************************************************************************
*! Name      : lfSavRet
*! Developer : Hend Ghanem
*! Date      : 04/28/2005
*! Purpose   : Save credit memo.
*!**********************************************************************************************
*! Calls     : None
*!**********************************************************************************************
*! Passed Parameters : llPontSale -> .T. if from point of sale prg.
*!                   : lcCrMemHdr -> Temp. cerdit memo header.
*!                   : lcCrMemLin -> Temp. credit memo line.
*!**********************************************************************************************
*! Returns           :  None
*!**********************************************************************************************
*! Example           :  =lfSavRet()
*!**********************************************************************************************
*! Modification:
*  B608074,1 05/06/2007 MMT fix bug of not saving CR details[T20061212.0040]
*  B608142,1 MMT 06/26/2007 fix bug of not updating RA status[T20061212.0040]
*  B608484,1 MHM 03/17/2008 Fix bug of not calculating Comm correctlly [T20080305.0009]
*  C200876,1 TMI 05/02/2008 Add the BIn location Triggers
*  B608739,1 WAM 11/06/2008 Calculate total amount based on the amount field in the lines file because
*  B608739,1 WAM 11/06/2008 the Price field is rounded [T20081030.0025]
*  E302590,1 MMT 03/31/2009 Update TrnHist file when credit Memo created [T20070214.0006]
*  E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[T20090605.0009]
*  B608948,1 MMT 07/28/2009 Update Styhist while creating and voiding CR[T20090715.0001]
*  B608991,1 AHS 09/02/2009 Using the store of the invoice instead the store of credit memo[T20090728.0011]
*  B608999,1 MMT 09/07/2009 credit memo saving doesnot include others in GST tax calc.[T20090827.0017]
*  B609037,1 TMI 10/12/2009 Update the TRNHIST file while saving using the store of the invoice [T20090728.0011   ]
*  E302760,1 MMT 09/22/2010 use Store code from Credit memo in case of Credit memo is not on invoice[T20100804.0005]
*  B609429,1 MMT 10/13/2010 Credit memo print does not calculate correctly with merchandise discount[T20100714.0006]
*  B609476,1 MMT 12/09/2010 Wrong PST  Value is calculated for canadian companies[T20101029.0011]
*  B609586,1 MMT 05/15/2011 Fix bug of Wrong PST Amount in Credit memo[T20110428.0011][Start]
*  B609587,1 SAB 05-16-2011 HST Tax is not caculated right and Other amount is not included in HST Tax [T20110328.0014]
*  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[T20110421.0019]
*  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{T20110428.0011}
*  B609823,1 TMI 02/08/2012 always call checkprg function to check the posting date [T20120106.0027]
*  B609888,1 MMT 04/11/2012 Fix bug of wrong PST calculation if CM is created from RA[T20120328.0025]
*  B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[T20110621.0057-Issue 26]
*  C201483,1 SAB 05/13/2012 New Customization to update site budget info on customer file[T20111206.0020][Start]
*  B609951,1 SAB 05/30/2012 Update credit memo on store level [T20120301.0003]
*  B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035]
*  B610380,1 HIA 06/13/13 T20130509.0007 - Gross profit invoiced report
*  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003]
*  B611012,1 MMT 06/04/2015 Include Vat in Trade Discount calculatons in Invoice and CR screens[T20150602.0002]
*  E304024,1 MMT 06/27/2018 Add trigger to credit memo saving program to update reference[T20180619.0002]
*!**********************************************************************************************
FUNCTION lfCMSav

PARAMETERS llPontSale , lcCrMemHdr , lcCrMemLin, llFromEdi , lcCrMemo , oBusObj

lcEngChrg = oBusObj.lcEngChrg

lcOpenLine = IIF(llPontSale,gfTempName(),oBusObj.lcOpenLine )
*-- Check if the current country is england or not.
llIsEnglnd = IIF(llPontSale,IIF(UPPER(ALLTRIM(oAriaApplication.DefaultCountry)) = 'ENG', .T., .F.),oBusObj.llIsEnglnd)
*-- If country is Canada.
llIsCanada = IIF(llPontSale,IIF(UPPER(ALLTRIM(oAriaApplication.DefaultCountry)) = 'CANADA', .T., .F.),oBusObj.llIsCanada)
*-- If calculate tax.
llTax      = IIF(llPontSale,ALLTRIM(gfGetMemVar('M_TAX',oAriaApplication.ActiveCompanyID)) = 'Y',oBusObj.llTax)
*-- Tax method.
lcTax_Meth = IIF(llPontSale,gfGetMemVar('M_TAX_METH',oAriaApplication.ActiveCompanyID),oBusObj.lcTax_Meth)
*-- See if there is GL link or not.
llLink_GL  = IIF(llPontSale,ALLTRIM(gfGetMemVar('M_LINK_GL',oAriaApplication.ActiveCompanyID)) = 'Y',oBusObj.llLink_GL)
*-- If link by division.
llDiv_Link = IIF(llPontSale,ALLTRIM(gfGetMemVar('M_DIV_LINK',oAriaApplication.ActiveCompanyID))  = 'Y',oBusObj.llDiv_Link)
*-- Post factored A/R to customer accounts or not.
llPostfInv = IIF(llPontSale,ALLTRIM(gfGetMemVar('XPOSTFINV',oAriaApplication.ActiveCompanyID)) = 'N',oBusObj.llPostfInv)
*-- If use the style average cost or not.
llAvg_Cost = IIF(llPontSale,ALLTRIM(gfGetMemVar('M_COST_METH',oAriaApplication.ActiveCompanyID)) = 'A',oBusObj.llAvg_Cost)
*-- Use Dyelot or not.
llUseDyes   = IIF(llPontSale,ALLTRIM(gfGetMemVar('M_DYELOT',oAriaApplication.ActiveCompanyID))    = 'Y',oBusObj.llUseDyes)
*-- Use Configuration or not.
llUseConfig = IIF(llPontSale,ALLTRIM(gfGetMemVar('M_STYCNFG',oAriaApplication.ActiveCompanyID))    = 'Y',oBusObj.llUseConfig)
*  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[Start]
llEditLineComm = IIF(llPontSale,ALLTRIM(gfGetMemVar('M_STY_COM',oAriaApplication.ActiveCompanyID))    = 'Y',oBusObj.editcommperline)
*  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[End]

IF llPontSale
  lcGLDstTmp = ""      && Var. hold the temp. name of gl distribution file.
  lcGLDstTmp = gfTempName()
  IF llLink_GL
    SELECT GLDIST
    COPY STRUCTURE TO (oAriaApplication.WorkDir+lcGLDstTmp)
    =gfOpenFile(oAriaApplication.WorkDir+lcGLDstTmp,'','EX')
    INDEX ON GLaccount TAG lcGLDstTmp
  ENDIF
ELSE
  lcGLDstTmp = oBusObj.lcGLDstTmp
ENDIF
lcInvSlLnk = IIF(llPontSale,"",oBusObj.lcInvSlLnk)      && Var. hold the invoice GL link sales account.
lcCstSlLnk = IIF(llPontSale,"",oBusObj.lcCstSlLnk)      && Var. hold the customer GL link sales account.
lcGLFYear  = IIF(llPontSale,"",oBusObj.lcGLFYear)       && Var. hold GL fisacl year.
lcGLPeriod = IIF(llPontSale,"",oBusObj.lcGLPeriod)      && Var. hold GL fisacl period.
lcRmGlSess = ""      && Var. hold the GL session no.
llRmGlSess = .F.     && Flag to know if create GL session no. or not.
*!* B609586,1 MMT 05/15/2011 Fix bug of Wrong PST Amount in Credit memo[T20110428.0011][Start]
*lnPstRate  = IIF(llPontSale,0,oBusObj.Parent.pgfReCrmem.pgDetail.cntDetail.txtPstRat.Value)        && Var. hold the default pst rate for canada.
lnPstRate  = IIF(llPontSale,0,oBusObj.lnDfltPstR)        && Var. hold the default pst rate for canada.
*!* B609586,1 MMT 05/15/2011 Fix bug of Wrong PST Amount in Credit memo[T20110428.0011][End]
*B609587,1 SAB 05-16-2011 HST Tax is not caculated right and Other amount is not included in HST Tax [Start]
lnHstRate  = 0       && Var. hold the default Hst rate for canada.
lnHstTotal = 0
*B609587,1 SAB 05-16-2011 [End]

lnPstTotal = IIF(llPontSale,0,oBusObj.lnPstTotal)       && Var. hold the total pst amount.
lnInvTrdDs = IIF(llPontSale,0,oBusObj.lnInvTrdDs)       && Var. hold the trade discount variable.

*-- Get the default of Canada tax rule from invoice file if there is invoice no.
IF !EMPTY(&lcCrMemHdr..Invoice) .AND. oBusObj.INVHDR.SEEK(&lcCrMemHdr..Invoice)
  lcPstRule  = IIF(llPontSale,InvHdr.cTaxRule,oBusObj.lcPstRule)
  lcInvTerms = IIF(llPontSale,INVHDR.cTermCode,oBusObj.lcInvTerms)
  *  B609476,1 MMT 12/09/2010 Wrong PST Value is calculated for canadian companies[Start]
  lnPstRate = invhdr.npstrate
  *  B609476,1 MMT 12/09/2010 Wrong PST Value is calculated for canadian companies[End]
  *B609587,1 SAB 05-16-2011 HST Tax is not caculated right and Other amount is not included in HST Tax [Start]
  *  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{Start}
  IF oBusObj.llstathst
    *  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{End}
    lnHstRate = invhdr.nhstrate
    *  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{Start}
    *B609888,1 MMT 04/11/2012 Fix bug of wrong PST calculation if CM is created from RA[Start]
    lnPstRate = 0
    *B609888,1 MMT 04/11/2012 Fix bug of wrong PST calculation if CM is created from RA[END]
  ENDIF
  *  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{End}
  *B609587,1 SAB 05-16-2011 [End]
ELSE
  lcInvTerms = IIF(llPontSale,"",oBusObj.lcInvTerms)
  *-- Get the default tax rule from the customer file.
  IF !EMPTY(&lcCrMemHdr..Account) .AND. oBusObj.CUSTOMER.SEEK("M"+&lcCrMemHdr..Account)
    lcPstRule  = IIF(llPontSale,Customer.cTaxRule,oBusObj.lcPstRule)
    *B609587,1 SAB 05-16-2011 HST Tax is not caculated right and Other amount is not included in HST Tax [Start]

    *  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{Start}
    IF oBusObj.llstathst
      *  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{End}
      lnHstRate = Customer.nTaxRate
      *  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{Start}
      *B609888,1 MMT 04/11/2012 Fix bug of wrong PST calculation if CM is created from RA[Start]
      lnPstRate = 0
      *B609888,1 MMT 04/11/2012 Fix bug of wrong PST calculation if CM is created from RA[END]
    ENDIF
    *  B609623,1 MMT 06/19/2011 Fix bug of wrong HST amount in CM{End}

    *B609587,1 SAB 05-16-2011 HST Tax is not caculated right and Other amount is not included in HST Tax [End]
  ELSE
    lcPstRule  = IIF(llPontSale,"",oBusObj.lcPstRule)
  ENDIF
ENDIF
*-- Get the invoice trade discount if it was not defined.
lnInvTrdDs = IIF(llPontSale,IIF(!EMPTY(&lcCrMemHdr..Invoice) .AND. oBusObj.INVHDR.SEEK(&lcCrMemHdr..Invoice ) , INVHDR.trde_disc , 0),oBusObj.lnInvTrdDs)

*-- Default the tax rate to the first tax rate.
GO TOP IN (lcCrMemLin)
lnTaxRate = IIF(llPontSale,&lcCrMemLin..Tax_Rate,oBusObj.PARENT.pgfReCrmem.pgHeader.cntHeader.txtTax.VALUE)


IF oBusObj.llMulCurr
  *-- Get exchange rate sign and unit sign.
  lcUntSin = ' '
  lcExRSin = gfGetExSin(@lcUntSin , &lcCrMemHdr..cCurrCode)
ELSE
  lcUntSin   = '/'        &&  Variable to hold unit sign.
  lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
ENDIF

*-- Validate the posting date if called from point of sale program & one of
*-- GL fiscal year or period is empty.
*B609823,1 TMI 02/08/2012 [Start] always call checkprg function to check the posting date [T20120106.0027]
*IF llPontSale .AND. (EMPTY(lcGLFYear) .OR. EMPTY(lcGLPeriod))
*B609823,1 TMI 02/08/2012 [End  ]
=CheckPrd(&lcCrMemHdr..dPostDate , 'lcGLFYear' , 'lcGLPeriod' , 'RM' , .T.)
*B609823,1 TMI 02/08/2012 [Start] close the above if
*ENDIF
*B609823,1 TMI 02/08/2012 [End  ]

*-- Get the year & period for the transaction date.
STORE "" TO lcTrYear , lcTrPeriod
=CHECKPRD(&lcCrMemHdr..CrDate , 'lcTrYear' , 'lcTrPeriod' , 'RM' , .T.)

*-- Check if the current customer is vat exempted or not from the customer file.
IF !EMPTY(&lcCrMemHdr..Account)
  IF !EMPTY(&lcCrMemHdr..STORE) .AND. oBusObj.CUSTOMER.SEEK("S"+&lcCrMemHdr..Account+&lcCrMemHdr..STORE)
    llVatExem  = CUSTOMER.lVatExem
  ELSE
    IF oBusObj.CUSTOMER.SEEK("M"+&lcCrMemHdr..Account)
      llVatExem  = CUSTOMER.lVatExem
    ENDIF
  ENDIF
ELSE
  llVatExem  = .F.
ENDIF

IF llLink_GL
  IF llDiv_Link .AND. !EMPTY(&lcCrMemHdr..cDivision)
    DECLARE laDRltFld[2,2]
    laDRltFld[1,1] = 'LINK_CODE'
    laDRltFld[1,2] = 'lcCustLink'
    laDRltFld[2,1] = 'CSLSGLLINK'
    laDRltFld[2,2] = 'lcCstSlLnk'
    =gfRltFld(&lcCrMemHdr..cDivision , @laDRltFld,'CDIVISION')
    lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
  ELSE
    lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
  ENDIF
ELSE
  lcCstSlLnk = ""
ENDIF

*-- Call local function to get the trade discount.
lnInvTrdDs = lfGetTrdDs(&lcCrMemHdr..cTermCode , &lcCrMemHdr..Invoice , oBusObj)
SELECT (lcCrMemHdr)

*B608484,1 MHM 03/17/2008 Fix bug of not calculating Comm correctlly [T20080305.0009] [Start]
*- move this part tell we recalculate Total amount
*!*	*-- Lock the current header record to grantee the phiscal update.
*!*	=RLOCK()
*!*	*-- Calculate the salesreps. charge back.
*!*	IF lnInvTrdDs <> 0
*!*	  REPLACE CommAmt1 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..CommPcnt1/100),2)) * -1 ;
*!*	          CommAmt2 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..CommPcnt2/100),2)) * -1
*!*	ELSE
*!*	  REPLACE CommAmt1 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..CommPcnt1/100),2)) * -1 ;
*!*	          CommAmt2 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..CommPcnt2/100),2)) * -1
*!*	ENDIF
*!*	UNLOCK
*--b608484[End]

*!*	*C102676,1 Custom process for A.S.T. [Begin]
*!*	IF ASCAN(laEvntTrig,PADR('CRUPCOM3',10))<>0
*!*	  =gfDoTriger('RMCRMEM',PADR('CRUPCOM3',10))
*!*	ENDIF
*!*	*C102676,1 Custom process for A.S.T. [End]
*-- Define temp. var. to hold the calculated taxes.
IF llTax
  *-- To display the total PST tax amount if the country was canada.
  IF llIsCanada
    SELECT (lcCrMemLin)
    *B609587,1 SAB 05-16-2011 HST Tax is not caculated right and Other amount is not included in HST Tax [T20110328.0014]
    *SUM Amount * (nPstRate/100) , Amount * (Tax_Rate/100) ;
    *    FOR !EMPTY(Style) TO lnPstTotal , lnTot_Tax
    *lnPstTotal = lnPstTotal + &lcCrMemHdr..Other * (lnPstRate/100)
    SUM Amount * (nPstRate/100) , Amount * (nHstRate/100) , Amount * (Tax_Rate/100) ;
      FOR !EMPTY(STYLE) TO lnPstTotal , lnHstTotal, lnTot_Tax
    lnPstTotal = lnPstTotal + &lcCrMemHdr..OTHER * (lnPstRate/100)
    lnHstTotal = lnHstTotal + &lcCrMemHdr..OTHER * (lnHstRate/100)
    *B609587,1 SAB 05-16-2011 [End]


    *  B608999,1 MMT 09/07/2009 credit memo saving doesnot include others in GST tax calc.[Start]
    lnTot_Tax = lnTot_Tax + + &lcCrMemHdr..OTHER * (lnTaxRate/100)
    *  B608999,1 MMT 09/07/2009 credit memo saving doesnot include others in GST tax calc.[End]

    *-- Take PST due to rule.
    IF VAL(ALLTRIM(lcPstRule)) <> 1
      lnPstTotal = lnPstTotal + lnTot_Tax * (lnPstRate/100)
    ENDIF
    SELECT (lcCrMemHdr)
    *-- Lock the current header record to grantee the phiscal update.
    =RLOCK()
    *-- Update the tax field.
    REPLACE Tax_Amt  WITH lnTot_Tax
    UNLOCK
  ENDIF
ENDIF

SELECT (lcCrMemLin)
*-- Calculate the totals for the following:
*-- Qty, Amount, discount amount, trade discount amount.
SUM ALL TotQty , (Gros_Price*TotQty)*(1-(Disc_Pcnt/100)) , ;
  (TotQty*Gros_Price) , Disc_amt , Trde_Amt FOR !EMPTY(STYLE) ;
  TO lnTPieces , lnTAmount , lnTGrosAmt , lnTDiscAmt , lnTTrdeAmt

lnPstTotal = IIF(llTax .AND. llIsCanada , lnPstTotal , 0)

*B611012,1 MMT 06/04/2015 Include Vat in Trade Discount calculatons in Invoice and CR screens[T20150602.0002][Start]
IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry)) = 'ENG'
  lnTTrdeAmt = lnTTrdeAmt + ROUND((&lcCrMemHdr..Tax_Amt *  lnInvTrdDs /100),2)
ENDIF  
*B611012,1 MMT 06/04/2015 Include Vat in Trade Discount calculatons in Invoice and CR screens[T20150602.0002][End]


SELECT (lcCrMemHdr)
*-- Lock the current record to grantee the phisacl update.
=RLOCK()
*  B609587,1 SAB 05-16-2011 HST Tax is not caculated right and Other amount is not included in HST Tax [Start]
*!*	REPLACE Pieces    WITH lnTPieces  ;
*!*	        Gross_Amt WITH lnTGrosAmt ;
*!*	        Disc_Amt  WITH lnTDiscAmt ;
*!*	        Amount    WITH lnTAmount  ;
*!*	        Tax_Amt   WITH IIF(llTax , Tax_Amt , 0) ;
*!*	        nPstAmt   WITH lnPstTotal ;
*!*	        TotCredit WITH Other + Amount + Tax_Amt + nPstAmt + nhstamt
REPLACE Pieces    WITH lnTPieces  ;
  Gross_Amt WITH lnTGrosAmt ;
  Disc_Amt  WITH lnTDiscAmt ;
  Amount    WITH lnTAmount  ;
  Tax_Amt   WITH IIF(llTax , Tax_Amt , 0) ;
  nPstAmt   WITH lnPstTotal ;
  nhstamt WITH lnHstTotal;
  TotCredit WITH OTHER + Amount + Tax_Amt + nPstAmt + nhstamt
*  B609587,1 SAB 05-16-2011 HST Tax is not caculated right and Other amount is not included in HST Tax [End]
UNLOCK

PRIVATE    lnPieces,lnTotQty,lnGrossAmt,lnAmount
STORE 0 TO lnPieces,lnTotQty,lnGrossAmt,lnAmount
*  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[Start]
PRIVATE    lnCommAmt1 ,lnCommAmt2
STORE 0 TO lnCommAmt1 ,lnCommAmt2
*  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[End]

*  B609429,1 MMT 10/13/2010 Credit memo print does not calculate correctly with merchandise discount[Start]
PRIVATE lnDiscAmt
STORE 0 TO lnDiscAmt
*  B609429,1 MMT 10/13/2010 Credit memo print does not calculate correctly with merchandise discount[End]
SELECT (lcCrMemLin)
LOCATE
SCAN
  *-- Variable to Hold TotQty.
  lnTotQty   = Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  IF lnTotQty = 0
    LOOP
  ELSE
    *-- Variable to Hold Pieces.
    lnPieces   = lnPieces   + lnTotQty
    *-- Variable to Hold Gross Amount.
    lnGrossAmt = lnGrossAmt + (Gros_Price * lnTotQty)
    *-- Variable to Hold Net Amount.

    *B608739,1 WAM 11/06/2008 Calculate total amount based on the amount field because the Price field is rounded
    *lnAmount   = lnAmount   + (Price      * lnTotQty)
    lnAmount   = lnAmount   + Amount
    *B608739,1 WAM 11/06/2008 (End)
    *  B609429,1 MMT 10/13/2010 Credit memo print does not calculate correctly with merchandise discount[Start]
    lnDiscAmt = lnDiscAmt + ((Gros_Price-PRICE) * lnTotQty)
    *  B609429,1 MMT 10/13/2010 Credit memo print does not calculate correctly with merchandise discount[End]
    *  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[Start]
    IF llEditLineComm
      IF lnInvTrdDs <> 0
        lnCommAmt1 = lnCommAmt1 + (AMOUNT  * (100 - lnInvTrdDs)/100) * (COMMPCNT1/100)
        lnCommAmt2 = lnCommAmt2 + (AMOUNT  * (100 - lnInvTrdDs)/100) * (COMMPCNT2/100)
      ELSE
        lnCommAmt1 = lnCommAmt1 +  (AMOUNT * COMMPCNT1/100)
        lnCommAmt2 = lnCommAmt2 +  (AMOUNT * COMMPCNT2/100)
      ENDIF
    ENDIF
    *  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[End]

  ENDIF
ENDSCAN

SELECT (lcCrMemHdr)
*-- Lock the current record to grantee the phisacl update.
=RLOCK()
*  B609429,1 MMT 10/13/2010 Credit memo print does not calculate correctly with merchandise discount[Start]
*!*	REPLACE Pieces    WITH lnPieces           ,;
*!*	        Gross_Amt WITH lnGrossAmt         ,;
*!*	        Amount    WITH lnAmount           ,;
*!*	        TotCredit WITH lnAmount + Other + Tax_Amt + nPstAmt + nhstamt,;
*!*	        Disc_Amt  WITH lnGrossAmt - lnAmount
REPLACE Pieces    WITH lnPieces           ,;
  Gross_Amt WITH lnGrossAmt         ,;
  Amount    WITH lnAmount           ,;
  TotCredit WITH lnAmount + OTHER + Tax_Amt + nPstAmt + nhstamt,;
  Disc_Amt  WITH lnDiscAmt
*  B609429,1 MMT 10/13/2010 Credit memo print does not calculate correctly with merchandise discount[End]

*B608484,1 MHM 03/17/2008 Fix bug of not calculating Comm correctlly [T20080305.0009] [Start]
*- move this part tell we recalculate Total amount
*-- Lock the current header record to grantee the phiscal update.
*-- Calculate the salesreps. charge back.
IF lnInvTrdDs <> 0
  REPLACE CommAmt1 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..CommPcnt1/100),2)) * -1 ;
    CommAmt2 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..CommPcnt2/100),2)) * -1
ELSE
  REPLACE CommAmt1 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..CommPcnt1/100),2)) * -1 ;
    CommAmt2 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..CommPcnt2/100),2)) * -1
ENDIF
*B608484,1 MHM 03/17/2008 Fix bug of not calculating Comm correctlly [T20080305.0009] [End]

*  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[Start]
IF llEditLineComm
  IF lnCommAmt1 > 0 OR  lnCommAmt2 > 0
    lnNetAmt = &lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100
    REPLACE CommPcnt1 WITH IIF(lnCommAmt1 >0,lnCommAmt1 /lnNetAmt *100,CommPcnt1 ),;
      CommPcnt2 WITH IIF(lnCommAmt2 >0,lnCommAmt2 /lnNetAmt *100,CommPcnt2),;
      CommAmt1 WITH IIF(lnCommAmt1 >0,ABS(ROUND(lnCommAmt1,2))*-1 ,CommAmt1),;
      CommAmt2 WITH IIF(lnCommAmt2 >0,ABS(ROUND(lnCommAmt2,2))*-1, CommAmt2)
  ENDIF
ENDIF
*  B609597,1 MMT 05/30/2011 Modify Credit Memo screen to handle Sales rep. per line[End]
UNLOCK

IF llLink_GL
  IF &lcCrMemHdr..nSteps < 2
    *-- Call GL Distribution Procedure To Add The Following
    *-- Accounts For This Return.
    IF UPPER(ALLTRIM(oAriaApplication.DefaultCountry))='ENG' .AND. TYPE('lcEngChrg') = "C" .AND.;
        !EMPTY(lcEngChrg) .AND. USED(lcEngChrg)

      IF SEEK(&lcCrMemHdr..ORDER+&lcCrMemHdr..STORE,lcEngChrg)
        *-- the company is england so update the Gl with the english charges
        *--In case of england charges taxes is at style level
        SELECT (lcEngChrg)
        SCAN REST WHILE ORDER+cStore+PikTkt+cchrgcode = &lcCrMemHdr..ORDER+&lcCrMemHdr..STORE

          SCATTER MEMVAR MEMO
          m.CrMemo = lcCrMemo
          m.Store  = cStore
          DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '004',&lcEngChrg..nChrgAmnt, 'RM' ,;
            &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
            lcGLFYear,lcGLPeriod , '&lcGLDstTmp' , &lcEngChrg..cFrgtAcnt ,;
            &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
            &lcCrMemHdr..nExRate
          oBusObj.CrmChrg.INSERT('FROM MEMVAR')
          *B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[Start]
          *=gfAdd_Info('CRMCHRG')
          =gfAdd_Info(oBusObj.CrmChrg.lcCursorUpdate)
          *B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[END]
        ENDSCAN
      ENDIF
    ELSE
      *-- Do the regular case
      DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '004',&lcCrMemHdr..OTHER , 'RM' ,;
        &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
        lcGLFYear,lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cFrgtAcnt ,;
        &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
        &lcCrMemHdr..nExRate
    ENDIF

    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    IF &lcCrMemHdr..OTHER <> 0
      REPLACE cFrgtAcnt WITH &lcGLDstTmp..glaccount
    ENDIF
    REPLACE nSteps    WITH 2
    UNLOCK
  ENDIF

  *-- If The System Has Been Setup To Compute Tax.
  IF llTax
    IF &lcCrMemHdr..nSteps < 3
      *!*	        *C200517,1 KHM 04/30/2003 (Begin) To update the GLAccount in the GLDIST file with
      *!*	        *C200517,1                6500-000-000-0000 for catg_key = "014" and cTax = "000100"
      *!*	        IF ASCAN(laEvntTrig,PADR('UPDGLACT',10))<>0
      *!*	          =gfDoTriger('RMCRMEM',PADR('UPDGLACT',10))
      *!*	        ENDIF
      *!*	        *C200517,1 KHM 04/30/2003 (End)

      *  E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[Start]
      *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
      &lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt+&lcCrMemHdr..nhstamt , 'RM' ,;
      &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
      lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
      &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
      &lcCrMemHdr..nExRate


      IF llIsCanada
        IF LFCHKSTATHST(LEFT(CUSTOMER.CADDRESS4,6))
          DO GLDIST WITH &LCCRMEMHDR..LINK_CODE , '030' ,;
            &LCCRMEMHDR..NHSTAMT , 'RM' ,;
            &LCCRMEMHDR..CRMEMO , &LCCRMEMHDR..DPOSTDATE ,;
            LCGLFYEAR , LCGLPERIOD , '&LCGLDSTTMP' , &LCCRMEMHDR..CTAXACNT ,;
            &LCCRMEMHDR..CCURRCODE , &LCCRMEMHDR..NCURRUNIT ,;
            &LCCRMEMHDR..NEXRATE
        ELSE
          DO GLDIST WITH &LCCRMEMHDR..LINK_CODE , '014' ,;
            &LCCRMEMHDR..TAX_AMT , 'RM' ,;
            &LCCRMEMHDR..CRMEMO , &LCCRMEMHDR..DPOSTDATE ,;
            LCGLFYEAR , LCGLPERIOD , '&LCGLDSTTMP' , &LCCRMEMHDR..CTAXACNT ,;
            &LCCRMEMHDR..CCURRCODE , &LCCRMEMHDR..NCURRUNIT ,;
            &LCCRMEMHDR..NEXRATE

          DO GLDIST WITH &LCCRMEMHDR..LINK_CODE , '029' ,;
            &LCCRMEMHDR..NPSTAMT , 'RM' ,;
            &LCCRMEMHDR..CRMEMO , &LCCRMEMHDR..DPOSTDATE ,;
            LCGLFYEAR , LCGLPERIOD , '&LCGLDSTTMP' , &LCCRMEMHDR..CTAXACNT ,;
            &LCCRMEMHDR..CCURRCODE , &LCCRMEMHDR..NCURRUNIT ,;
            &LCCRMEMHDR..NEXRATE

        ENDIF
      ELSE
        DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
          &lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt+&lcCrMemHdr..nhstamt , 'RM' ,;
          &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
          lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
          &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
          &lcCrMemHdr..nExRate
      ENDIF
      *  E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[End]

      SELECT (lcCrMemHdr)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      IF (&lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt) <> 0
        REPLACE cTaxAcnt WITH &lcGLDstTmp..glaccount
      ENDIF

      *!*	      *C200517,1 KHM 04/30/2003 (Begin) To replace cTaxAcnt with the related field
      *!*	      *C200517,1                (glaccount) of taxcode "000100"
      *!*	      IF ASCAN(laEvntTrig,PADR('REPTAXAC',10))<>0
      *!*	        =gfDoTriger('RMCRMEM',PADR('REPTAXAC',10))
      *!*	      ENDIF
      *!*	      *C200517,1 KHM 04/30/2003 (End)
      REPLACE nSteps   WITH 3
      UNLOCK
    ENDIF
  ENDIF

  IF &lcCrMemHdr..nSteps < 4
    DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '001' , -(&lcCrMemHdr..TOTCREDIT),;
      'RM' , &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
      lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cARAcnt ,;
      &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
      &lcCrMemHdr..nExRate

    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    IF &lcCrMemHdr..TOTCREDIT <> 0
      REPLACE cArAcnt WITH &lcGLDstTmp..glaccount
    ENDIF
    REPLACE nSteps  WITH 4
    UNLOCK
  ENDIF
ENDIF
IF &lcCrMemHdr..nSteps < 5
  SELECT (lcCrMemHdr)
  SCATTER MEMVAR MEMO

  *-- Save the current credit memo to the master return header file.
  SELECT (oBusObj.RETHDR.lcCursorUpdate)
  APPEND BLANK
  m.CrMemo = lcCrMemo
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  GATHER MEMVAR MEMO

  *-- Call global function to add audit fields info.
  =gfAdd_Info(oBusObj.RETHDR.lcCursorUpdate)

  *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
  *!*	  oBusObj.RETHDR.REPLACE('cEdit_User  WITH oAriaApplication.User_ID')
  *!*	  oBusObj.RETHDR.REPLACE('dEdit_Date  WITH DATE()')
  *!*	  oBusObj.RETHDR.REPLACE('cEdit_Time  WITH gfGetTime()')
  *!*	  oBusObj.RETHDR.REPLACE('cEdt_Ver    WITH "A40"')
  SELECT (oBusObj.RETHDR.lcCursorUpdate)
  REPLACE cEdit_User  WITH oAriaApplication.User_ID,;
    dEdit_Date  WITH DATE(),;
    cEdit_Time  WITH gfGetTime(),;
    cEdt_Ver    WITH "A40"

  *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

  UNLOCK

  *-- Call Global Function to transmit the local data.
  =gfTraceKey(oBusObj.RETHDR.lcCursorUpdate , RetHdr.CRMEMO , "A")

  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  REPLACE nSteps WITH 5
  UNLOCK
ENDIF

SELECT (lcCrMemLin)
*-- Lock the record to grantee the phiscal update.
= RLOCK()

DELETE ALL FOR EMPTY(STYLE) .OR. (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8) = 0
UNLOCK

*-- Flag to know if cancel the difference between
*-- the open qty. & the received qty.
llCancRest = .F.

=lfCrtUnComp(.F. , .F. , .F. , .T.,llFromEDI)

SELECT (lcCrMemLin)
GO TOP
DECLARE laOrgQty[8] , laOpnLin[8]
SCAN
  STORE 0 TO laOpnLin , laOrgQty
  SCATTER MEMO TO MEMVAR
  SELECT (lcOpenLine)
  IF !SEEK(&lcCrMemLin..ACCOUNT+&lcCrMemLin..STYLE+&lcCrMemLin..Cret_LinNo, lcOpenLine)
    APPEND BLANK
  ENDIF
  SCATTER FIELDS Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 TO laOrgQty
  GATHER MEMO FROM MEMVAR
  FOR lnOpn = 1 TO 8
    lcOpn = ALLTRIM(STR(lnOpn))
    REPLACE Qty&lcOpn     WITH Qty&lcOpn + laOrgQty[lnOpn]
  ENDFOR
  REPLACE TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  SELECT (lcCrMemLin)
ENDSCAN
SELECT (lcOpenLine)

*-- Locate for any open qty in all the lines & ask the user to leave it
*-- open or cancel the difference between the open & the received.
LOCATE FOR Qty1 < nOpnQty1 .OR. Qty2 < nOpnQty2 .OR. Qty3 < nOpnQty3 .OR. ;
  Qty4 < nOpnQty4 .OR. Qty5 < nOpnQty5 .OR. Qty6 < nOpnQty6 .OR. ;
  Qty7 < nOpnQty7 .OR. Qty8 < nOpnQty8
IF FOUND()
  *** There is open qty. in the current RA.  Do you want to ***
  *** receive this RA completely and consider the open qty. ***
  *** as canceled or leave the RA open and consider the     ***
  *** previous receive as partially receive?
  *** < Completely > - < Partially >
  IF gfModalGen("QRM46009B46000" , "DIALOG") = 1
    llCancRest = .T.
  ENDIF
ENDIF



SELECT (lcCrMemLin)
lnLineCnt  = 1    && Var. hold line counter.
lnTotCancl = 0    && Var. hold total qty. canceled.

IF !llRmGlSess
  lcRmGlSess = gfsequence('GLSESSION')

  *C131527,1 KHM [Start]
  lcSessNo   = lcRmGlSess
  *C131527,1 KHM [End]

  llRmGlSess = .T.
ENDIF

*-- Var. hold the incremental # will be saved in the nSteps field in the
*-- credit memo line file.
lnNxtStp   = 0

IF &lcCrMemHdr..nSteps < 6
  lnCurRec  = 1                     && Var. hold current record no.
  lnTotRec  = RECCOUNT(lcCrMemLin)  && Var. hold total lines record count.
  IF !llPontSale
    lcCurrtag= ORDER()
    SET ORDER TO TAG (oBusObj.lcCrMmLine)
  ENDIF
  opross = CREATEOBJECT('ariaprogressbar')
  oPross.lblFirstLabel.CAPTION = "Saving credit memo"
  oPross.TotalProgress = lnTotRec
  oPross.AUTOCENTER = .T.
  oPross.SHOW()

  *B608074,1 05/06/2007 MMT fix bug of not saving CR details [Start]
  SELECT (lcCrMemLin)
  *B608074,1 05/06/2007 MMT fix bug of not saving CR details [End]

  SCAN FOR !EMPTY(STYLE)
    lnNxtStp = 0
    *-- Call the global function that execute the thermometer.
    oPross.lblSecondLabel.CAPTION = "Saving credit memo # : "+&lcCrMemHdr..CrMemo
    oPross.CurrentProgress(lnCurRec)
    lnCurRec = lnCurRec + 1

    SELECT (lcCrMemLin)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()

    REPLACE CRMEMO     WITH &lcCrMemHdr..CrMemo ;
      CRET_LINNO WITH IIF(llPontSale , CRET_LINNO , ALLTRIM(STR(lnLineCnt)))
    *-- If there is a R/A no. & the line is already exist in RALINE file.
    IF !EMPTY(&lcCrMemHdr..RaNo) .AND. oBusObj.RALINE.SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..STYLE)
      llRALinExs = .T.    && The current line exist in RALINE file.
    ELSE
      llRALinExs = .F.    && The current line does not exist in RALINE file.
    ENDIF

    SELECT STYLE
    *-- If there is return style , get its sales code
    IF !EMPTY(&lcCrMemLin..cretSty) .AND. SEEK(&lcCrMemLin..cretSty)
      lcStySlLnk = STYLE.cslsgllink
    ELSE
      lcStySlLnk = ""
    ENDIF
    =SEEK(&lcCrMemLin..STYLE)
    llInvSty = STYLE.lInvSty

    *-- If return to the original style, get its sales code.
    lcStySlLnk = IIF(EMPTY(lcStySlLnk) , STYLE.cslsgllink , lcStySlLnk )
    SELECT (lcCrMemLin)
    REPLACE GL_SALES WITH IIF(EMPTY(lcInvSlLnk) , ALLTRIM(lcCstSlLnk) + ALLTRIM(lcStySlLnk) , ALLTRIM(lcInvSlLnk) + ALLTRIM(lcStySlLnk))
    UNLOCK

    IF &lcCrMemLin..nSteps < 1
      *-- Now Update The Style Dyelot Inventory For Dyelot
      *-- System And Styles That Come In Dyelots.
      IF (llUseDyes OR llUseConfig).AND. STYLE.cDye_Flg = 'Y' .AND. llInvSty
        SELECT STYDYE
        IF oBusObj.STYDYE.SEEK(&lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + &lcCrMemLin..DYELOT)
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          oBusObj.STYDYE.REPLACE('Ret1   WITH Ret1 + &lcCrMemLin..Qty1')
          oBusObj.STYDYE.REPLACE('Ret2   WITH Ret2 + &lcCrMemLin..Qty2')
          oBusObj.STYDYE.REPLACE('Ret3   WITH Ret3 + &lcCrMemLin..Qty3')
          oBusObj.STYDYE.REPLACE('Ret4   WITH Ret4 + &lcCrMemLin..Qty4')
          oBusObj.STYDYE.REPLACE('Ret5   WITH Ret5 + &lcCrMemLin..Qty5')
          oBusObj.STYDYE.REPLACE('Ret6   WITH Ret6 + &lcCrMemLin..Qty6')
          oBusObj.STYDYE.REPLACE('Ret7   WITH Ret7 + &lcCrMemLin..Qty7')
          oBusObj.STYDYE.REPLACE('Ret8   WITH Ret8 + &lcCrMemLin..Qty8')
          oBusObj.STYDYE.REPLACE('TotRet WITH Ret1+Ret2+Ret3+Ret4+Ret5+Ret6+Ret7+Ret8')

          *-- If this line from RA & exist in RALINE file, Adjust RA fields.
          IF llRALinExs
            oBusObj.STYDYE.REPLACE('RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1)')
            oBusObj.STYDYE.REPLACE('RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2)')
            oBusObj.STYDYE.REPLACE('RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3)')
            oBusObj.STYDYE.REPLACE('RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4)')
            oBusObj.STYDYE.REPLACE('RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5)')
            oBusObj.STYDYE.REPLACE('RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6)')
            oBusObj.STYDYE.REPLACE('RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7)')
            oBusObj.STYDYE.REPLACE('RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8)')
            oBusObj.STYDYE.REPLACE('TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8')
          ENDIF
          UNLOCK

          *-- Call Global Function to transmit the local data.
          =gfTraceKey("StyDye" , &lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + &lcCrMemLin..DYELOT , "M")
        ENDIF
      ENDIF
      SELECT (lcCrMemLin)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE nSteps WITH 1
      UNLOCK
    ENDIF

    IF &lcCrMemLin..nSteps < 2
      *-- Update the warehouse record in the StyDye file if the
      *-- system is setup to use the multiple warehouses.
      SELECT StyDye
      IF oBusObj.StyDye.SEEK(&lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + SPACE(10))
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        IF llInvSty
          oBusObj.StyDye.REPLACE('Ret1   WITH Ret1 + &lcCrMemLin..Qty1')
          oBusObj.StyDye.REPLACE('Ret2   WITH Ret2 + &lcCrMemLin..Qty2')
          oBusObj.StyDye.REPLACE('Ret3   WITH Ret3 + &lcCrMemLin..Qty3')
          oBusObj.StyDye.REPLACE('Ret4   WITH Ret4 + &lcCrMemLin..Qty4')
          oBusObj.StyDye.REPLACE('Ret5   WITH Ret5 + &lcCrMemLin..Qty5')
          oBusObj.StyDye.REPLACE('Ret6   WITH Ret6 + &lcCrMemLin..Qty6')
          oBusObj.StyDye.REPLACE('Ret7   WITH Ret7 + &lcCrMemLin..Qty7')
          oBusObj.StyDye.REPLACE('Ret8   WITH Ret8 + &lcCrMemLin..Qty8')
          oBusObj.StyDye.REPLACE('TotRet WITH Ret1+Ret2+Ret3+Ret4+Ret5+Ret6+Ret7+Ret8')
          *-- If this line from RA & exist in RALINE file, Adjust RA fields.
          IF llRALinExs
            oBusObj.StyDye.REPLACE('RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1)')
            oBusObj.StyDye.REPLACE('RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2)')
            oBusObj.StyDye.REPLACE('RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3)')
            oBusObj.StyDye.REPLACE('RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4)')
            oBusObj.StyDye.REPLACE('RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5)')
            oBusObj.StyDye.REPLACE('RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6)')
            oBusObj.StyDye.REPLACE('RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7)')
            oBusObj.StyDye.REPLACE('RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8)')
            oBusObj.StyDye.REPLACE('TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8')
          ENDIF
        ENDIF

        UNLOCK
        *-- Call Global Function to transmit the local data.
        =gfTraceKey("StyDye" , &lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + SPACE(10) , "M")
      ENDIF

      SELECT (lcCrMemLin)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE nSteps WITH 2
      UNLOCK
    ENDIF

    IF &lcCrMemLin..nSteps < 3
      SELECT STYLE
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      IF llInvSty
        oBusObj.STYLE.REPLACE('RET1   WITH RET1 + &lcCrMemLin..QTY1')
        oBusObj.STYLE.REPLACE('RET2   WITH RET2 + &lcCrMemLin..QTY2')
        oBusObj.STYLE.REPLACE('RET3   WITH RET3 + &lcCrMemLin..QTY3')
        oBusObj.STYLE.REPLACE('RET4   WITH RET4 + &lcCrMemLin..QTY4')
        oBusObj.STYLE.REPLACE('RET5   WITH RET5 + &lcCrMemLin..QTY5')
        oBusObj.STYLE.REPLACE('RET6   WITH RET6 + &lcCrMemLin..QTY6')
        oBusObj.STYLE.REPLACE('RET7   WITH RET7 + &lcCrMemLin..QTY7')
        oBusObj.STYLE.REPLACE('RET8   WITH RET8 + &lcCrMemLin..QTY8')
        oBusObj.STYLE.REPLACE('TOTRET WITH RET1+RET2+RET3+RET4+RET5+RET6+RET7+RET8')
        *-- If this line from RA & exist in RALINE file, Adjust RA fields.
        IF llRALinExs
          oBusObj.STYLE.REPLACE('RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1)')
          oBusObj.STYLE.REPLACE('RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2)')
          oBusObj.STYLE.REPLACE('RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3)')
          oBusObj.STYLE.REPLACE('RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4)')
          oBusObj.STYLE.REPLACE('RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5)')
          oBusObj.STYLE.REPLACE('RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6)')
          oBusObj.STYLE.REPLACE('RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7)')
          oBusObj.STYLE.REPLACE('RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8)')
          oBusObj.STYLE.REPLACE('TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8')
        ENDIF
      ENDIF
      UNLOCK
      *-- Call Global Function to transmit the local data.
      =gfTraceKey("Style" , &lcCrMemLin..STYLE , "M")

      SELECT (lcCrMemLin)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      IF EMPTY(&lcCrMemLin..cRetSty) .AND. STYLE.cStyGrade <> &lcCrMemLin..cStyGrade
        REPLACE cStyGrade WITH STYLE.cStyGrade
      ENDIF
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF

    *-- Update general ledger entreis
    IF llLink_GL
      IF &lcCrMemLin..nSteps < 4
        DO GLDIST WITH &lcCrMemLin..GL_Sales , '020' , ;
          &lcCrMemLin..TotQty * &lcCrMemLin..Gros_Price , 'RM', ;
          &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
          lcGLFYear , lcGLPeriod , '&lcGLDstTmp', &lcCrMemLin..cSalesAcnt , ;
          &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
          &lcCrMemHdr..nExRate

        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        IF (&lcCrMemLin..TotQty * &lcCrMemLin..Gros_Price) <> 0
          REPLACE &lcCrMemLin..cSalesAcnt WITH &lcGLDstTmp..glaccount
        ENDIF
        REPLACE &lcCrMemLin..nSteps     WITH 4
        UNLOCK
      ENDIF

      IF &lcCrMemLin..nSteps < 5
        DO GLDIST WITH &lcCrMemLin..GL_Sales , '005' , ;
          -(&lcCrMemLin..DISC_AMT),'RM', ;
          &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
          lcGLFYear , lcGLPeriod , '&lcGLDstTmp', &lcCrMemLin..cDiscAcnt , ;
          &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
          &lcCrMemHdr..nExRate

        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        IF &lcCrMemLin..DISC_AMT <> 0
          REPLACE &lcCrMemLin..cDiscAcnt WITH &lcGLDstTmp..glaccount
        ENDIF
        REPLACE &lcCrMemLin..nSteps   WITH 5
        UNLOCK
      ENDIF
    ENDIF

    *E302590,1 MMT 03/31/2009 Update TrnHist file when credit Memo created [Start]
    IF USED('TRNHIST') AND !EMPTY(&lcCrMemLin..Employee)
      IF !gfSEEK('C'+&lcCrMemHdr..CRMEMO+&lcCrMemLin..Employee+&lcCrMemLin..STYLE,'TRNHIST')
        SELECT TRNHIST
        APPEND BLANK

        *B609037,1 TMI 10/12/2009 02:18:46 PM [Start] use the INVHDR.STORE field instead of &lcCrMemHdr..Store
        *REPLACE TRANTYPE WITH 'C' ;
        TRANNO   WITH &lcCrMemHdr..CRMEMO ;
        ACCOUNT  WITH &lcCrMemHdr..Account ;
        STORE    WITH &lcCrMemHdr..Store ;
        EMPLOYEE WITH &lcCrMemLin..Employee ;
        STYLE    WITH &lcCrMemLin..STYLE ;
        DTRANDT  WITH &lcCrMemHdr..crdate
        *  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003][Start]
*!*	        REPLACE TRANTYPE WITH 'C' ;
*!*	          TRANNO   WITH &lcCrMemHdr..CRMEMO ;
*!*	          ACCOUNT  WITH &lcCrMemHdr..Account ;
*!*	          STORE    WITH INVHDR.STORE ;
*!*	          EMPLOYEE WITH &lcCrMemLin..Employee ;
*!*	          STYLE    WITH &lcCrMemLin..STYLE ;
*!*	          DTRANDT  WITH &lcCrMemHdr..crdate
        REPLACE TRANTYPE WITH 'C' ;
          TRANNO   WITH &lcCrMemHdr..CRMEMO ;
          ACCOUNT  WITH &lcCrMemHdr..Account ;
          STORE    WITH IIF(!EMPTY(ALLTRIM(&lcCrMemHdr..invoice)) AND INVHDR.CONSOL <> 'Y',INVHDR.STORE,&lcCrMemHdr..Store) ;
          EMPLOYEE WITH &lcCrMemLin..Employee ;
          STYLE    WITH &lcCrMemLin..STYLE ;
          DTRANDT  WITH &lcCrMemHdr..crdate
        *  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003][End]
        *B609037,1 TMI 10/12/2009 02:19:16 PM [End  ]

      ENDIF
      REPLACE QTY1 WITH &lcCrMemLin..QTY1 ;
        QTY2 WITH &lcCrMemLin..QTY2 ;
        QTY3 WITH &lcCrMemLin..QTY3 ;
        QTY4 WITH &lcCrMemLin..QTY4 ;
        QTY5 WITH &lcCrMemLin..QTY5 ;
        QTY6 WITH &lcCrMemLin..QTY6 ;
        QTY7 WITH &lcCrMemLin..QTY7 ;
        QTY8 WITH &lcCrMemLin..QTY8 ;
        TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

      =gfAdd_Info('TRNHIST')
      =gfReplace()
    ENDIF

    *B608948,1 MMT 07/28/2009 Update Styhist while creating and voiding CR[Start]
    IF USED('STYHIST') AND !EMPTY(&lcCrMemLin..Employee)
      lcSty = PADR(SUBSTR(&lcCrMemLin..STYLE,1,oBusObj.lnMajorLen),19)
      lcClr = PADR(SUBSTR(&lcCrMemLin..STYLE,oBusObj.lnClrPos,oBusObj.lnClrLen ),6)
      *B608991,1 AHS 09/02/2009 Using the store of the invoice instead the store of credit memo[Start]
      *IF gfSEEK(&lcCrMemHdr..Account+&lcCrMemHdr..Store+&lcCrMemLin..EMPLOYEE+lcSty+lcClr,'STYHIST')
      *  E302760,1 MMT 09/22/2010 use Store code from Credit memo in case of Credit memo is not on invoice[Start]
      *IF gfSEEK(&lcCrMemHdr..Account+INVHDR.STORE+&lcCrMemLin..EMPLOYEE+lcSty+lcClr,'STYHIST')
      *  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003][Start]
      *IF gfSEEK(&lcCrMemHdr..Account+IIF(!EMPTY(&lcCrMemHdr..INVOICE),INVHDR.STORE,&lcCrMemHdr..STORE)+&lcCrMemLin..EMPLOYEE+lcSty+lcClr,'STYHIST')
      IF gfSEEK(&lcCrMemHdr..Account+IIF(!EMPTY(&lcCrMemHdr..INVOICE) AND INVHDR.CONSOL <> 'Y',INVHDR.STORE,&lcCrMemHdr..STORE)+&lcCrMemLin..EMPLOYEE+lcSty+lcClr,'STYHIST')
      *  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003][End]
        *  E302760,1 MMT 09/22/2010 use Store code from Credit memo in case of Credit memo is not on invoice[End]
        SELECT STYHIST
        *=gfSEEK('C'+PADR(&lcCrMemHdr..Account,8)+&lcCrMemHdr..Store,'Contact_A')
        *  E302760,1 MMT 09/22/2010 use Store code from Credit memo in case of Credit memo is not on invoice[Start]
        *=gfSEEK('C'+PADR(&lcCrMemHdr..Account,8)+INVHDR.STORE,'Contact_A')
        *  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003][Start]
        *=gfSEEK('C'+PADR(&lcCrMemHdr..Account,8)+IIF(!EMPTY(&lcCrMemHdr..INVOICE),INVHDR.STORE,&lcCrMemHdr..STORE),'Contact_A')
        =gfSEEK('C'+PADR(&lcCrMemHdr..Account,8)+IIF(!EMPTY(&lcCrMemHdr..INVOICE) AND INVHDR.CONSOL <> 'Y',INVHDR.STORE,&lcCrMemHdr..STORE),'Contact_A')
        *  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003][End]
        *  E302760,1 MMT 09/22/2010 use Store code from Credit memo in case of Credit memo is not on invoice[ENd]
        SELECT Contact_A
        *LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcCrMemHdr..Account,8)+&lcCrMemHdr..Store FOR;
        CCNTCTCODE = ALLTRIM(&lcCrMemLin..Employee)
        *  E302760,1 MMT 09/22/2010 use Store code from Credit memo in case of Credit memo is not on invoice[Start]
        *LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcCrMemHdr..Account,8)+INVHDR.STORE FOR;
        *    CCNTCTCODE = ALLTRIM(&lcCrMemLin..Employee)
        *  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003][Start]
*!*	        LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcCrMemHdr..Account,8)+IIF(!EMPTY(&lcCrMemHdr..INVOICE),INVHDR.STORE,&lcCrMemHdr..STORE) FOR;
*!*	          CCNTCTCODE = ALLTRIM(&lcCrMemLin..Employee)
        LOCATE REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT = 'C'+PADR(&lcCrMemHdr..Account,8)+IIF(!EMPTY(&lcCrMemHdr..INVOICE) AND INVHDR.CONSOL <> 'Y',INVHDR.STORE,&lcCrMemHdr..STORE) FOR;
          CCNTCTCODE = ALLTRIM(&lcCrMemLin..Employee)
        *  B610537,1 MMT 09/30/2013 Store in trnhist table is not updated while creating Credit Memo[T20130927.0003][End]
        *  E302760,1 MMT 09/22/2010 use Store code from Credit memo in case of Credit memo is not on invoice[End]
        *B608991,1 AHS 09/02/2009 Using the store of the invoice instead the store of credit memo[End]
        IF FOUND()
          =gfSEEK(Contact_A.UCODE+lcSty+lcClr,'UNIFORM')
          lnFactor = 1
          DO CASE
          CASE UNIFORM.TYPE = 'V'
            lnFactor = PRICE
          CASE UNIFORM.TYPE = 'P'
            lnFactor = UNIFORM.PNTSVAL
          ENDCASE
          SELECT STYHIST
          FOR lnI = 1 TO 8
            lcI = STR(lnI,1)
            REPLACE NUSED WITH NUSED - &lcCrMemLin..QTY&lcI*lnFactor
          ENDFOR
          =gfAdd_Info('STYHIST')
          =gfReplace('')
        ENDIF
      ENDIF
    ENDIF
    *B608948,1 MMT 07/28/2009 Update Styhist while creating and voiding CR[End]

    *E302590,1 MMT 03/31/2009 Update TrnHist file when credit Memo created [End]

    SELECT (lcCrMemLin)
    IF llLink_GL
      IF !oBusObj.GL_LINK.SEEK(&lcCrMemLin..Gl_Sales+'008')
        =oBusObj.GL_LINK.SEEK("DEFDEF"+'008')
      ENDIF

      REPLACE cCOGSAcnt WITH GL_LINK.GlAcnt

      IF !oBusObj.GL_LINK.SEEK(&lcCrMemLin..Gl_Cost+'006')
        =oBusObj.GL_LINK.SEEK("DEFDEF"+'006')
      ENDIF

      REPLACE cICAcnt   WITH GL_LINK.GlAcnt

      DECLARE laGLDistAr[2,13]
      laGLDistAr[1,1] = &lcCrMemLin..GL_Cost
      laGLDistAr[2,1] = &lcCrMemLin..GL_Sales
      laGLDistAr[1,10] = &lcCrMemLin..cICAcnt
      laGLDistAr[2,10] = &lcCrMemLin..cCOGSAcnt

      laGLDistAr[1,2] = '006'
      laGLDistAr[2,2] = '008'
      laGLDistAr[1,3] =  1
      laGLDistAr[2,3] = -1
      STORE 'RM' TO laGLDistAr[1,4],laGLDistAr[2,4]
      STORE &lcCrMemLin..CrMemo    TO laGLDistAr[1,5],laGLDistAr[2,5]
      STORE &lcCrMemHdr..dPostDate TO laGLDistAr[1,6],laGLDistAr[2,6]
      STORE lcGLFYear              TO laGLDistAr[1,7],laGLDistAr[2,7]
      STORE lcGlPeriod             TO laGLDistAr[1,8],laGLDistAr[2,8]
      STORE lcGLDstTmp             TO laGLDistAr[1,9],laGLDistAr[2,9]
    ELSE
      DIME laGLDistAr[1,1]
      laGLDistAr = ''
    ENDIF

    lnNxtStp = &lcCrMemLin..nSteps + 1

    SELECT (lcCrMemLin)
    DECLARE laAdjQty[9]
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      laAdjQty[lnCount] = Qty&lcCount
    ENDFOR
    laAdjQty[9] = TotQty

    IF &lcCrMemLin..nSteps < 6
      *--Call the global function for update style inventory control.
      PRIVATE lcRefer
      lcRefer = 'CUST# '+ Customer.Account + "-" + Customer.BTName
      lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , STYLE), ;
        &lcCrMemHdr..cWareCode,Dyelot,;
        CrDate,CrMemo,@laAdjQty,Cost,lcRefer,lcRmGlSess,'',;
        lnNxtStp,lcCrMemLin,'nSteps',@laGLDistAr,;
        VAL(&lcCrMemLin..cRet_LinNo))
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE &lcCrMemLin..nSteps WITH lnNxtStp
      UNLOCK
    ENDIF

    IF &lcCrMemLin..nSteps < lnNxtStp + 1
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE &lcCrMemLin..nSteps WITH lnNxtStp
      UNLOCK
    ENDIF
    IF &lcCrMemLin..nSteps < lnNxtStp + 2
      *-- Save the current line from the temp. file lines to
      *-- the master return line file.
      SELECT (lcCrMemLin)
      *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[START]
      lnRecoNo = RECNO()
      *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

      SCATTER MEMVAR MEMO
      SELECT (oBusObj.RETLINE.lcCursorUpdate)
      APPEND BLANK
      m.CrMemo = lcCrMemo
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      GATHER MEMVAR MEMO
      *-- Call global function to add audit fields info.
      *B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[Start]
      *=gfAdd_Info('RETLINE')
      =gfAdd_Info(oBusObj.RETLINE.lcCursorUpdate)
      *B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[END]
      UNLOCK

      *-- Call Global Function to transmit the local data.
      =gfTraceKey("RetLine" , CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD , "A")


      *-- If cancel the rest of open qty.
      IF llCancRest
        SELECT (lcCrMemLin)

        *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
        IF BETWEEN(lnRecoNo,1,RECCOUNT())
          GO RECORD lnRecoNo
        ENDIF
        *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

        IF Qty1 < nOpnQty1 .OR. Qty2 < nOpnQty2 .OR. Qty3 < nOpnQty3 .OR. ;
            Qty4 < nOpnQty4 .OR. Qty5 < nOpnQty5 .OR. Qty6 < nOpnQty6 .OR. ;
            Qty7 < nOpnQty7 .OR. Qty8 < nOpnQty8
          SCATTER MEMVAR MEMO
          *-- Create record in the R/A lines with status cancel has the
          *-- difference between the open qty. & returned qty.
          SELECT (oBusObj.RETLINE.lcCursorUpdate)
          APPEND BLANK
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          GATHER MEMVAR MEMO

          **  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
          *!*	          oBusObj.RETLINE.REPLACE('CRET_TRNCD WITH "4"')
          *!*	          oBusObj.RETLINE.REPLACE('CRET_LINNO WITH ALLTRIM(STR(lnLineCnt))')
          *!*	          oBusObj.RETLINE.REPLACE('QTY1       WITH &lcCrMemLin..nOpnQty1 - &lcCrMemLin..QTY1')
          *!*	          oBusObj.RETLINE.REPLACE('QTY2       WITH &lcCrMemLin..nOpnQty2 - &lcCrMemLin..QTY2')
          *!*	          oBusObj.RETLINE.REPLACE('QTY3       WITH &lcCrMemLin..nOpnQty3 - &lcCrMemLin..QTY3')
          *!*	          oBusObj.RETLINE.REPLACE('QTY4       WITH &lcCrMemLin..nOpnQty4 - &lcCrMemLin..QTY4')
          *!*	          oBusObj.RETLINE.REPLACE('QTY5       WITH &lcCrMemLin..nOpnQty5 - &lcCrMemLin..QTY5')
          *!*	          oBusObj.RETLINE.REPLACE('QTY6       WITH &lcCrMemLin..nOpnQty6 - &lcCrMemLin..QTY6')
          *!*	          oBusObj.RETLINE.REPLACE('QTY7       WITH &lcCrMemLin..nOpnQty7 - &lcCrMemLin..QTY7')
          *!*	          oBusObj.RETLINE.REPLACE('QTY8       WITH &lcCrMemLin..nOpnQty8 - &lcCrMemLin..QTY8')
          *!*	          oBusObj.RETLINE.REPLACE('TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8')
          REPLACE CRET_TRNCD WITH "4",;
            CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)),;
            QTY1       WITH &lcCrMemLin..nOpnQty1 - &lcCrMemLin..QTY1,;
            QTY2       WITH &lcCrMemLin..nOpnQty2 - &lcCrMemLin..QTY2,;
            QTY3       WITH &lcCrMemLin..nOpnQty3 - &lcCrMemLin..QTY3,;
            QTY4       WITH &lcCrMemLin..nOpnQty4 - &lcCrMemLin..QTY4,;
            QTY5       WITH &lcCrMemLin..nOpnQty5 - &lcCrMemLin..QTY5,;
            QTY6       WITH &lcCrMemLin..nOpnQty6 - &lcCrMemLin..QTY6,;
            QTY7       WITH &lcCrMemLin..nOpnQty7 - &lcCrMemLin..QTY7,;
            QTY8       WITH &lcCrMemLin..nOpnQty8 - &lcCrMemLin..QTY8,;
            TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8

          *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

          *-- Call global function to add audit fields info.
          =gfAdd_Info(oBusObj.RETLINE.lcCursorUpdate)
          UNLOCK

          *-- Call Global Function to transmit the local data.
          =gfTraceKey(oBusObj.RETLINE.lcCursorUpdate , CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD , "A")

          *-- Calculate the canceled qty.

          *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
          *lnTotCancl = lnTotCancl + RETLINE.TotQty
          lnTotCancl = lnTotCancl + TotQty
          *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[END]
        ENDIF
      ENDIF
      SELECT (lcCrMemLin)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE nSteps WITH lnNxtStp + 2
      UNLOCK
    ENDIF

    IF &lcCrMemLin..nSteps < lnNxtStp + 3
      IF !EMPTY(lcTrYear) .AND. BETWEEN(VAL(lcTrPeriod) , 1 , 13)
        lcTrPeriod = PADL(ALLTRIM(lcTrPeriod) , 2 , "0" )

        *-- Update the style history file for current line.
        SELECT ICSTYHST
        *!* B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][Begin]
        IF !OBusObj.ICSTYHST.SEEK(&lcCrMemLin..STYLE + lcTrYear)
          =gfOpenTable('ICSTYHST','STYHST','SH','ICSTYHSX')
          SELECT ICSTYHSX
          
          APPEND BLANK
          REPLACE STYLE WITH &lcCrMemLin..STYLE
          REPLACE CFISFYEAR  WITH lcTrYear
          = GFREPLACE('')
          =gfTABLEUPDATE()
          
          *USE IN ICSTYHSX
          =gfcloseTable('ICSTYHSX')
          SELECT ICSTYHST
        ENDIF
        *!* B610078,1 HIA 09/11/2012 A27 in closing programs, add records for styles with new year but not A40, so we need in A4, to insert record with style-year if not found in ICSTYHST[T20120813.0035][End]

        IF OBusObj.ICSTYHST.SEEK(&lcCrMemLin..STYLE + lcTrYear)
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          lnRetAmt = ROUND(&lcCrMemLin..Amount &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)
          lnDisAmt = ROUND(&lcCrMemLin..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)
          OBusObj.ICSTYHST.REPLACE('nRetAmt 		   WITH nRetAmt + lnRetAmt')
          OBusObj.ICSTYHST.REPLACE('nRetQty			   WITH nRetQty + &lcCrMemLin..TOTQTY')
          OBusObj.ICSTYHST.REPLACE('nDisAmt 		   WITH nDisAmt - lnDisAmt')
          OBusObj.ICSTYHST.REPLACE('nRetAmt&lcTrPeriod WITH nRetAmt&lcTrPeriod + lnRetAmt')
          OBusObj.ICSTYHST.REPLACE('nRetQty&lcTrPeriod WITH nRetQty&lcTrPeriod + &lcCrMemLin..TOTQTY')
          OBusObj.ICSTYHST.REPLACE('nDisAmt&lcTrPeriod WITH nDisAmt&lcTrPeriod - lnDisAmt')
          UNLOCK

          *-- Call Global Function to transmit the local data.
          =gfTraceKey("ICSTYHST" , &lcCrMemLin..STYLE + lcTrYear , "M")
        ENDIF
      ENDIF
      SELECT (lcCrMemLin)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE nSteps WITH lnNxtStp + 3
      UNLOCK
    ENDIF

    lnLineCnt  = lnLineCnt  + 1
    SELECT (lcCrMemLin)
  ENDSCAN

  IF !llPontSale
    SELECT (lcCrMemLin)
    SET ORDER TO TAG lcCurrtag
  ENDIF

  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  REPLACE nSteps WITH 6
  UNLOCK
ENDIF

IF &lcCrMemHdr..nSteps < 7
  *-- Updating General Ledger Distribution File the customer payment transaction.
  IF llLink_GL

    *-- Update the Gl distribution file with created entries.
    SELECT (lcGLDstTmp)

    REPLACE ALL GLSESSION WITH lcRmGlSess ;
      cAdd_User WITH oAriaApplication.User_ID  ;
      dAdd_Date WITH DATE()     ;
      cAdd_Time WITH gfGetTime()

    USE
    SELECT (oBusObj.GLDIST.lcCursorUpdate)
    APPEND FROM (oAriaApplication.WorkDir+lcGLDstTmp)

    *-- If called from point of sale program.
    IF llPontSale
      *-- Erase the temp. file that hold the gl distribution records.
      ERASE &oAriaApplication.WorkDir.&lcGLDstTmp..DBF
      ERASE &oAriaApplication.WorkDir.&lcGLDstTmp..CDX
    ELSE
      =lfCrtUnComp(.F. , .F. , .T.)
    ENDIF
  ENDIF

  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  REPLACE nSteps WITH 7
  UNLOCK
ENDIF

IF &lcCrMemHdr..nSteps < 8

  *-- Update [COMMISSIONS] Salesreps commissions / chargebacks
  SELECT SALESREP

  IF &lcCrMemHdr..CommPcnt1 + &lcCrMemHdr..CommPcnt2 > 0
    FOR lnCount = 1 TO 2
      lcCount = ALLTRIM(STR(lnCount))
      IF !EMPTY(&lcCrMemHdr..SalesRep&lcCount) .AND. &lcCrMemHdr..CommPcnt&lcCount > 0
        IF oBusObj.SALESREP.SEEK(&lcCrMemHdr..SalesRep&lcCount)
          *-- Calculated the equivalent amount.
          lnFrnAmnt = &lcCrMemHdr..CommAmt&lcCount

          lnCommDue = ROUND(&lcCrMemHdr..CommAmt&lcCount &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit , 2)
          lnNewBal = SALESREP.BALANCE  + lnCommDue
          SELECT SALESREP
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          oBusObj.SALESREP.REPLACE('CURRENT WITH CURRENT + lnCommDue')
          oBusObj.SALESREP.REPLACE('BALANCE WITH lnNewBal')

          UNLOCK
          *-- Call Global Function to transmit the local data.
          =gfTraceKey("SALESREP" , REPCODE , "M")

          IF &lcCrMemHdr..TotCredit > 0

            SELECT (oBusObj.REPCOMM.lcCursorUpdate)
            APPEND BLANK
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()

            *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
            *!*				oBusObj.REPCOMM.REPLACE('STATUS     WITH "O"')
            *!*				oBusObj.REPCOMM.REPLACE('REPCODE    WITH &lcCrMemHdr..SalesRep&lcCount')
            *!*				oBusObj.REPCOMM.REPLACE('ACCOUNT    WITH &lcCrMemHdr..Account')
            *!*				oBusObj.REPCOMM.REPLACE('ORDER      WITH &lcCrMemHdr..Order')
            *!*				oBusObj.REPCOMM.REPLACE('DATE       WITH &lcCrMemHdr..CrDate')
            *!*				oBusObj.REPCOMM.REPLACE('TRANTYPE   WITH "5"')
            *!*				oBusObj.REPCOMM.REPLACE('DESC       WITH "RETURN/I#"+&lcCrMemHdr..Invoice')
            *!*				oBusObj.REPCOMM.REPLACE('TRAN       WITH &lcCrMemHdr..CrMemo')
            *!*				oBusObj.REPCOMM.REPLACE('CUSTPO     WITH &lcCrMemHdr..CustPo')
            *!*				oBusObj.REPCOMM.REPLACE('COMMPCNT   WITH &lcCrMemHdr..CommPcnt&lcCount')
            *!*				oBusObj.REPCOMM.REPLACE('AMOUNT     WITH lnCommDue')
            *!*				oBusObj.REPCOMM.REPLACE('BALANCE    WITH lnNewBal')
            *!*				oBusObj.REPCOMM.REPLACE('cCurrCode  WITH &lcCrMemHdr..cCurrCode')
            *!*				oBusObj.REPCOMM.REPLACE('nExRate    WITH &lcCrMemHdr..nExRate')
            *!*				oBusObj.REPCOMM.REPLACE('nCurrUnit  WITH &lcCrMemHdr..nCurrUnit')
            *!*				oBusObj.REPCOMM.REPLACE('nForAmnt   WITH &lcCrMemHdr..CommAmt&lcCount')
            REPLACE STATUS     WITH "O",;
              REPCODE    WITH &lcCrMemHdr..SalesRep&lcCount,;
              ACCOUNT    WITH &lcCrMemHdr..Account,;
              ORDER      WITH &lcCrMemHdr..ORDER,;
              DATE       WITH &lcCrMemHdr..CrDate,;
              TRANTYPE   WITH "5",;
              DESC       WITH "RETURN/I#"+&lcCrMemHdr..Invoice,;
              TRAN       WITH &lcCrMemHdr..CrMemo,;
              CUSTPO     WITH &lcCrMemHdr..CustPo,;
              COMMPCNT   WITH &lcCrMemHdr..CommPcnt&lcCount,;
              AMOUNT     WITH lnCommDue,;
              BALANCE    WITH lnNewBal,;
              cCurrCode  WITH &lcCrMemHdr..cCurrCode,;
              nExRate    WITH &lcCrMemHdr..nExRate,;
              nCurrUnit  WITH &lcCrMemHdr..nCurrUnit,;
              nForAmnt   WITH &lcCrMemHdr..CommAmt&lcCount
            *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

            *-- Call global function to add audit fields info.
            *B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[Start]
            *=gfAdd_Info("REPCOMM")
            =gfAdd_Info(oBusObj.REPCOMM.lcCursorUpdate)
            *B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[END]
            UNLOCK

            *-- Call Global Function to transmit the local data.
            =gfTraceKey("REPCOMM" , REPCODE+DTOS(DATE)+TRAN+TRANTYPE , "A")
          ENDIF
        ENDIF
      ENDIF
    ENDFOR
  ENDIF
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  REPLACE nSteps WITH 8
  UNLOCK
ENDIF

*!*	*C102676,1 Custom process for A.S.T. [Begin]
*!*	IF ASCAN(laEvntTrig,PADR('MEMOREP3',10))<>0
*!*	  =gfDoTriger('RMCRMEM',PADR('MEMOREP3',10))
*!*	ENDIF
*!*	*C102676,1 Custom process for A.S.T. [End]

IF &lcCrMemHdr..nSteps < 9
  *-- Update Return Authorization.
  IF !EMPTY(&lcCrMemHdr..RaNo)
    SELECT RALINE
    *-- Set order descending into the RALINE file to get the last line.
    SET ORDER TO RALINE DESCENDING
    *-- Get the last line in the R/A.
    lnRaCnt = IIF(oBusObj.RALINE.SEEK(&lcCrMemHdr..RaNo) , VAL(RALINE.cRa_LinNo) + 1 , 1)
    SET ORDER TO RALINE ASCENDING
    lnTotDedct = 0    && Var. hold the total difference between open & received
    lnAmtDedct = 0    && Var. hold the total amount of difference between open & received
    lnTotRecvd = 0    && Var. hold total received qty.
    lcSaveFile = lcCrMemLin
    lcCrMemLin = lcOpenLine
    SELECT (lcCrMemLin)
    SCAN
      *-- If this line related to the selected invoice & not related to
      *-- the selected R/A, Add this line with zero qty. in RALINE file.
      IF &lcCrMemLin..lInvoice .AND. ;
          !oBusObj.RALINE.SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..STYLE)
        SELECT (oBusObj.RALINE.lcCursorUpdate)
        APPEND BLANK
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()

        *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
        *!*	        oBusObj.RALINE.REPLACE('Rano      WITH &lcCrMemHdr..RaNo')
        *!*	        oBusObj.RALINE.REPLACE('Account   WITH &lcCrMemHdr..Account')
        *!*	        oBusObj.RALINE.REPLACE('Style     WITH &lcCrMemLin..Style')
        *!*	        oBusObj.RALINE.REPLACE('cRa_LinNo WITH ALLTRIM(STR(lnRaCnt))')
        *!*	        oBusObj.RALINE.REPLACE('Dyelot    WITH &lcCrMemLin..Dyelot')
        *!*	        oBusObj.RALINE.REPLACE('Reason    WITH &lcCrMemHdr..Reason')
        *!*	        oBusObj.RALINE.REPLACE('Price     WITH &lcCrMemLin..Price')
        *!*	        oBusObj.RALINE.REPLACE('Tax_Rate  WITH &lcCrMemLin..Tax_Rate')
        *!*	        oBusObj.RALINE.REPLACE('nPstRate  WITH &lcCrMemLin..nPstRate')
        REPLACE Rano      WITH &lcCrMemHdr..RaNo,;
          Account   WITH &lcCrMemHdr..Account,;
          STYLE     WITH &lcCrMemLin..STYLE,;
          cRa_LinNo WITH ALLTRIM(STR(lnRaCnt)),;
          Dyelot    WITH &lcCrMemLin..Dyelot,;
          Reason    WITH &lcCrMemHdr..Reason,;
          Price     WITH &lcCrMemLin..Price,;
          Tax_Rate  WITH &lcCrMemLin..Tax_Rate,;
          nPstRate  WITH &lcCrMemLin..nPstRate
        *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

        *-- Call global function to add audit fields info.
        *B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[Start]
        *=gfAdd_Info('RALINE')
        =gfAdd_Info(oBusObj.RALINE.lcCursorUpdate)
        *B609896,1 MMT 04/24/2012 Fix bug of not updating user information in RETALINE Table[Start]
        UNLOCK

        *-- Call Global Function to transmit the local data.
        =gfTraceKey("RALINE" , RANO+STYLE+CRA_LINNO , "A")

        *-- Increment the line no. in the raline file.
        lnRaCnt = lnRaCnt + 1
      ENDIF

      DO CASE
        *-- If received line.
      CASE &lcCrMemLin..cRet_TrnCd = "2"
        IF oBusObj.RALINE.SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..STYLE)
          SELECT RALINE
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          oBusObj.RALINE.REPLACE('nOpnQty1 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty1-&lcCrMemLin..Qty1 , 0))')
          oBusObj.RALINE.REPLACE('nOpnQty2 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty2-&lcCrMemLin..Qty2 , 0))')
          oBusObj.RALINE.REPLACE('nOpnQty3 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty3-&lcCrMemLin..Qty3 , 0))')
          oBusObj.RALINE.REPLACE('nOpnQty4 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty4-&lcCrMemLin..Qty4 , 0))')
          oBusObj.RALINE.REPLACE('nOpnQty5 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty5-&lcCrMemLin..Qty5 , 0))')
          oBusObj.RALINE.REPLACE('nOpnQty6 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty6-&lcCrMemLin..Qty6 , 0))')
          oBusObj.RALINE.REPLACE('nOpnQty7 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty7-&lcCrMemLin..Qty7 , 0))')
          oBusObj.RALINE.REPLACE('nOpnQty8 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty8-&lcCrMemLin..Qty8 , 0))')
          oBusObj.RALINE.REPLACE('nTotOpnQty WITH nOpnQty1 + nOpnQty2 + nOpnQty3 + nOpnQty4 + nOpnQty5 + nOpnQty6 + nOpnQty7 + nOpnQty8')
          UNLOCK
          FOR lnCount = 1 TO 8
            lcCount = ALLTRIM(STR(lnCount))
            lnTotDedct = lnTotDedct + MIN(&lcCrMemLin..nOpnQty&lcCount , &lcCrMemLin..Qty&lcCount)
            lnAmtDedct = lnAmtDedct + (MIN(&lcCrMemLin..nOpnQty&lcCount , &lcCrMemLin..Qty&lcCount) * &lcCrMemLin..Price)
            lnTotRecvd = lnTotRecvd + &lcCrMemLin..Qty&lcCount
          ENDFOR

          *-- Call Global Function to transmit the local data.
          =gfTraceKey("RALINE" , RANO+STYLE+CRA_LINNO , "M")
        ENDIF
        *-- If canceled line.
      CASE &lcCrMemLin..cRet_TrnCd = "4"
        lnTotCancl = lnTotCancl + &lcCrMemLin..TotQty
      ENDCASE
      SELECT (lcCrMemLin)
    ENDSCAN

    lcCrMemLin = lcSaveFile

    *-- Update the RA header file with the changed fields.
    SELECT RETAUTH
    oBusObj.RETAUTH.SETORDER('RETAUTHA')
    *B608142,1 MMT 06/26/2007 fix bug of not updating RA status[Start]
    *IF SEEK(&lcCrMemHdr..Account + &lcCrMemHdr..RaNo)
    IF oBusObj.RETAUTH.SEEK(&lcCrMemHdr..Account + &lcCrMemHdr..RaNo)
      *B608142,1 MMT 06/26/2007 fix bug of not updating RA status[End]

      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      oBusObj.RETAUTH.REPLACE('INVOICE    WITH &lcCrMemHdr..Invoice')
      oBusObj.RETAUTH.REPLACE('TRAN       WITH &lcCrMemHdr..CrMemo')
      oBusObj.RETAUTH.REPLACE('ORDER      WITH &lcCrMemHdr..Order')
      oBusObj.RETAUTH.REPLACE('CUSTPO     WITH &lcCrMemHdr..CustPo')
      oBusObj.RETAUTH.REPLACE('RETDATE    WITH &lcCrMemHdr..CrDate')
      oBusObj.RETAUTH.REPLACE('RETURN     WITH RETURN + &lcCrMemHdr..Pieces')
      oBusObj.RETAUTH.REPLACE('RETURNAMT  WITH RETURNAMT + &lcCrMemHdr..Gross_Amt')
      oBusObj.RETAUTH.REPLACE('cCurrCode  WITH &lcCrMemHdr..cCurrCode')
      oBusObj.RETAUTH.REPLACE('nExRate    WITH &lcCrMemHdr..nExRate')
      oBusObj.RETAUTH.REPLACE('nCurrUnit  WITH &lcCrMemHdr..nCurrUnit')

      oBusObj.RETAUTH.REPLACE('nreta_opn  WITH IIF(llCancRest , 0 , MAX(nreta_opn-lnTotDedct , 0))')
      oBusObj.RETAUTH.REPLACE('nreta_rec  WITH nreta_rec + lnTotRecvd')
      oBusObj.RETAUTH.REPLACE('nreta_can  WITH nreta_can + lnTotCancl')
      oBusObj.RETAUTH.REPLACE('nRtOpnAmt  WITH IIF(llCancRest , 0 , MAX(nRtOpnAmt-lnAmtDedct , 0))')

      oBusObj.RETAUTH.REPLACE('STATUS     WITH IIF(llCancRest .OR. nreta_opn = 0 , "C" , STATUS)')
      UNLOCK

      *-- Call Global Function to transmit the local data.
      =gfTraceKey("RETAUTH" , RANO , "M")
    ENDIF
    oBusObj.RETAUTH.SETORDER('RETAUTH')
  ENDIF
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  REPLACE nSteps WITH 9
  UNLOCK
ENDIF

*-- Post customer's A/R
llPost = IIF(llPostfInv .AND. !EMPTY(&lcCrMemHdr..cFacCode) , .F. , .T.)

IF llPost .AND. &lcCrMemHdr..TotCredit <> 0
  IF &lcCrMemHdr..nSteps < 10

    lnTotCredit = ABS(&lcCrMemHdr..TotCredit) * -1

    *-- Add record in the credit file for the total credit amount.
    SELECT (oBusObj.CREDIT.lcCursorUpdate)
    APPEND BLANK
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()

    *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
    *!*	    oBusObj.CREDIT.REPLACE('TRAN       WITH &lcCrMemHdr..CrMemo ')
    *!*	    oBusObj.CREDIT.REPLACE('cFacCode   WITH &lcCrMemHdr..cFacCode ')
    *!*	    oBusObj.CREDIT.REPLACE('ACCOUNT    WITH &lcCrMemHdr..Account ')
    *!*	    oBusObj.CREDIT.REPLACE('TRANDATE   WITH &lcCrMemHdr..CrDate ')
    *!*	    oBusObj.CREDIT.REPLACE('dPostDate  WITH &lcCrMemHdr..dPostDate')
    *!*	    oBusObj.CREDIT.REPLACE('TRANTYPE   WITH "0" ')
    *!*	    oBusObj.CREDIT.REPLACE('CCREDITCOD WITH &lcCrMemHdr..Reason ')
    *!*	    oBusObj.CREDIT.REPLACE('DESC       WITH "RET.INV#"+&lcCrMemHdr..Invoice ')
    *!*	    oBusObj.CREDIT.REPLACE('STORE      WITH &lcCrMemHdr..Store ')
    *!*	    oBusObj.CREDIT.REPLACE('DSC_AMT    WITH ABS(lnTTrdeAmt) * -1 ')
    *!*	    oBusObj.CREDIT.REPLACE('cCurrCode  WITH &lcCrMemHdr..cCurrCode')
    *!*	    oBusObj.CREDIT.REPLACE('nExRate    WITH &lcCrMemHdr..nExRate ')
    *!*	    oBusObj.CREDIT.REPLACE('nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ')
    *!*	    oBusObj.CREDIT.REPLACE('AMOUNT     WITH lnTotCredit ')
    *!*	    oBusObj.CREDIT.REPLACE('cArGlAcc   WITH &lcCrMemHdr..cArAcnt')
    *B608142,1 MMT 06/26/2007 fix bug of not updating RA status[Start]
    *!*	    	REPLACE TRAN       WITH &lcCrMemHdr..CrMemo,;
    *!*		   			cFacCode   WITH &lcCrMemHdr..cFacCode ,;
    *!*	    			ACCOUNT    WITH &lcCrMemHdr..Account ,;
    *!*	    			TRANDATE   WITH &lcCrMemHdr..CrDate ,;
    *!*	    			dPostDate  WITH &lcCrMemHdr..dPostDate,;
    *!*				   	TRANTYPE   WITH "0" ,;
    *!*					CCREDITCOD WITH &lcCrMemHdr..Reason ,;
    *!*				    DESC       WITH "RET.INV#"+&lcCrMemHdr..Invoice ,;
    *!*	  				STORE      WITH &lcCrMemHdr..Store,;
    *!*					DSC_AMT    WITH ABS(lnTTrdeAmt) * -1 ,;
    *!*	   				cCurrCode  WITH &lcCrMemHdr..cCurrCode,;
    *!*	  			    nExRate    WITH &lcCrMemHdr..nExRate ,;
    *!*	    			nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ,;
    *!*	 				AMOUNT     WITH lnTotCredit ,;
    *!*	    			cArGlAcc   WITH &lcCrMemHdr..cArAcnt

    REPLACE TRAN       WITH IIF(!EMPTY(&lcCrMemHdr..CrMemo),&lcCrMemHdr..CrMemo,lcCrMemo),;
      cFacCode   WITH &lcCrMemHdr..cFacCode ,;
      ACCOUNT    WITH &lcCrMemHdr..Account ,;
      TRANDATE   WITH &lcCrMemHdr..CrDate ,;
      dPostDate  WITH &lcCrMemHdr..dPostDate,;
      TRANTYPE   WITH "0" ,;
      CCREDITCOD WITH &lcCrMemHdr..Reason ,;
      DESC       WITH "RET.INV#"+&lcCrMemHdr..Invoice ,;
      STORE      WITH &lcCrMemHdr..STORE,;
      DSC_AMT    WITH ABS(lnTTrdeAmt) * -1 ,;
      cCurrCode  WITH &lcCrMemHdr..cCurrCode,;
      nExRate    WITH &lcCrMemHdr..nExRate ,;
      nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ,;
      AMOUNT     WITH lnTotCredit ,;
      cArGlAcc   WITH &lcCrMemHdr..cArAcnt



    *B608142,1 MMT 06/26/2007 fix bug of not updating RA status[End]
    *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

    *-- Call global function to add audit fields info.
    =gfAdd_Info(oBusObj.CREDIT.lcCursorUpdate)

    *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
    *!*	    oBusObj.CREDIT.REPLACE('cEdit_User  WITH oAriaApplication.User_ID')
    *!*	    oBusObj.CREDIT.REPLACE('dEdit_Date  WITH DATE()')
    *!*	    oBusObj.CREDIT.REPLACE('cEdit_Time  WITH gfGetTime()')
    *!*	    oBusObj.CREDIT.REPLACE('cEdt_Ver    WITH "A40"')
    SELECT (oBusObj.CREDIT.lcCursorUpdate)
    REPLACE cEdit_User  WITH oAriaApplication.User_ID,;
      dEdit_Date  WITH DATE(),;
      cEdit_Time  WITH gfGetTime(),;
      cEdt_Ver    WITH "A40"
    *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

    IF !EMPTY(&lcCrMemHdr..RaNo) .AND. EMPTY(&lcCrMemHdr..REFERENCE)
      *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
      *!*	 	  oBusObj.CREDIT.REPLACE("REFERENCE WITH 'R/A# '+&lcCrMemHdr..RaNo")
      REPLACE REFERENCE WITH 'R/A# '+&lcCrMemHdr..RaNo
      *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]
    ELSE
      IF EMPTY(&lcCrMemHdr..RaNo) .AND. !EMPTY(&lcCrMemHdr..REFERENCE)
        *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
        *!*		      oBusObj.CREDIT.REPLACE('REFERENCE WITH &lcCrMemHdr..Reference')
        REPLACE REFERENCE WITH &lcCrMemHdr..REFERENCE
        *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

      ELSE
        IF !EMPTY(&lcCrMemHdr..RaNo) .AND. !EMPTY(&lcCrMemHdr..REFERENCE)
          *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
          *!*	          oBusObj.CREDIT.REPLACE("REFERENCE WITH 'RA'+&lcCrMemHdr..RaNo+'/'+&lcCrMemHdr..Reference")
          REPLACE REFERENCE WITH 'RA'+&lcCrMemHdr..RaNo+'/'+&lcCrMemHdr..REFERENCE
          *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

        ELSE

          *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[Start]
          *oBusObj.CREDIT.REPLACE('REFERENCE WITH " "')
          REPLACE REFERENCE WITH " "
          *  B608074,1 05/06/2007 MMT fix bug of not saving CR details[End]

        ENDIF
      ENDIF
    ENDIF
    *E304024,1 MMT 06/27/2018 Add trigger to credit memo saving program to update reference[T20180619.0002][Start]
    IF ASCAN(oBusObj.PARENT.PARENT.laEvntTrig , PADR('UPCRDTREF',10)) <> 0
      =oBusObj.PARENT.PARENT.mDoTrigger(PADR('UPCRDTREF',10))
    ENDIF
    *E304024,1 MMT 06/27/2018 Add trigger to credit memo saving program to update reference[T20180619.0002][End]
    UNLOCK

    *-- Call Global Function to transmit the local data.
    =gfTraceKey(oBusObj.CREDIT.lcCursorUpdate , TRANTYPE+TRAN , "A")

    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE nSteps WITH 10
    UNLOCK
  ENDIF

  IF &lcCrMemHdr..nSteps < 11
    *-- Compute new aged A/R applying credits to oldest balance first
    *-- Return new customer a/r balance.
    SELECT CUSTOMER
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *-- Calculated the equivalent amount.
    oBusObj.CUSTOMER.REPLACE('OPENCR  WITH OPENCR + ROUND((ABS(&lcCrMemHdr..TotCredit) * -1) &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)')
    oBusObj.CUSTOMER.REPLACE('NETBAL     WITH TOTAGE + OPENCR')
    oBusObj.CUSTOMER.REPLACE('nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)')


    UNLOCK
    
    *  B609951,1 SAB 05/30/2012 Update credit memo on store level [T20120301.0003][Start]
    **! C201483,1 SAB 05/13/2012 New Customization to update site budget info on customer file[T20111206.0020][Start]
    **-- Add Trigger to Update Customer Site Budget info
    *IF ASCAN(oBusObj.PARENT.PARENT.laEvntTrig , PADR('SBSAVCRMEM',10)) <> 0
    *  =oBusObj.PARENT.PARENT.mDoTrigger(PADR('SBSAVCRMEM',10))
    *ENDIF
    **! C201483,1 SAB 05/13/2012 New Customization to update site budget info on customer file[T20111206.0020][End]
    *  B609951,1 SAB 05/30/2012 Update credit memo on store level [T20120301.0003][End]

    *-- Call Global Function to transmit the local data.
    =gfTraceKey("CUSTOMER" , "M"+Account+STORE , "M")

    *  B609951,1 SAB 05/30/2012 Update credit memo on store level [T20120301.0003][Start]
    *-- Add Trigger to Update Customer Site Budget info
    IF ASCAN(oBusObj.PARENT.PARENT.laEvntTrig , PADR('SBSAVCRMEM',10)) <> 0
      =oBusObj.PARENT.PARENT.mDoTrigger(PADR('SBSAVCRMEM',10))
    ENDIF
    *  B609951,1 SAB 05/30/2012 Update credit memo on store level [T20120301.0003][End]

    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE nSteps WITH 11
    UNLOCK
  ENDIF
ENDIF

IF &lcCrMemHdr..nSteps < 12
  IF !EMPTY(lcTrYear) .AND. BETWEEN(VAL(lcTrPeriod) , 1 , 13)

    lcTrPeriod = PADL(ALLTRIM(lcTrPeriod) , 2 , "0" )
    *-- Update the customer history file.
    SELECT ARCUSHST
    IF oBusObj.ARCUSHST.SEEK(&lcCrMemHdr..Account + lcTrYear)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      lnRetAmt =  ROUND(&lcCrMemHdr..Gross_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)
      lnDisAmt = ROUND(&lcCrMemHdr..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)
      oBusObj.ARCUSHST.REPLACE('nRetAmt            WITH nRetAmt + lnRetAmt')
      oBusObj.ARCUSHST.REPLACE('nRetQty            WITH nRetQty + &lcCrMemHdr..Pieces')
      oBusObj.ARCUSHST.REPLACE('nDisAmt            WITH nDisAmt - lnDisAmt  ')
      oBusObj.ARCUSHST.REPLACE('nRetAmt&lcTrPeriod WITH nRetAmt&lcTrPeriod + lnRetAmt ')
      oBusObj.ARCUSHST.REPLACE('nRetQty&lcTrPeriod WITH nRetQty&lcTrPeriod + &lcCrMemHdr..Pieces ')
      oBusObj.ARCUSHST.REPLACE('nDisAmt&lcTrPeriod WITH nDisAmt&lcTrPeriod - lnDisAmt')
      UNLOCK

      *-- Call Global Function to transmit the local data.
      =gfTraceKey("ARCUSHST" , &lcCrMemHdr..Account + lcTrYear , "M")
    ENDIF
  ENDIF
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  REPLACE nSteps WITH 12
  UNLOCK
ENDIF

*E302590,1 MMT 03/31/2009 Update TrnHist file when credit Memo created [Start]
IF USED('TRNHIST')
  SELECT TRNHIST
  =gfTableUpdate()
ENDIF
*B608948,1 MMT 07/28/2009 Update Styhist while creating and voiding CR[Start]
IF USED('STYHIST')
  SELECT STYHIST
  =gfTableUpdate()
ENDIF
*B608948,1 MMT 07/28/2009 Update Styhist while creating and voiding CR[End]
*E302590,1 MMT 03/31/2009 Update TrnHist file when credit Memo created [End]

DIMENSION laTableUpdate[13]
laTableUpdate[1]  = oBusObj.RetLine
laTableUpdate[2]  = oBusObj.RETHDR
laTableUpdate[3]  = oBusObj.STYLE
laTableUpdate[4]  = oBusObj.Stydye
laTableUpdate[5]  = oBusObj.ICSTYHST
laTableUpdate[6]  = oBusObj.ARCUSHST
laTableUpdate[7]  = oBusObj.CUSTOMER
laTableUpdate[8]  = oBusObj.GLDIST
laTableUpdate[9]  = oBusObj.SALESREP
laTableUpdate[10] = oBusObj.REPCOMM
laTableUpdate[11] = oBusObj.RALINE
laTableUpdate[12] = oBusObj.RETAUTH
laTableUpdate[13] = oBusObj.CREDIT

IF !lfTableUpdate()
  RETURN .F.
ENDIF


*!*************************************************************
*! Name      : lfTableUpdate
*! Developer : Hend Ghanem
*! Date      : 11/07/2004
*! Purpose   : function to Update Sql Tables.
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
FUNCTION lfTableUpdate

*--Open Dictionary files.
LOCAL lnAlias,lnConnectionHandlar,lcTranCode,lnI,llUpdate
lnAlias = SELECT(0)

lcTranCode = oAriaApplication.RemoteCompanyData.BeginTran(oAriaApplication.ActiveCompanyConStr,3,'')
IF TYPE('lcTranCode') = 'N'
  SELECT (lnAlias)
  RETURN .F.
ENDIF

FOR lnI = 1 TO ALEN(laTableUpdate,1)

  *WSH [Start]
  IF TYPE("laTableUpdate[lnI]") = "O"
    *WSH [End]

    llUpdate = laTableUpdate[lnI].TABLEUPDATE(lcTranCode)
    IF !llUpdate
      =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
      SELECT (lnAlias)
      RETURN .F.
    ENDIF

    *WSH [Start]
  ENDIF
  *WSH [End]

ENDFOR

lnConnectionHandlar = oAriaApplication.RemoteCompanyData.CommitTran(lcTranCode)
IF lnConnectionHandlar # 1
  =oAriaApplication.RemoteCompanyData.RollBackTran(lcTranCode)
  SELECT(lnAlias)
  RETURN .F.
ENDIF

SELECT(lnAlias)
*--end of lfTableUpdate.

*!*************************************************************
*! Name      : lfGetTrdDs
*! Developer : Hend Ghanem
*! Date      : 04/28/2005
*! Purpose   : Local function to get the trade discount.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lcCurTerm -> Term Code
*!             lcCurInv  -> Invoice #
*!*************************************************************
*! Returns   : lnInvTrdDs
*!*************************************************************
*! Example   :  lnInvTrdDs = lfGetTrdDs(lcCurTerm , lcCurInv)
*!*************************************************************
FUNCTION lfGetTrdDs
PARAMETERS lcCurTerm , lcCurInv , oBusClss

PRIVATE lcCurTerm , lcCurInv

lnInvTrdDs = 0

*-- If there is invoice #.
IF !EMPTY(lcCurInv)
  *-- If the terms used in the invoice changed.
  IF !(lcCurTerm == oBusClss.lcInvTerms)
    *-- Get the trade discount from the terms related fields in the codes file.
    IF !EMPTY(lcCurTerm)
      *-- Get the trade discount for the current term.
      DECLARE laTrmRltFd[1,2]
      laTrmRltFd[1,1] = 'NTERDISCR'
      laTrmRltFd[1,2] = 'lnInvTrdDs'
      =gfRltFld(lcCurTerm , @laTrmRltFd , "CTERMCODE")
    ELSE
      lnInvTrdDs = 0
    ENDIF
  ELSE
    *-- If the terms code used in the invoice does not change, get the
    *-- trade discount saved in the invoice header file.
    lnInvTrdDs = IIF(oBusClss.INVHDR.SEEK(lcCurInv) , InvHdr.Trde_Disc , 0)
  ENDIF
ELSE
  *-- If there is no invoice # , use the trade discount from the related
  *-- fields in the codes file for the used terms code.
  *-- If not empty of the terms field.
  IF !EMPTY(lcCurTerm)
    *-- Get the trade discount for the current term.
    DECLARE laTrmRltFd[1,2]
    laTrmRltFd[1,1] = 'NTERDISCR'
    laTrmRltFd[1,2] = 'lnInvTrdDs'
    =gfRltFld(lcCurTerm , @laTrmRltFd , "CTERMCODE")
  ELSE
    lnInvTrdDs = 0
  ENDIF
ENDIF
oBusClss.lnInvTrdDs = lnInvTrdDs
RETURN lnInvTrdDs

*!*************************************************************
*! Name      : lfCrtUnComp
*! Developer : Hend Ghanem
*! Date      : 04/28/2005
*! Purpose   : Create all the needed temp. files.
*!*************************************************************
*! Calls     : gfCrtTmp
*!*************************************************************
*! Parameters: llHeader  -> Flag to know if recreate header file.
*!             llDetails -> Flag to know if recreate detail file.
*!*************************************************************
*! Returns   :  None.
*!*************************************************************
*! Example   :  =lfCrtUnComp()
*!*************************************************************
FUNCTION lfCrtUnComp
PARAMETERS llHeader , llDetails , llGLDist , llOpenLine , llFromEDI

PRIVATE llHeader , llDetails , llGLDist , llOpenLine

*-- If parameter send to create the temp. header file, create it from the
*-- master Return header file.
IF llHeader
  SELECT RETHDR
  =AFIELDS(laCrMemHdr)
  lnCrMemHdr = ALEN(laCrMemHdr , 1)
  *-- Add field to know at which step the saving has done.
  DIMENSION laCrMemHdr [lnCrMemHdr+1,18]
  laCrMemHdr[lnCrMemHdr + 1 , 1] = 'NSTEPS'
  laCrMemHdr[lnCrMemHdr + 1 , 2] = 'N'
  laCrMemHdr[lnCrMemHdr + 1 , 3] = 6
  laCrMemHdr[lnCrMemHdr + 1 , 4] = 0
  FOR lnI = 1 TO ALEN(laCrMemHdr,1)
    STORE '' TO laCrMemHdr[lnI ,7],laCrMemHdr[lnI ,8],laCrMemHdr[lnI ,9],;
      laCrMemHdr[lnI ,10],laCrMemHdr[lnI ,11],laCrMemHdr[lnI ,12],;
      laCrMemHdr[lnI ,13],laCrMemHdr[lnI ,14],laCrMemHdr[lnI ,15],;
      laCrMemHdr[lnI ,16]
    STORE 0  TO laCrMemHdr[lnI ,17],  laCrMemHdr[lnI ,18]
  ENDFOR
  *-- Call global function to create the temp. header file.
  =gfCrtTmp(lcCrMemHdr , @laCrMemHdr , [ACCOUNT] , lcCrMemHdr)
ENDIF


*-- Temp. name for the lines file.
SELECT RETLINE
=AFIELDS(laCrMemLin)
lnCrMemLin = ALEN(laCrMemLin , 1)

*!B610380,1 HIA 06/13/13 T20130509.0007 - Gross profit invoiced report [Begin]
*DIMENSION laCrMemLin [lnCrMemLin+13,18]
DIMENSION laCrMemLin [lnCrMemLin+15,18]
*!B610380,1 HIA 06/13/13 T20130509.0007 - Gross profit invoiced report [End]

FOR lnCount = 1 TO 8
  lcCount = ALLTRIM(STR(lnCount))
  laCrMemLin[lnCrMemLin + lnCount , 1] = 'NOPNQTY&lcCount'
  laCrMemLin[lnCrMemLin + lnCount , 2] = 'N'
  laCrMemLin[lnCrMemLin + lnCount , 3] = 6
  laCrMemLin[lnCrMemLin + lnCount , 4] = 0
ENDFOR
*-- Add field for total open qty. to the lines file.
laCrMemLin[lnCrMemLin + 9 , 1] = 'NTOTOPNQTY'
laCrMemLin[lnCrMemLin + 9 , 2] = 'N'
laCrMemLin[lnCrMemLin + 9 , 3] = 6
laCrMemLin[lnCrMemLin + 9 , 4] = 0

*-- Add logical field to know if we need to add the valid lines of the
*-- selected invoice in the RA line file.
laCrMemLin[lnCrMemLin + 10 , 1] = 'LINVOICE'
laCrMemLin[lnCrMemLin + 10 , 2] = 'L'
laCrMemLin[lnCrMemLin + 10 , 3] = 1
laCrMemLin[lnCrMemLin + 10 , 4] = 0

*-- Add field to know at which step the saving has done.
laCrMemLin[lnCrMemLin + 11 , 1] = 'NSTEPS'
laCrMemLin[lnCrMemLin + 11 , 2] = 'N'
laCrMemLin[lnCrMemLin + 11 , 3] = 6
laCrMemLin[lnCrMemLin + 11 , 4] = 0

laCrMemLin[lnCrMemLin + 12 , 1] = 'LSHIPPED'
laCrMemLin[lnCrMemLin + 12 , 2] = 'L'
laCrMemLin[lnCrMemLin + 12 , 3] = 0
laCrMemLin[lnCrMemLin + 12 , 4] = 0

laCrMemLin[lnCrMemLin + 13 , 1] = 'cReason'
laCrMemLin[lnCrMemLin + 13 , 2] = 'C'
laCrMemLin[lnCrMemLin + 13 , 3] = 60
laCrMemLin[lnCrMemLin + 13 , 4] = 0

*!B610380,1 HIA 06/13/13 T20130509.0007 - Gross profit invoiced report [Begin]

laCrMemLin[lnCrMemLin + 14 , 1] = 'tmpPrice'
laCrMemLin[lnCrMemLin + 14 , 2] = 'N'
laCrMemLin[lnCrMemLin + 14 , 3] = 12
laCrMemLin[lnCrMemLin + 14 , 4] = 2


laCrMemLin[lnCrMemLin + 15 , 1] = 'tmpDiscLP'
laCrMemLin[lnCrMemLin + 15 , 2] = 'N'
laCrMemLin[lnCrMemLin + 15 , 3] = 12
laCrMemLin[lnCrMemLin + 15 , 4] = 2

*!B610380,1 HIA 06/13/13 T20130509.0007 - Gross profit invoiced report [End]


*T20060818.0001(C200876) TMI [Start] Add the locbin fields to the temp file
IF TYPE('loFormSet')='O' .AND. ASCAN(loFormSet.laEvntTrig,PADR('ADDRMFLD',10)) <> 0
  =loFormSet.mDoTrigger(PADR('ADDRMFLD',10))
ENDIF
*T20060818.0001(C200876) TMI [End  ]


FOR lnI = 1 TO ALEN(laCrMemLin,1)
  STORE '' TO laCrMemLin[lnI ,7],laCrMemLin[lnI ,8],laCrMemLin[lnI ,9],;
    laCrMemLin[lnI ,10],laCrMemLin[lnI ,11],laCrMemLin[lnI ,12],;
    laCrMemLin[lnI ,13],laCrMemLin[lnI ,14],laCrMemLin[lnI ,15],;
    laCrMemLin[lnI ,16]
  STORE 0  TO laCrMemLin[lnI ,17],  laCrMemLin[lnI ,18]
ENDFOR
*-- If parameter send to create the temp. lines file, create it from the
*-- master Return lines file.
IF llDetails
  *-- Call global function to create the temp. lines file.
  DECLARE laIndexTag[2,2]
  laIndexTag[1,1] = [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD]
  laIndexTag[1,2] = lcCrMmStyl
  laIndexTag[2,1] = [ACCOUNT+STR(VAL(CRET_LINNO),4)+STYLE]
  laIndexTag[2,2] = lcCrMmLine
  =gfCrtTmp(lcCrMemLin , @laCrMemLin , @laIndexTag )
ENDIF

IF llOpenLine
  =gfCrtTmp(lcOpenLine , @laCrMemLin , [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD] , lcOpenLine)
ENDIF

IF llLink_GL .AND. llGLDist
  SELECT GLDIST
  =AFIELDS(laGLDstTmp)
  *-- Call global function to create the temp. header file.
  =gfCrtTmp(lcGLDstTmp , @laGLDstTmp , [GLaccount] , lcGLDstTmp)
ENDIF

*:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[Start]
*!*************************************************************
*! Name      : lfChkStatHst
*! Developer : Mariam Mazhar
*! Date      : 06/17/2009
*! Purpose   : Check if State is HST Tax
*!*************************************************************
FUNCTION lfChkStatHst
PARAMETERS lcCustState
DIMENSION laTaxHstRl[1,2]
laTaxHstRl[1,1] = "LHSTTAX"
laTaxHstRl[1,2] = "llISHSt"
STORE .F. TO llISHSt
*-- Fill the related GL information from the codes file.
llNoThing = gfRltFld(lcCustState, @laTaxHstRl, "STATE")
RETURN llISHSt
*:* E302618,1 MMT 06/17/2009 Call GLDIST For GL Category 029,030 For Candian Companies[End]