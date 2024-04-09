*!**********************************************************************************************
*! Name      : lfSavRet
*! Developer : Reham Al-Allamy
*! Date      : 02/26/1998
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
*! Modifications :
*! E301490,1 AHM,1 Copy save function to separate program
*! B604036,1 KAM 25/12/2000 change replace amount with calculated amount
*! C102212,1 ADEL 03/14/20001 Add Canadian HST tax to setup and add it on the Charge folder.
*! B604896,1 MHM 10/08/2001 The Sales link on the style for the credit memo is wrong for RetLine
*! B605842,1 ABD 04/22/2002 Fix bug that the header doesn't update with correct data.
*! B606195,1 ADEL 08/21/02 Save all lines of the RA.
*! C102676,1 SSE 09/03/2002 Custom modifications add 3rd Rep and comm for OrdHdr file.
*! B606720,4 ABD 01/13/2003 Fix Bug that the credit memo get the incorrect tax.
*! B606929,1 ABD 02/04/2003 Check if the file is used or not.
*! C200517,1 KHM 04/30/2003 (Begin) To update the GLAccount in the GLDIST file with 
*! C200517,1                6500-000-000-0000 for catg_key = "014" and cTax = "000100"
*! B607195,1 KHM 06/03/2003 Saving the charges to CRMCHRG in case of England
*! C037816,1 MHM 04/06/2004 Custom Bin Location For David Luke
*! C123847,1 TMI 12/28/2004 Add a trigger to reduce the STYHIST.used qty for an employee for DIR03
*! B127806,1 EIH 05/22/2005 Fix bug that when you Credit Memo is selected to return the entire invoice 
*:                         ,values  should exactly match the invoice .
*: B129709,1 MHM 10/10/2005 Fix bug of return wrong in case of there is return style
*: B131872,1 NNA 04/20/2006 fix some errors as a result of B#127806
*: B039660,1 NNA 06/06/2006 Convert CP#037816 (Bin Location) to be used within Binmain.Prg.
*: T20060831.0026 TMI 09/20/2006 calculate the merchendize discount as it is calculated in the invoice
*: B607978,1 TMI 02/19/2007 Fix a bug that Credit Memo Not Updating GL Correctly
*: B608085,1 NNA 05/15/2007 Fix bug that if you created R/A that assigned with an invoice that also has merchandise discount
*: B608085,1 NNA            then you created a credit memo on this R/A , On saving the credit memo, the merchandise discount
*: B608085,1 NNA            is incorrectly applied again on the RETHDR record [T20070327.0010]
*: E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] 
*: B608533,1 NNA 04/23/2008 (T20080404.0003) Fix bug that Credit memo with merch discount assigned to invoice 
*: B608533,1 NNA            (without return entire invoice) has header value <> line value after saving and re_access
*!**********************************************************************************************
FUNCTION lfCMSav
*E301490,1 Add new parameters Called from EDI
*PARAMETERS llPontSale , lcCrMemHdr , lcCrMemLin
PARAMETERS llPontSale , lcCrMemHdr , lcCrMemLin, llFromEdi
*E301490,1 (End)

*-- If called from point of sale program.
IF llPontSale
  *B603538,1 Reham On 03/29/2000   *** Begin ***
  *B603538,1 Fix the error of "Variable lcOpenLine not found" if called from PS module.
  lcOpenLine = gfTempName()
  *B603538,1 Reham On 03/29/2000   *** End   ***
  
  *-- Open the gl distribution file if it was not open and set flag 
  *-- to close before quitting the program.
  =gfOpenFile(gcDataDir+'GLDIST' , gcDataDir+'GLDISTAC' , 'SH')
  
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the invoice header file.
  =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***
  
  *-- Check if the current country is england or not.
  llIsEnglnd = IIF(UPPER(ALLTRIM(gcContCode)) = 'ENG', .T., .F.)
  
  *-- If country is Canada.
  llIsCanada = IIF(UPPER(ALLTRIM(gcContCode)) = 'CANADA', .T., .F.)
  
  *-- If calculate tax.
  llTax      = ALLTRIM(gfGetMemVar('M_TAX',gcAct_Comp)) = 'Y'
  
  *-- Tax method.
  lcTax_Meth = gfGetMemVar('M_TAX_METH',gcAct_Comp)
  
  *-- See if there is GL link or not.
  llLink_GL  = ALLTRIM(gfGetMemVar('M_LINK_GL',gcAct_Comp)) = 'Y'
  
  *B603003,1 Reham On 06/21/1999  *** Begin ***
  *B603003,1 Define this variable to prevent the error if called
  *B603003,1 from point of sale.
  *-- If link by division.
  llDiv_Link = ALLTRIM(gfGetMemVar('M_DIV_LINK',gcAct_Comp))  = 'Y'
  *B603003,1 Reham On 06/21/1999   *** End   ***
  
  *-- Post factored A/R to customer accounts or not.
  llPostfInv = ALLTRIM(gfGetMemVar('XPOSTFINV',gcAct_Comp)) = 'N'
  
  *-- If use the style average cost or not.
  llAvg_Cost = ALLTRIM(gfGetMemVar('M_COST_METH',gcAct_Comp)) = 'A'
  
  *-- Use Dyelot or not.
  llUseDyes  = ALLTRIM(gfGetMemVar('M_DYELOT',gcAct_Comp))    = 'Y'
  
  lcGLDstTmp = ""      && Var. hold the temp. name of gl distribution file.
  *B602719,1 Rehan On 04/05/99   *** Begin ***
  *B602719,1 If called from point of sale module, Create the temp. GLDIST file.
  lcGLDstTmp = gfTempName()
  IF llLink_GL
    SELECT GLDIST
    COPY STRUCTURE TO (gcWorkDir+lcGLDstTmp)
    =gfOpenFile(gcWorkDir+lcGLDstTmp,'','EX')
    INDEX ON GLaccount TAG lcGLDstTmp
  ENDIF
  *B602719,1 Rehan On 04/05/99   *** End   ***
  
  lcInvSlLnk = ""      && Var. hold the invoice GL link sales account.
  lcCstSlLnk = ""      && Var. hold the customer GL link sales account.
  lcGLFYear  = ""      && Var. hold GL fisacl year.
  lcGLPeriod = ""      && Var. hold GL fisacl period.
  lcRmGlSess = ""      && Var. hold the GL session no.
  llRmGlSess = .F.     && Flag to know if create GL session no. or not.
  lnPstRate  = 0       && Var. hold the default pst rate for canada.
  lnPstTotal = 0       && Var. hold the total pst amount.
  lnInvTrdDs = 0       && Var. hold the trade discount variable.
  
  *-- Get the default of Canada tax rule from invoice file if there is invoice no.
  IF !EMPTY(&lcCrMemHdr..Invoice) .AND. SEEK(&lcCrMemHdr..Invoice , "INVHDR")
    lcPstRule  = InvHdr.cTaxRule
    lcInvTerms = INVHDR.cTermCode
  ELSE
    lcInvTerms = ""
    *-- Get the default tax rule from the customer file.
    IF !EMPTY(&lcCrMemHdr..Account) .AND. SEEK("M"+&lcCrMemHdr..Account , "CUSTOMER")
      lcPstRule  = Customer.cTaxRule
    ELSE
      lcPstRule  = ""
    ENDIF
  ENDIF

  *-- Get the invoice trade discount if it was not defined.
  lnInvTrdDs = IIF(!EMPTY(&lcCrMemHdr..Invoice) .AND. ;
               SEEK(&lcCrMemHdr..Invoice , "INVHDR") , INVHDR.trde_disc , 0)
  
  *-- Default the tax rate to the first tax rate.
  GO TOP IN (lcCrMemLin)
  lnTaxRate = &lcCrMemLin..Tax_Rate
ENDIF

*E301077,4 Reham On 12/27/98   *** Begin ***
*E301077,4 Open the style dyelot file.
=gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
*E301077,4 Reham On 12/27/98   *** End   ***

*B602719,1 Rehan On 04/05/99   *** Begin ***
*-- Create temp. file to hold the gl distribution records.
*IF llLink_GL
*  lcGLDstTmp = gfTempName()
*  SELECT GLDIST
*  COPY STRUCTURE TO (gcWorkDir+lcGLDstTmp)
*  =gfOpenFile(gcWorkDir+lcGLDstTmp,'','EX')
*  INDEX ON GLaccount TAG lcGLDstTmp
*ENDIF
*B602719,1 Rehan On 04/05/99   *** End   ***

*B602771,1 Reham On 04/11/99   *** Begin ***
IF llMulCurr
*B602771,1 Reham On 04/11/99   *** End   ***
  *-- Get exchange rate sign and unit sign.
  lcUntSin = ' '
  lcExRSin = gfGetExSin(@lcUntSin , &lcCrMemHdr..cCurrCode)
*B602771,1 Reham On 04/11/99   *** Begin ***
ELSE
  lcUntSin   = '/'        &&  Variable to hold unit sign.
  lcExRSin   = '/'        &&  Variable to hold exchange rate sign.
ENDIF
*B602771,1 Reham On 04/11/99   *** End   ***

*-- Validate the posting date if called from point of sale program & one of
*-- GL fiscal year or period is empty.
IF llPontSale .AND. (EMPTY(lcGLFYear) .OR. EMPTY(lcGLPeriod))
  =CheckPrd(&lcCrMemHdr..dPostDate , 'lcGLFYear' , 'lcGLPeriod' , 'RM' , .T.)
ENDIF

*-- Get the year & period for the transaction date.
STORE "" TO lcTrYear , lcTrPeriod
=CHECKPRD(&lcCrMemHdr..CrDate , 'lcTrYear' , 'lcTrPeriod' , 'RM' , .T.)

*-- Check if the current customer is vat exempted or not from the customer file.
*B603666,1 Ramy On 06/20/2000   Begin
*IF !EMPTY(&lcCrMemHdr..Account) .AND. SEEK("M"+&lcCrMemHdr..Account , "CUSTOMER")
IF !EMPTY(&lcCrMemHdr..Account)
  IF !EMPTY(&lcCrMemHdr..STORE) .AND. SEEK("S"+&lcCrMemHdr..Account+&lcCrMemHdr..Store , "CUSTOMER")
    llVatExem  = CUSTOMER.lVatExem
  ELSE
    IF SEEK("M"+&lcCrMemHdr..Account , "CUSTOMER")
      llVatExem  = CUSTOMER.lVatExem
    ENDIF
  ENDIF
  *B603666,1 Ramy On 06/20/2000   End
  
  *B602688,1 Reham On 03/18/99   *** Begin ***
  *lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
  *B602688,1 Reham On 03/18/99   *** End   ***
ELSE
  llVatExem  = .F.
  *B602688,1 Reham On 03/18/99   *** Begin ***
  *lcCstSlLnk = "DEF"
  *B602688,1 Reham On 03/18/99   *** End   ***
ENDIF

*B602688,1 Reham On 03/18/99   *** Begin ***
IF llLink_GL

  *B607978,1 TMI [Start] locate the main record for the account in customer file
  lnRecno = RECNO('CUSTOMER')
  =SEEK('M'+laData[2],'CUSTOMER')
  *B607978,1 TMI [End  ] 

  *B602688,1 If Gl is set at division level, get GL Link code from division.
  IF llDiv_Link .AND. !EMPTY(&lcCrMemHdr..cDivision)
    DECLARE laDRltFld[2,2]
    laDRltFld[1,1] = 'LINK_CODE'
    laDRltFld[1,2] = 'lcCustLink'
    laDRltFld[2,1] = 'CSLSGLLINK'
    laDRltFld[2,2] = 'lcCstSlLnk'
    
    =gfRltFld(&lcCrMemHdr..cDivision , @laDRltFld,'CDIVISION')
    lcCstSlLnk = IIF(!EMPTY(lcCstSlLnk), PADR(lcCstSlLnk,3) ,IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF"))
  ELSE
    *! B604896,1 MHM 10/08/2001 get sales link from customer if found [Start]
    *lcCstSlLnk = "DEF"
    lcCstSlLnk = IIF(!EMPTY(CUSTOMER.cslsgllink) , CUSTOMER.cslsgllink , "DEF")
    *! B604896,1 MHM 10/08/2001 [End]
  ENDIF

  *B607978,1 TMI [Start] locate the old record
  IF BETWEEN(lnRecno,1,RECCOUNT('CUSTOMER'))
    GOTO (lnRecno) IN CUSTOMER
  ENDIF
  *B607978,1 TMI [End  ] 

ELSE
  lcCstSlLnk = ""
ENDIF
*B602688,1 Reham On 03/18/99   *** Begin ***

*-- Call local function to get the trade discount.
lnInvTrdDs = lfGetTrdDs(&lcCrMemHdr..cTermCode , &lcCrMemHdr..Invoice )

SELECT (lcCrMemHdr)
*-- Lock the current header record to grantee the phiscal update.
=RLOCK()
*-- Calculate the salesreps. charge back.
IF lnInvTrdDs <> 0
  REPLACE CommAmt1 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..CommPcnt1/100),2)) * -1 ;
          CommAmt2 WITH ABS(ROUND((&lcCrMemHdr..Amount * (100 - lnInvTrdDs)/100) * (&lcCrMemHdr..CommPcnt2/100),2)) * -1
ELSE
  REPLACE CommAmt1 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..CommPcnt1/100),2)) * -1 ;
          CommAmt2 WITH ABS(ROUND(&lcCrMemHdr..Amount * (&lcCrMemHdr..CommPcnt2/100),2)) * -1
ENDIF
UNLOCK

*C102676,1 Custom process for A.S.T. [Begin]
IF ASCAN(laEvntTrig,PADR('CRUPCOM3',10))<>0
  =gfDoTriger('RMCRMEM',PADR('CRUPCOM3',10))
ENDIF
  
*C102676,1 Custom process for A.S.T. [End]
*-- Calculate taxes for England.
*IF llIsEnglnd
*  lnEngTax = lfTotalTax()
*  SELECT (lcCrMemHdr)
*  *-- Lock the current record in the header file to grantee the phiscal update.
*  =RLOCK()
*  REPLACE Tax_Amt WITH lnEngTax
*  UNLOCK
*ENDIF

*-- Define temp. var. to hold the calculated taxes.
*lnCurTax   = &lcCrMemHdr..Tax_Amt
IF llTax
  *IF lcTax_Meth = 'A'
    *-- Calculate the tax amount if the company is "England" & the RA 
    *-- related to the invoice, otherwise the program does not Calculate
    *-- the tax amount.
  *  IF llIsEnglnd
  *    *-- Changed the way of calculating tax amount.
  *    IF !EMPTY(&lcCrMemHdr..Invoice) .AND. &lcCrMemHdr..cIntr_Inv = "Y"
  *      lnCurTax = lnCurTax + lfchgTax()
  *    ENDIF
  *  ELSE
  *    IF !llIsCanada
  *      *lnCurTax = lnCurTax + &lcCrMemHdr..Other * (lnTaxRate/100)
  *    ENDIF
  *  ENDIF
  *ENDIF
  
  *-- To display the total PST tax amount if the country was canada.
  IF llIsCanada
    SELECT (lcCrMemLin)
    SUM Amount * (nPstRate/100) , Amount * (Tax_Rate/100) ;
        FOR !EMPTY(Style) TO lnPstTotal , lnTot_Tax
    lnPstTotal = lnPstTotal + &lcCrMemHdr..Other * (lnPstRate/100)
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
*-mhm2006
*SUM ALL TotQty , (Gros_Price*TotQty)*(1-(Disc_Pcnt/100)) , ;
        (TotQty*Gros_Price) , Disc_amt , Trde_Amt FOR !EMPTY(Style) ;
        TO lnTPieces , lnTAmount , lnTGrosAmt , lnTDiscAmt , lnTTrdeAmt

SUM ALL TotQty , TotQty * ROUND((Gros_Price * (1-(Disc_Pcnt/100))),2) , ;
        (TotQty*Gros_Price) , Disc_amt , Trde_Amt FOR !EMPTY(Style) ;
        TO lnTPieces , lnTAmount , lnTGrosAmt , lnTDiscAmt , lnTTrdeAmt

*--mhm2006

lnPstTotal = IIF(llTax .AND. llIsCanada , lnPstTotal , 0)

SELECT (lcCrMemHdr)
*-- Lock the current record to grantee the phisacl update.
=RLOCK()
*C102212,1 (Begin) Add HDT Tax .
*REPLACE Pieces    WITH lnTPieces  ;
        Gross_Amt WITH lnTGrosAmt ;
        Disc_Amt  WITH lnTDiscAmt ;
        Amount    WITH lnTAmount  ;
        Tax_Amt   WITH IIF(llTax , Tax_Amt , 0) ;
        nPstAmt   WITH lnPstTotal ;
        TotCredit WITH Other + Amount + Tax_Amt + nPstAmt
REPLACE Pieces    WITH lnTPieces  ;
        Gross_Amt WITH lnTGrosAmt ;
        Disc_Amt  WITH lnTDiscAmt ;
        Amount    WITH lnTAmount  ;
        Tax_Amt   WITH IIF(llTax , Tax_Amt , 0) ;
        nPstAmt   WITH lnPstTotal ;
        TotCredit WITH Other + Amount + Tax_Amt + nPstAmt + nhstamt
*C102212,1 (End)        
UNLOCK

*B605842,1 ABD - FiX bug that the header doesn't update with correct data , so 
*B605842,1 ABD - I will sum some fields from the line file. [Begin]
PRIVATE    lnPieces,lnTotQty,lnGrossAmt,lnAmount
STORE 0 TO lnPieces,lnTotQty,lnGrossAmt,lnAmount
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
    *B127806,1 EIH 05/22/2005 if entire invoice selected then it must same as invoice[Begin].
    *lnAmount   = lnAmount   + (Price      * lnTotQty)
    *--mhm2006
    *lnAmount   = lnAmount   + (Gros_Price * (1 -  (Disc_pcnt/100)) * lnTotQty)
    *T20060831.0026 TMI [Start] Remove the EIH fix and get the merchandize discount from the INVHDR.DISCPCNT field
    *lnAmount   = lnAmount   + lnTotQty * ROUND((Gros_Price * (1 -  (Disc_pcnt/100)) ),2)
    lnAmount   = lnAmount   + (Gros_Price*lnTotQty)
    *T20060831.0026 TMI [End  ] 
    *--mhm2006
    *B127806,1 EIH 05/22/2005 [End].
  ENDIF  
ENDSCAN

*T20061031.0001 TMI [Start] take the discount of the invoice only if an invoice is selected
IF !EMPTY(laData[7])
  *T20061031.0001 TMI [End  ] 

  *T20060831.0026 TMI [Start] get the merchandize discount from the INVHDR.DISCPCNT field

  *B608085,1 NNA 05/15/2007 (Begin) Do the following code only if credit memo not created on R/A
  IF EMPTY(laData[3])
  *B608085,1 NNA(END)
    
    *B608533,1 NNA 04/23/2008 (Begin) Recalculated the discount from the detail lines in all cases (even it assigned to invoice or not)
    *B608533,1 NNA            so I replaced the equation with laData[24] that has sum for all lines discount
    *lnAmount = lnAmount - ROUND( lnAmount*(INVHDR.DISCPCNT/100) , 2 )
    lnAmount = lnAmount - laData[24]
    *B608533,1 NNA (End)
    
  *B608085,1 NNA 05/15/2007 (Begin)
  ENDIF
  *B608085,1 NNA (END)

  *T20060831.0026 TMI [End  ] 

  *T20061031.0001 TMI [Start] preserve the amount with the discount removed
ELSE  && added as a rework
  lnAmount = lnAmount - lnTDiscAmt 
ENDIF
*T20061031.0001 TMI [End  ] 

SELECT (lcCrMemHdr)
*-- Lock the current record to grantee the phisacl update.
=RLOCK()
REPLACE Pieces    WITH lnPieces           ,;
        Gross_Amt WITH lnGrossAmt         ,;
        Amount    WITH lnAmount           ,;
        TotCredit WITH lnAmount + Other + Tax_Amt + nPstAmt + nhstamt,;
        Disc_Amt  WITH lnGrossAmt - lnAmount
UNLOCK
*B605842,1 ABD - [End]

IF llLink_GL
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *B602719,1 Check the header steps that done.
  IF &lcCrMemHdr..nSteps < 2
  *B602719,1 Reham On 04/05/99   *** End   ***
        
    *-- Call GL Distribution Procedure To Add The Following 
    *-- Accounts For This Return.
    *B602688,1 Reham On 03/21/99   *** Begin ***
    *B602688,1 Send the frieght account to the function.
    *** 1) FREIGHT + INSURANCE + COD   <DEBIT>  FREIGHT INCOME
    *** CATEGORY KEY FOR "Freight"................=> '004'
    *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '004',&lcCrMemHdr..OTHER , 'RM' ,;
                   &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                   lcGLFYear,lcGLPeriod , '&lcGLDstTmp' , '' ,;
                   &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                   &lcCrMemHdr..nExRate
    *B606720,4 ABD - Fix Bug that the credit memo get the incorrect tax. [Begin]
    *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '004',&lcCrMemHdr..OTHER , 'RM' ,;
    *               &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
    *               lcGLFYear,lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cFrgtAcnt ,;
    *               &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
    *               &lcCrMemHdr..nExRate
    *B602688,1 Reham On 03/21/99   *** End   ***

    *B606929,1 ABD - Check if the file is used or not.[Begin]
    *IF UPPER(ALLTRIM(gcContCode))='ENG' .AND. TYPE('lcEngChrg') = "C" .AND. !EMPTY(lcEngChrg)
     IF UPPER(ALLTRIM(gcContCode))='ENG' .AND. TYPE('lcEngChrg') = "C" .AND.;
      !EMPTY(lcEngChrg) .AND. USED(lcEngChrg)
      *B606929,1 ABD - Check if the file is used or not.[End]

      IF SEEK(&lcCrMemHdr..Order+&lcCrMemHdr..Store,lcEngChrg)
        *-- the company is england so update the Gl with the english charges
        *--In case of england charges taxes is at style level
        SELECT (lcEngChrg)
        SCAN REST WHILE Order+cStore+PikTkt+cchrgcode = &lcCrMemHdr..Order+&lcCrMemHdr..Store

          *B607195,1 KHM 06/03/2003 (Begin) Saving the charges to CRMCHRG file
          SCATTER MEMVAR MEMO
          m.CrMemo = laData[1]
          m.Store  = cStore
          *B607195,1 KHM 06/03/2003 (End)
          
          DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '004',&lcEngChrg..nChrgAmnt, 'RM' ,;
                         &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                         lcGLFYear,lcGLPeriod , '&lcGLDstTmp' , &lcEngChrg..cFrgtAcnt ,;
                         &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                         &lcCrMemHdr..nExRate
          
          *B607195,1 KHM 06/03/2003 (Begin) Saving the charges to CRMCHRG file
          INSERT INTO CrmChrg FROM MEMVAR
          =gfAdd_Info('CRMCHRG')
          *B607195,1 KHM 06/03/2003 (End)
          
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
    *B606720,4 ABD - [End]
    
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *B602719,1 Reham On 04/05/99   *** Begin ***
    IF &lcCrMemHdr..OTHER <> 0
      REPLACE cFrgtAcnt WITH &lcGLDstTmp..glaccount
    ENDIF
    *B602719,1 Svae the header steps that done.
    REPLACE nSteps    WITH 2
    *B602719,1 Reham On 04/05/99   *** End   ***
    UNLOCK
  ENDIF
  
  *-- If The System Has Been Setup To Compute Tax.
  IF llTax

    *B602719,1 Reham On 04/05/99   *** Begin ***
    *B602719,1 Check the header steps that done.
    IF &lcCrMemHdr..nSteps < 3
    *B602719,1 Reham On 04/05/99   *** End   ***
      *B602688,1 Reham On 03/21/99   *** Begin ***
      *B602688,1 Send the tax account to the function.
      *** 2) SALES TAX LIABILITY       <DEBIT>  TAX INCOME
      *** CATEGORY KEY FOR "Sales tax liability"..=> '014'
      *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
                     &lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt , 'RM' ,;
                     &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                     lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , '' ,;
                     &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                     &lcCrMemHdr..nExRate
      *C102212,1 (Begin) Add HDT Tax .
      *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
                     &lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt , 'RM' ,;
                     &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                     lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
                     &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                     &lcCrMemHdr..nExRate
        
        *C200517,1 KHM 04/30/2003 (Begin) To update the GLAccount in the GLDIST file with 
        *C200517,1                6500-000-000-0000 for catg_key = "014" and cTax = "000100"
        IF ASCAN(laEvntTrig,PADR('UPDGLACT',10))<>0
          =gfDoTriger('RMCRMEM',PADR('UPDGLACT',10))
        ENDIF        
        *C200517,1 KHM 04/30/2003 (End)

        *E302472,1 MHM 11/18/2007  Multiple Taxes for Canada on Invoice [T20060709.0008] [Start]
        *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
                     &lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt+&lcCrMemHdr..nhstamt , 'RM' ,;
                     &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                     lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
                     &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                     &lcCrMemHdr..nExRate             

        IF llIsCanada
            llRpHSTTax = .F.
            =lfGetHST()
            IF !llRpHSTTax
              DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
                           &lcCrMemHdr..TAX_AMT, 'RM' ,;
                           &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                           lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
                           &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                           &lcCrMemHdr..nExRate             


              DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '027' ,;
                           &lcCrMemHdr..nPstAmt , 'RM' ,;
                           &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                           lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
                           &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                           &lcCrMemHdr..nExRate             

            ELSE
              DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '028' ,;
                           &lcCrMemHdr..nhstamt , 'RM' ,;
                           &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                           lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
                           &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                           &lcCrMemHdr..nExRate             
            ENDIF
                     
        ELSE
        
              DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '014' ,;
                           &lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt+&lcCrMemHdr..nhstamt , 'RM' ,;
                           &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                           lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cTaxAcnt ,;
                           &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                           &lcCrMemHdr..nExRate             
        ENDIF
        *E302472,1 MHM [END]
                     
       *C102212,1 (END) Add HDT Tax .            
      *B602688,1 Reham On 03/21/99   *** End   ***
      
      SELECT (lcCrMemHdr)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      *B602719,1 Reham On 04/05/99   *** Begin ***
      IF (&lcCrMemHdr..TAX_AMT+&lcCrMemHdr..nPstAmt) <> 0
        REPLACE cTaxAcnt WITH &lcGLDstTmp..glaccount
      ENDIF
      
      *C200517,1 KHM 04/30/2003 (Begin) To replace cTaxAcnt with the related field 
      *C200517,1                (glaccount) of taxcode "000100"
      IF ASCAN(laEvntTrig,PADR('REPTAXAC',10))<>0
        =gfDoTriger('RMCRMEM',PADR('REPTAXAC',10))
      ENDIF
      *C200517,1 KHM 04/30/2003 (End)
      REPLACE nSteps   WITH 3
      *B602719,1 Reham On 04/05/99   *** End   ***
      UNLOCK
    ENDIF
  ENDIF
  
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *B602719,1 Check the header steps that done.
  IF &lcCrMemHdr..nSteps < 4
  *B602719,1 Reham On 04/05/99   *** End   ***
    *B602688,1 Reham On 03/21/99   *** Begin ***
    *B602688,1 Send the AR account to the function.
    *** 3) TOTAL CHARGING              <CREDIT>  ACCOUNT RECEIVABLE
    *** CATEGORY KEY FOR "Accounts Receivable"....=> '001'
    *DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '001' , -(&lcCrMemHdr..TOTCREDIT),;
                   'RM' , &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                   lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , '' ,;
                   &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                   &lcCrMemHdr..nExRate
    DO GLDIST WITH &lcCrMemHdr..LINK_CODE , '001' , -(&lcCrMemHdr..TOTCREDIT),;
                   'RM' , &lcCrMemHdr..CRMEMO , &lcCrMemHdr..dPostDate ,;
                   lcGLFYear , lcGLPeriod , '&lcGLDstTmp' , &lcCrMemHdr..cARAcnt ,;
                   &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit ,;
                   &lcCrMemHdr..nExRate
    *B602688,1 Reham On 03/21/99   *** End   ***
    
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *B602719,1 Reham On 04/05/99   *** Begin ***
    IF &lcCrMemHdr..TOTCREDIT <> 0
      REPLACE cArAcnt WITH &lcGLDstTmp..glaccount
    ENDIF
    REPLACE nSteps  WITH 4
    *B602719,1 Reham On 04/05/99   *** End   ***
    UNLOCK
  ENDIF
ENDIF

*B602719,1 Reham On 04/05/99   *** Begin ***
*B602719,1 Check the header steps that done.
*IF &lcCrMemHdr..nSteps < 2
IF &lcCrMemHdr..nSteps < 5
*B602719,1 Reham On 04/05/99   *** End   ***
  SELECT (lcCrMemHdr)


  *B127806,1 EIH 05/22/2005 If entire invoice selected then it must save same as invoice
  *B127806,1 EIH            We update numeric header data by its related in invhdr.
  *B127806,1 EIH            llEntrInv varaible to know if its entire or not declared in rmcrmem.prg [Begin].
  IF llEntrInv  
 
    *B131872,1 NNA 04/20/2006 (Begin) Comment this code because it is not logic specially if you change if the header information
    *REPLACE Gross_Amt WITH INVHDR.shipamt       ,;
            Disc_Amt  WITH -1*(INVHDR.discount) ,;
            Tax_Amt   WITH INVHDR.Tax_Amt       ,;
            TotCredit WITH INVHDR.totalchg      ,;
            OTHER     WITH INVHDR.Ncharges
    *B131872,1 NNA (End)

  ENDIF
  *B127806,1 EIH   [End]

  SCATTER MEMVAR MEMO
  
  *-- Save the current credit memo to the master return header file.
  SELECT RETHDR
  APPEND BLANK
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  GATHER MEMVAR MEMO
  
  *-- Call global function to add audit fields info.
  =gfAdd_Info('RETHDR')
  UNLOCK
  
  *-- Call Global Function to transmit the local data.
  =gfTraceKey("RetHdr" , RetHdr.CRMEMO , "A")
  
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *REPLACE nSteps WITH 2
  REPLACE nSteps WITH 5
  *B602719,1 Reham On 04/05/99   *** End   ***
  UNLOCK
ENDIF

*-- Do not call the notepad program if coming from the point of sale program.
IF !llPontSale
  *B602753,1 HDM 04/07/1999[Start] Stop Calling NotePad Program In lpSavScr as the global save
  *                         Will Call it
  **-- If add notes for the current credit memo.
  **** Do you want to add notes for the current cedit memo? ***
  **** < Yes > - < No > ***
  *IF gfModalGen("QRM00271B00006" , "DIALOG") = 1
  *  *E301077,4 Reham On 12/17/98   *** Begin ***
  *  *E301077,4 Open the note pad file.
  *  =gfOpenFile(gcDataDir+'NotePad',gcDataDir+'NotePad','SH')
  *  *E301077,4 Reham On 12/17/98   *** End   ***
    
  *  =NOTEPAD('R',&lcCrMemHdr..CrMemo)
  *  *-- Call Global Function to transmit the local data.
  *  =gfTraceKey("NotePad" , "R"+&lcCrMemHdr..CrMemo , "A")
    
  *  *E301077,4 Reham On 12/17/98   *** Begin ***
  *  =gfCloseFile('NotePad')
  *  *E301077,4 Reham On 12/17/98   *** End   ***
  *ENDIF
  *B602753,1 HDM 04/07/1999[End]
ENDIF

SELECT (lcCrMemLin)
*-- Lock the record to grantee the phiscal update.
= RLOCK()

*B802907,1 Reham On 12/26/1999    *** Begin ***
*-- Delete all empty styles records.
*DELETE ALL FOR EMPTY(Style)
DELETE ALL FOR EMPTY(Style) .OR. (Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8) = 0
*B802907,1 Reham On 12/26/1999    *** End   ***
UNLOCK

*-- Flag to know if cancel the difference between 
*-- the open qty. & the received qty.
llCancRest = .F.

*B802907,1 Reham On 12/26/1999   *** Begin ***
*E301490,1 START
*=lfCrtUnComp(.F. , .F. , .F. , .T.)
=lfCrtUnComp(.F. , .F. , .F. , .T.,llFromEDI)
*E301490,1 END
SELECT (lcCrMemLin)
GO TOP
DECLARE laOrgQty[8] , laOpnLin[8]
SCAN
  STORE 0 TO laOpnLin , laOrgQty
  SCATTER MEMO TO MEMVAR
  SELECT (lcOpenLine)
  
  *B603741,1 WAB - add the Cret-linNo in the seek expression so it can add new line if the style 
  *B603741,1 WAB - is dublicated
  *B603741,1 WAB - StART
  *IF !SEEK(&lcCrMemLin..ACCOUNT+&lcCrMemLin..STYLE , lcOpenLine)
  IF !SEEK(&lcCrMemLin..ACCOUNT+&lcCrMemLin..STYLE+&lcCrMemLin..Cret_LinNo, lcOpenLine)
  *B603741,1 WAB - end
    APPEND BLANK
  ENDIF
  SCATTER FIELDS Qty1 , Qty2 , Qty3 , Qty4 , Qty5 , Qty6 , Qty7 , Qty8 TO laOrgQty
*  SCATTER FIELDS nOpnQty1 , nOpnQty2 , nOpnQty3 , nOpnQty4 , nOpnQty5 , nOpnQty6 , nOpnQty7 , nOpnQty8 TO laOpnLin
  GATHER MEMO FROM MEMVAR
  FOR lnOpn = 1 TO 8
    lcOpn = ALLTRIM(STR(lnOpn))
*    REPLACE Qty&lcOpn     WITH Qty&lcOpn + laOrgQty[lnOpn] ;
*            nOpnQty&lcOpn WITH nOpnQty&lcOpn + laOpnLin[lnOpn]
    REPLACE Qty&lcOpn     WITH Qty&lcOpn + laOrgQty[lnOpn]
  ENDFOR
*  REPLACE TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8 ;
*          nTotOpnQty WITH nOpnQty1+nOpnQty2+nOpnQty3+nOpnQty4+nOpnQty5+nOpnQty6+nOpnQty7+nOpnQty8
  REPLACE TotQty     WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8
  SELECT (lcCrMemLin)
ENDSCAN
SELECT (lcOpenLine)
*B802907,1 Reham On 12/26/1999   *** End   ***

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

*B802907,1 Reham On 12/26/1999   *** Begin ***
SELECT (lcCrMemLin)
*B802907,1 Reham On 12/26/1999   *** End   ***

lnLineCnt  = 1    && Var. hold line counter.

lnTotCancl = 0    && Var. hold total qty. canceled.

*!B602322,1 Moved-Read the gl session if not readed to be used in GfStyclr.
IF !llRmGlSess
  lcRmGlSess = gfsequence('GLSESSION')
  llRmGlSess = .T.
ENDIF
*!B602322,1 End.

*B602719,1 Reham On 04/05/99   *** Begin ***
*-- Call local function to update the temp. files & needed variables 
*-- fields in the uncompleted session file record.
llNothing = IIF(llPontSale , .F. , lfUpdVars())
*B602719,1 Reham On 04/05/99   *** End   ***

*-- Var. hold the incremental # will be saved in the nSteps field in the
*-- credit memo line file.
lnNxtStp   = 0

*B602719,1 Reham On 04/05/99   *** Begin ***
*B602719,1 Check the header steps that done.
*IF &lcCrMemHdr..nSteps < 3
IF &lcCrMemHdr..nSteps < 6
*B602719,1 Reham On 04/05/99   *** End   ***
  lnCurRec  = 1                     && Var. hold current record no.
  lnTotRec  = RECCOUNT(lcCrMemLin)  && Var. hold total lines record count.

  *B603741,1 WAB - in case of !calling from POS set the order to 2nd tag (account+Cret_LinNo+style)
  *B603741,1 WAB - cause in some case if we used the 1st tag (account+style+Cret-linNo+cret_trncd) 
  *B603741,1 WAB - and we have dublicate style/color in 2 diffrent lines during the save 1st record 
  *B603741,1 WAB - get his Crmemo # so they rotate their order the 2nd one did not get his Crmemo# 
  *B603741,1 WAB - so the prog didn't save this line in the mastr file.
  *B603741,1 WAB - StART
  IF !llPontSale
    lcCurrtag= ORDER()
    SET ORDER TO TAG lcCrMmLine 
  ENDIF
  *B603741,1 WAB - END
  
  SCAN FOR !EMPTY(STYLE)
    *B602732,1 Reham On 04/04/99   *** Begin ***
    *B602732,1 Blank the steps counter for the current line
    lnNxtStp = 0
    *B602732,1 Reham On 04/04/99   *** End   ***
    
    *-- Call the global function that execute the thermometer.
    =gfTherm(lnTotRec,lnCurRec,"Saving credit memo # : "+&lcCrMemHdr..CrMemo)
    lnCurRec = lnCurRec + 1
    
    SELECT (lcCrMemLin)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    
    *B603003,1 Reham On 06/21/1999   *** Begin ***
    *B603003,1 Do not update the lines numbers in the credit memo lines
    *B603003,1 in the save procedure if called from point of sale to 
    *B603003,1 prevent the error of not adding some lines.
    *REPLACE CRMEMO WITH &lcCrMemHdr..CrMemo ;
            CRET_LINNO WITH ALLTRIM(STR(lnLineCnt))
    REPLACE CRMEMO     WITH &lcCrMemHdr..CrMemo ;
            CRET_LINNO WITH IIF(llPontSale , CRET_LINNO , ALLTRIM(STR(lnLineCnt)))
    *B603003,1 Reham On 06/21/1999   *** End   ***
    *-- If there is a R/A no. & the line is already exist in RALINE file.
    IF !EMPTY(&lcCrMemHdr..RaNo) .AND. SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
      llRALinExs = .T.    && The current line exist in RALINE file.
    ELSE
      llRALinExs = .F.    && The current line does not exist in RALINE file.
    ENDIF
    
    SELECT STYLE
    *-- If there is return style , get its sales code
    IF !EMPTY(&lcCrMemLin..cretSty) .AND. SEEK(&lcCrMemLin..cretSty)
      lcStySlLnk = Style.cslsgllink
    ELSE
      lcStySlLnk = ""
    ENDIF
    =SEEK(&lcCrMemLin..STYLE)
    llInvSty = Style.lInvSty
    
    *-- If return to the original style, get its sales code.
    lcStySlLnk = IIF(EMPTY(lcStySlLnk) , Style.cslsgllink , lcStySlLnk )
    SELECT (lcCrMemLin)
    REPLACE GL_SALES WITH IIF(EMPTY(lcInvSlLnk) , ALLTRIM(lcCstSlLnk) + ALLTRIM(lcStySlLnk) , ALLTRIM(lcInvSlLnk) + ALLTRIM(lcStySlLnk))
    UNLOCK
    
    IF &lcCrMemLin..nSteps < 1
      *-- Now Update The Style Dyelot Inventory For Dyelot 
      *-- System And Styles That Come In Dyelots.
      IF llUseDyes .AND. Style.cDye_Flg = 'Y' .AND. llInvSty
        SELECT STYDYE
        
        *: B129709,1 MHM 10/10/2005 Fix bug of return wrong in case of there is return style[Start]
        *IF SEEK (&lcCrMemLin..STYLE + &lcCrMemHdr..cWareCode + &lcCrMemLin..DYELOT)
        IF SEEK (IIF(!EMPTY(&lcCrMemLin..cretsty),&lcCrMemLin..cretsty,&lcCrMemLin..STYLE) + &lcCrMemHdr..cWareCode + &lcCrMemLin..DYELOT)
        *: B129709,1 MHM 10/10/2005 [End]
        
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          REPLACE Ret1   WITH Ret1 + &lcCrMemLin..Qty1   ;
                  Ret2   WITH Ret2 + &lcCrMemLin..Qty2   ;
                  Ret3   WITH Ret3 + &lcCrMemLin..Qty3   ;
                  Ret4   WITH Ret4 + &lcCrMemLin..Qty4   ;
                  Ret5   WITH Ret5 + &lcCrMemLin..Qty5   ;
                  Ret6   WITH Ret6 + &lcCrMemLin..Qty6   ;
                  Ret7   WITH Ret7 + &lcCrMemLin..Qty7   ;
                  Ret8   WITH Ret8 + &lcCrMemLin..Qty8   ;
                  TotRet WITH Ret1+Ret2+Ret3+Ret4+Ret5+Ret6+Ret7+Ret8
          
          *-- If this line from RA & exist in RALINE file, Adjust RA fields.
          IF llRALinExs
            REPLACE RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1) ;
                    RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2) ;
                    RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3) ;
                    RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4) ;
                    RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5) ;
                    RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6) ;
                    RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7) ;
                    RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8) ;
                    TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
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
      *: B129709,1 MHM 10/10/2005 Fix bug of return wrong in case of there is return style[Start]
      *IF SEEK(&lcCrMemLin..Style + &lcCrMemHdr..cWareCode + SPACE(10))
      IF SEEK(IIF(!EMPTY(&lcCrMemLin..cretsty),&lcCrMemLin..cretsty,&lcCrMemLin..STYLE)+ &lcCrMemHdr..cWareCode + SPACE(10))
      *: B129709,1 MHM 10/10/2005 [End]

        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        IF llInvSty
          REPLACE Ret1   WITH Ret1 + &lcCrMemLin..Qty1   ;
                  Ret2   WITH Ret2 + &lcCrMemLin..Qty2   ;
                  Ret3   WITH Ret3 + &lcCrMemLin..Qty3   ;
                  Ret4   WITH Ret4 + &lcCrMemLin..Qty4   ;
                  Ret5   WITH Ret5 + &lcCrMemLin..Qty5   ;
                  Ret6   WITH Ret6 + &lcCrMemLin..Qty6   ;
                  Ret7   WITH Ret7 + &lcCrMemLin..Qty7   ;
                  Ret8   WITH Ret8 + &lcCrMemLin..Qty8   ;
                  TotRet WITH Ret1+Ret2+Ret3+Ret4+Ret5+Ret6+Ret7+Ret8
          *-- If this line from RA & exist in RALINE file, Adjust RA fields.
          IF llRALinExs
            REPLACE  RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1) ;
                     RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2) ;
                     RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3) ;
                     RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4) ;
                     RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5) ;
                     RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6) ;
                     RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7) ;
                     RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8) ;
                     TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
          ENDIF
        ENDIF
        
        UNLOCK
        *-- Call Global Function to transmit the local data.
        =gfTraceKey("StyDye" , &lcCrMemLin..Style + &lcCrMemHdr..cWareCode + SPACE(10) , "M")
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

      *: B129709,1 MHM 10/10/2005 Fix bug of return wrong in case of there is return style[Start]
      IF !EMPTY(&lcCrMemLin..cretsty)
        =SEEK (&lcCrMemLin..cretsty)
      ENDIF  
      *: B129709,1 MHM 10/10/2005 [End]
      
      = RLOCK()
      IF llInvSty
        REPLACE RET1   WITH RET1 + &lcCrMemLin..QTY1 ;
                RET2   WITH RET2 + &lcCrMemLin..QTY2 ;
                RET3   WITH RET3 + &lcCrMemLin..QTY3 ;
                RET4   WITH RET4 + &lcCrMemLin..QTY4 ;
                RET5   WITH RET5 + &lcCrMemLin..QTY5 ;
                RET6   WITH RET6 + &lcCrMemLin..QTY6 ;
                RET7   WITH RET7 + &lcCrMemLin..QTY7 ;
                RET8   WITH RET8 + &lcCrMemLin..QTY8 ;
                TOTRET WITH RET1+RET2+RET3+RET4+RET5+RET6+RET7+RET8
        *C123847,1  TMI [Start] Add a trigger to reduce the STYHIST.used qty for an employee for DIR03
        IF ASCAN(laEvntTrig,PADR('EMPHST',10))<>0
          =gfDoTriger('RMCRMEM',PADR('EMPHST',10))
        ENDIF 
        *C123847,1  TMI [End  ] 
        *-- If this line from RA & exist in RALINE file, Adjust RA fields.
        IF llRALinExs
          REPLACE RA1    WITH RA1 - MIN(&lcCrMemLin..QTY1,&lcCrMemLin..nOpnQty1) ;
                  RA2    WITH RA2 - MIN(&lcCrMemLin..QTY2,&lcCrMemLin..nOpnQty2) ;
                  RA3    WITH RA3 - MIN(&lcCrMemLin..QTY3,&lcCrMemLin..nOpnQty3) ;
                  RA4    WITH RA4 - MIN(&lcCrMemLin..QTY4,&lcCrMemLin..nOpnQty4) ;
                  RA5    WITH RA5 - MIN(&lcCrMemLin..QTY5,&lcCrMemLin..nOpnQty5) ;
                  RA6    WITH RA6 - MIN(&lcCrMemLin..QTY6,&lcCrMemLin..nOpnQty6) ;
                  RA7    WITH RA7 - MIN(&lcCrMemLin..QTY7,&lcCrMemLin..nOpnQty7) ;
                  RA8    WITH RA8 - MIN(&lcCrMemLin..QTY8,&lcCrMemLin..nOpnQty8) ;
                  TOTRA  WITH RA1+RA2+RA3+RA4+RA5+RA6+RA7+RA8
        ENDIF
      ENDIF
      UNLOCK
      *-- Call Global Function to transmit the local data.
      =gfTraceKey("Style" , &lcCrMemLin..Style , "M")
      
      SELECT (lcCrMemLin)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      *B802907,1 Reham On 12/28/1999   *** Begin ***
      IF EMPTY(&lcCrMemLin..cRetSty) .AND. Style.cStyGrade <> &lcCrMemLin..cStyGrade
        REPLACE cStyGrade WITH Style.cStyGrade
      ENDIF
      *B802907,1 Reham On 12/28/1999   *** End   ***
      REPLACE nSteps WITH 3
      UNLOCK
    ENDIF
    
    *-- Update general ledger entreis
    IF llLink_GL
      *B602719,1 Reham On 04/06/99   *** Begin ***
      IF &lcCrMemLin..nSteps < 4
      *B602719,1 Reham On 04/06/99   *** End   ***
        *B602688,1 Reham On 03/21/99   *** Begin ***
        *B602688,1 Send the sales account to the function.
        *-- Post to Return Merchandise
        *-- 1) TOTAL GROSS MERCHANDISE         <DEBIT>  Return Merchandise
        *-- CATEGORY KEY FOR "Return Merchaandise....=> '020'
        *DO GLDIST WITH &lcCrMemLin..GL_Sales , '020' , ;
                       &lcCrMemLin..TotQty * &lcCrMemLin..Gros_Price , 'RM', ;
                       &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
                       lcGLFYear , lcGLPeriod , '&lcGLDstTmp', '' , ;
                       &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
                       &lcCrMemHdr..nExRate
        DO GLDIST WITH &lcCrMemLin..GL_Sales , '020' , ;
                       &lcCrMemLin..TotQty * &lcCrMemLin..Gros_Price , 'RM', ;
                       &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
                       lcGLFYear , lcGLPeriod , '&lcGLDstTmp', &lcCrMemLin..cSalesAcnt , ;
                       &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
                       &lcCrMemHdr..nExRate
        *B602688,1 Reham On 03/21/99   *** End   ***
      
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        *B602719,1 Reham On 04/06/99   *** Begin ***
        IF (&lcCrMemLin..TotQty * &lcCrMemLin..Gros_Price) <> 0
          REPLACE &lcCrMemLin..cSalesAcnt WITH &lcGLDstTmp..glaccount
        ENDIF
        REPLACE &lcCrMemLin..nSteps     WITH 4
        *B602719,1 Reham On 04/06/99   *** End ***
        UNLOCK
      ENDIF
      
      *B602719,1 Reham On 04/06/99   *** Begin ***
      IF &lcCrMemLin..nSteps < 5
      *B602719,1 Reham On 04/06/99   *** End   ***
        *B602688,1 Reham On 03/21/99   *** Begin ***
        *B602688,1 Send the discount account to the function.
        *-- 2) DISCOUNT AMOUNT                 <CREDIT>  SALES DISCOUNT
        *-- CATEGORY KEY FOR "Discount"...............=> '005'
        *DO GLDIST WITH &lcCrMemLin..GL_Sales , '005' , ;
                       -(&lcCrMemLin..DISC_AMT),'RM', ;
                       &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
                       lcGLFYear , lcGLPeriod , '&lcGLDstTmp', '' , ;
                       &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
                       &lcCrMemHdr..nExRate
        DO GLDIST WITH &lcCrMemLin..GL_Sales , '005' , ;
                       -(&lcCrMemLin..DISC_AMT),'RM', ;
                       &lcCrMemLin..CRMEMO , &lcCrMemHdr..dPostDate , ;
                       lcGLFYear , lcGLPeriod , '&lcGLDstTmp', &lcCrMemLin..cDiscAcnt , ;
                       &lcCrMemHdr..cCurrCode , &lcCrMemHdr..nCurrUnit , ;
                       &lcCrMemHdr..nExRate
        *B602688,1 Reham On 03/21/99   *** End   ***
        
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        *B602719,1 Reham On 04/06/99   *** Begin ***
        IF &lcCrMemLin..DISC_AMT <> 0
          REPLACE &lcCrMemLin..cDiscAcnt WITH &lcGLDstTmp..glaccount
        ENDIF
        REPLACE &lcCrMemLin..nSteps   WITH 5
        *B602719,1 Reham On 04/06/99   *** End   ***
        UNLOCK
      ENDIF
    ENDIF
    
    SELECT (lcCrMemLin)
    IF llLink_GL
      *B602688,1 Reham On 06/13/1999   *** Begin ***
      
      *! B604896,5 MHM 10/15/2001 IF Not Found get Defulte account [Start]
      *=SEEK(&lcCrMemLin..Gl_Sales+'008','GL_LINK')
      IF !SEEK(&lcCrMemLin..Gl_Sales+'008','GL_LINK')
        =SEEK("DEFDEF"+'008','GL_LINK')
      ENDIF
      *! B604896,5 MHM [End]
      
      REPLACE cCOGSAcnt WITH GL_LINK.GlAcnt
      
      *! B604896,5 MHM 10/15/2001 IF Not Found get Defulte account [Start]
      *=SEEK(&lcCrMemLin..Gl_Cost+'006','GL_LINK')
      IF !SEEK(&lcCrMemLin..Gl_Cost+'006','GL_LINK')
        =SEEK("DEFDEF"+'006','GL_LINK')
      ENDIF
      *! B604896,5 MHM  [End]
      
      REPLACE cICAcnt   WITH GL_LINK.GlAcnt
      *B602688,1 Reham On 06/13/1999   *** End   ***
      
      DECLARE laGLDistAr[2,13]
      *B602688,1 Reham On 03/21/99   *** Begin ***
      *STORE &lcCrMemLin..GL_Cost TO laGLDistAr[1,1] , laGLDistAr[2,1]
      laGLDistAr[1,1] = &lcCrMemLin..GL_Cost
      laGLDistAr[2,1] = &lcCrMemLin..GL_Sales
      *B602688,1 Send the related account to the function.
      laGLDistAr[1,10] = &lcCrMemLin..cICAcnt
      laGLDistAr[2,10] = &lcCrMemLin..cCOGSAcnt
      *B602688,1 Reham On 03/21/99   *** Begin ***
      
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
    
    *B602732,1 Reham On 04/04/99   *** Begin ***
    *B602732,1 Get the steps counter from the current line
    lnNxtStp = &lcCrMemLin..nSteps + 1
    *B602732,1 Reham On 04/04/99   *** End   ***

    SELECT (lcCrMemLin)
    DECLARE laAdjQty[9]
    FOR lnCount = 1 TO 8
      lcCount = STR(lnCount,1)
      laAdjQty[lnCount] = Qty&lcCount
    ENDFOR
    laAdjQty[9] = TotQty
    
    *B602719,1 Reham On 04/06/99   *** Begin ***
    IF &lcCrMemLin..nSteps < 6
    *B602719,1 Reham On 04/06/99   *** End   ***
      *--Call the global function for update style inventory control.
      *B602322,1 Pass gl session as a parameter.
      *lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
                          &lcCrMemHdr..cWareCode,Dyelot,;
                          CrDate,CrMemo,@laAdjQty,Cost,'',.T.,'',;
                          4,lcCrMemLin,'nSteps',@laGLDistAr)

      *E301200 (AAMER) Start
      *lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
                          &lcCrMemHdr..cWareCode,Dyelot,;
                          CrDate,CrMemo,@laAdjQty,Cost,'',lcRmGlSess,'',;
                          4,lcCrMemLin,'nSteps',@laGLDistAr)

      *Pass lnNxtStp instead of 4 (AAMER) 04/19/999 (Start)
      *lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
                          &lcCrMemHdr..cWareCode,Dyelot,;
                          CrDate,CrMemo,@laAdjQty,Cost,'',lcRmGlSess,'',;
                          4,lcCrMemLin,'nSteps',@laGLDistAr,;
                          VAL(&lcCrMemLin..cRet_LinNo))

      *E301192 AAMER (Start) 
      *lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
                          &lcCrMemHdr..cWareCode,Dyelot,;
                          CrDate,CrMemo,@laAdjQty,Cost,'',lcRmGlSess,'',;
                          lnNxtStp,lcCrMemLin,'nSteps',@laGLDistAr,;
                          VAL(&lcCrMemLin..cRet_LinNo))
      PRIVATE lcRefer
      lcRefer = 'CUST# '+ Customer.Account + "-" + Customer.BTName
      lnNxtStp = gfStyCrl('7',IIF(!EMPTY(cRetSty) , cRetSty , Style), ;
                          &lcCrMemHdr..cWareCode,Dyelot,;
                          CrDate,CrMemo,@laAdjQty,Cost,lcRefer,lcRmGlSess,'',;
                          lnNxtStp,lcCrMemLin,'nSteps',@laGLDistAr,;
                          VAL(&lcCrMemLin..cRet_LinNo))
      *E301192 AAMER (End)

      *Pass lnNxtStp instead of 4 (AAMER) 04/19/999 (End)
      *E301200 (AAMER) End
      *B602322,1 end.
      
      *B602719,1 Reham On 04/06/99   *** Begin ***
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE &lcCrMemLin..nSteps WITH lnNxtStp
      UNLOCK
    ENDIF
    
    IF &lcCrMemLin..nSteps < lnNxtStp + 1
    *B602719,1 Reham On 04/06/99   *** End ***
    
      *-- Update the GL accounts in the credit memo line temp. file.
      
      *! B604896,5 MHM 10/08/2001 we comment this line becuase it is wrong [Start] 
      *! B604896,5                to use it it update wromg Gl Account
      *IF llLink_GL
        *-- Update cost of goods sold account in the credit memo line
        *-- temp. file from the gl distribution temp. file.
      *  SELECT (lcGLDstTmp)
      *  LOCATE FOR Tran_Type = "RM" AND Catg_Key = "008"
      *  IF FOUND()
      *    REPLACE &lcCrMemLin..cCoGsAcnt WITH GLAccount
      *  ENDIF
        
        *-- Update inventory control account in the credit memo line
        *-- temp. file from the gl distribution temp. file.
      *  LOCATE FOR Tran_Type = "RM" AND Catg_Key = "006"
      *  IF FOUND()
      *    REPLACE &lcCrMemLin..cICAcnt WITH GLAccount
      *  ENDIF
      *ENDIF
     *! B604896,5 MHM [End]
     
      *B602719,1 Reham On 04/06/99   *** Begin ***
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE &lcCrMemLin..nSteps WITH lnNxtStp
      UNLOCK
    ENDIF
    *B602719,1 Reham On 04/06/99   *** End   ***
    
    *B602719,1 Reham On 04/06/99   *** Begin  ***
    *IF &lcCrMemLin..nSteps < lnNxtStp + 1
    IF &lcCrMemLin..nSteps < lnNxtStp + 2
    *B602719,1 Reham On 04/06/99   *** End   ***
      *-- Save the current line from the temp. file lines to 
      *-- the master return line file.
      SELECT (lcCrMemLin)
      SCATTER MEMVAR MEMO
      SELECT RETLINE
      APPEND BLANK
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      GATHER MEMVAR MEMO
      *-- Call global function to add audit fields info.
      =gfAdd_Info('RETLINE')
      UNLOCK
      
      *-- Call Global Function to transmit the local data.
      =gfTraceKey("RetLine" , CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD , "A")

      *-- If cancel the rest of open qty.
      IF llCancRest
        SELECT (lcCrMemLin)
        IF Qty1 < nOpnQty1 .OR. Qty2 < nOpnQty2 .OR. Qty3 < nOpnQty3 .OR. ;
           Qty4 < nOpnQty4 .OR. Qty5 < nOpnQty5 .OR. Qty6 < nOpnQty6 .OR. ;
           Qty7 < nOpnQty7 .OR. Qty8 < nOpnQty8
          *B606195,1 (Begin) Cancelled line must have the same no as its original
          *lnLineCnt = lnLineCnt + 1           
          *B606195,1 (End)
          SCATTER MEMVAR MEMO
          *-- Create record in the R/A lines with status cancel has the 
          *-- difference between the open qty. & returned qty.
          SELECT RETLINE
          APPEND BLANK
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          GATHER MEMVAR MEMO
          REPLACE CRET_TRNCD WITH "4" ;
                  CRET_LINNO WITH ALLTRIM(STR(lnLineCnt)) ;
                  QTY1       WITH &lcCrMemLin..nOpnQty1 - &lcCrMemLin..QTY1 ;
                  QTY2       WITH &lcCrMemLin..nOpnQty2 - &lcCrMemLin..QTY2 ;
                  QTY3       WITH &lcCrMemLin..nOpnQty3 - &lcCrMemLin..QTY3 ;
                  QTY4       WITH &lcCrMemLin..nOpnQty4 - &lcCrMemLin..QTY4 ;
                  QTY5       WITH &lcCrMemLin..nOpnQty5 - &lcCrMemLin..QTY5 ;
                  QTY6       WITH &lcCrMemLin..nOpnQty6 - &lcCrMemLin..QTY6 ;
                  QTY7       WITH &lcCrMemLin..nOpnQty7 - &lcCrMemLin..QTY7 ;
                  QTY8       WITH &lcCrMemLin..nOpnQty8 - &lcCrMemLin..QTY8 ;
                  TOTQTY     WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8
          
          *-- Call global function to add audit fields info.
          =gfAdd_Info('RETLINE')
          UNLOCK
          
          *-- Call Global Function to transmit the local data.
          =gfTraceKey("RetLine" , CRMEMO+STYLE+CRET_LINNO+CRET_TRNCD , "A")
          
          *-- Calculate the canceled qty.
          lnTotCancl = lnTotCancl + RETLINE.TotQty
        ENDIF
      ENDIF
      SELECT (lcCrMemLin)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      *B602719,1 Reham On 04/06/99   *** Begin ***
      *REPLACE nSteps WITH lnNxtStp + 1
      REPLACE nSteps WITH lnNxtStp + 2
      *B602719,1 Reham On 04/06/99   *** End   ***
      UNLOCK
    ENDIF
    
    *B602719,1 Reham On 04/06/99   *** Begin ***
    *IF &lcCrMemLin..nSteps < lnNxtStp + 2
    IF &lcCrMemLin..nSteps < lnNxtStp + 3
    *B602719,1 Reham On 04/06/99   *** End   ***
      IF !EMPTY(lcTrYear) .AND. BETWEEN(VAL(lcTrPeriod) , 1 , 13)
        *E301077,4 Reham On 12/27/98   *** Begin ***
        *E301077,4 Open the style history file.
        =gfOpenFile(gcDataDir+'ICSTYHST',gcDataDir+'STYHST','SH')
        *E301077,4 Reham On 12/27/98   *** End   ***
        
        lcTrPeriod = PADL(ALLTRIM(lcTrPeriod) , 2 , "0" )
        
        *-- Update the style history file for current line.
        SELECT ICSTYHST
        IF SEEK(&lcCrMemLin..Style + lcTrYear , "ICSTYHST")
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          REPLACE nRetAmt            WITH nRetAmt + ROUND(&lcCrMemLin..Amount &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
                  nRetQty            WITH nRetQty + &lcCrMemLin..TOTQTY ;
                  nDisAmt            WITH nDisAmt - ROUND(&lcCrMemLin..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
                  nRetAmt&lcTrPeriod WITH nRetAmt&lcTrPeriod + ROUND(&lcCrMemLin..Amount &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
                  nRetQty&lcTrPeriod WITH nRetQty&lcTrPeriod + &lcCrMemLin..TOTQTY ;
                  nDisAmt&lcTrPeriod WITH nDisAmt&lcTrPeriod - ROUND(&lcCrMemLin..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)
          UNLOCK
          
          *-- Call Global Function to transmit the local data.
          =gfTraceKey("ICSTYHST" , &lcCrMemLin..Style + lcTrYear , "M")
        ENDIF
      ENDIF
      SELECT (lcCrMemLin)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      *B602719,1 Reham On 04/06/99   *** Begin ***
      *REPLACE nSteps WITH lnNxtStp + 2
      REPLACE nSteps WITH lnNxtStp + 3
      *B602719,1 Reham On 04/06/99   *** End   ***
      UNLOCK
    ENDIF
    
    lnLineCnt  = lnLineCnt  + 1
    SELECT (lcCrMemLin)
  ENDSCAN

  *B603741,1 WAB - Return the order to the 1st tag (account+style+Cret-linNo+cret_trncd)
  *B603741,1 WAB - StART
  IF !llPontSale
    SELECT (lcCrMemLin)
    SET ORDER TO TAG lcCurrtag
  ENDIF
  *B603741,1 WAB - END

  *-- If the thermometer was not closed, call global function to close it.
  *B802736,1 [Start]the Current RecCount lnCurRec always exceeds the real
  *                 reccount by one (MAN)
  *IF lnCurRec # lnTotRec
  IF lnCurRec-1 # lnTotRec
  *B802736,1 [End]
    =gfTherm(lnTotRec , lnTotRec , "Saving credit memo # : "+&lcCrMemHdr..CrMemo)
  ENDIF
  
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *REPLACE nSteps WITH 3
  REPLACE nSteps WITH 6
  *B602719,1 Reham On 04/05/99   *** End   ***
  UNLOCK
ENDIF

*B602719,1 Reham On 04/05/99   *** Begin ***
*IF &lcCrMemHdr..nSteps < 4
IF &lcCrMemHdr..nSteps < 7
*B602719,1 Reham On 04/05/99   *** End   ***
  *-- Updating General Ledger Distribution File the customer payment transaction.
  IF llLink_GL
    *B602322,1 comment this part since it moved at the bigining of saving.
    *IF !llRmGlSess
    *  lcRmGlSess = gfsequence('GLSESSION')
    *  llRmGlSess = .T.
    *ENDIF
    *B602322,1 End.

    *-- Call local function to update the temp. files & needed variables 
    *-- fields in the uncompleted session file record.
    llNothing = IIF(llPontSale , .F. , lfUpdVars())
    
    *-- Update the Gl distribution file with created entries.
    SELECT (lcGLDstTmp)
    
    *B602719,1 Reham On 04/06/99   *** Begin ***
    *REPLACE ALL GLSESSION WITH lcRmGlSess
    REPLACE ALL GLSESSION WITH lcRmGlSess ;
                cAdd_User WITH gcUser_ID  ;
                dAdd_Date WITH DATE()     ;
                cAdd_Time WITH gfGetTime()
    *B602719,1 Reham On 04/06/99   *** End   ***
    
    USE
    SELECT GLDIST
    APPEND FROM (gcWorkDir+lcGLDstTmp)
    
    *B602719,1 Reham On 04/05/99   *** Begin ***
    *B602719,1 Erase the temp. GLDIST file only if called from point of sale.
    *-- If called from point of sale program.
    IF llPontSale
    *B602719,1 Reham On 04/05/99   *** End   ***
      *-- Erase the temp. file that hold the gl distribution records.
      ERASE &gcWorkDir.&lcGLDstTmp..DBF
      ERASE &gcWorkDir.&lcGLDstTmp..CDX
    *B602719,1 Reham On 04/05/99   *** Begin  ***
    ELSE
      *B602719,1 Recreate the temp. GLDIST.
      =lfCrtUnComp(.F. , .F. , .T.)
    ENDIF
    *B602719,1 Reham On 04/05/99   *** End   ***
  ENDIF
  
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *REPLACE nSteps WITH 4
  REPLACE nSteps WITH 7
  *B602719,1 Reham On 04/05/99   *** End   ***
  UNLOCK
ENDIF

*B602719,1 Reham On 04/05/99   *** Begin ***
*IF &lcCrMemHdr..nSteps < 5
IF &lcCrMemHdr..nSteps < 8
*B602719,1 Reham On 04/05/99   *** End   ***
  *E301077,4 Reham On 12/27/98   *** Begin ***
  *E301077,4 Open the salesreps file.
  =gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
  *E301077,4 Reham On 12/27/98   *** End   ***
  
  *-- Update [COMMISSIONS] Salesreps commissions / chargebacks
  SELECT SALESREP
  
  IF &lcCrMemHdr..CommPcnt1 + &lcCrMemHdr..CommPcnt2 > 0
    FOR lnCount = 1 TO 2
      lcCount = ALLTRIM(STR(lnCount))
      IF !EMPTY(&lcCrMemHdr..SalesRep&lcCount) .AND. &lcCrMemHdr..CommPcnt&lcCount > 0
        IF SEEK(&lcCrMemHdr..SalesRep&lcCount , "SALESREP")
          *-- Calculated the equivalent amount.
          lnFrnAmnt = &lcCrMemHdr..CommAmt&lcCount
          
          lnCommDue = ROUND(&lcCrMemHdr..CommAmt&lcCount &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit , 2)
          lnNewBal = SALESREP.BALANCE  + lnCommDue
          SELECT SALESREP
          *-- Lock the record to grantee the phiscal update.
          = RLOCK()
          REPLACE CURRENT WITH CURRENT + lnCommDue ;
                  BALANCE WITH lnNewBal
          UNLOCK
          *-- Call Global Function to transmit the local data.
          =gfTraceKey("SALESREP" , REPCODE , "M")
          
          IF &lcCrMemHdr..TotCredit > 0
            *E301077,4 Reham On 12/27/98   *** Begin ***
            *E301077,4 Open the salesreps commissions file.
            =gfOpenFile(gcDataDir+'REPCOMM',gcDataDir+'REPCOMM','SH')
            *E301077,4 Reham On 12/27/98   *** End   ***

            SELECT REPCOMM
            APPEND BLANK
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()
            *B604036,1 KAM change replace amount with lnFrnAmnt to
            *B604036,1     replace amount with lnCommDue[start]           
            *REPLACE STATUS     WITH 'O' ;
                    REPCODE    WITH &lcCrMemHdr..SalesRep&lcCount ;
                    ACCOUNT    WITH &lcCrMemHdr..Account ;
                    ORDER      WITH &lcCrMemHdr..Order ;
                    DATE       WITH &lcCrMemHdr..CrDate ;
                    TRANTYPE   WITH '5' ;
                    DESC       WITH 'RETURN/I#'+&lcCrMemHdr..Invoice ;
                    TRAN       WITH &lcCrMemHdr..CrMemo ;
                    CUSTPO     WITH &lcCrMemHdr..CustPo ;
                    COMMPCNT   WITH &lcCrMemHdr..CommPcnt&lcCount ;
                    AMOUNT     WITH lnFrnAmnt ;
                    BALANCE    WITH lnNewBal ;
                    cCurrCode  WITH &lcCrMemHdr..cCurrCode ;
                    nExRate    WITH &lcCrMemHdr..nExRate ;
                    nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ;
                    nForAmnt   WITH &lcCrMemHdr..CommAmt&lcCount
			REPLACE STATUS     WITH 'O' ;
                    REPCODE    WITH &lcCrMemHdr..SalesRep&lcCount ;
                    ACCOUNT    WITH &lcCrMemHdr..Account ;
                    ORDER      WITH &lcCrMemHdr..Order ;
                    DATE       WITH &lcCrMemHdr..CrDate ;
                    TRANTYPE   WITH '5' ;
                    DESC       WITH 'RETURN/I#'+&lcCrMemHdr..Invoice ;
                    TRAN       WITH &lcCrMemHdr..CrMemo ;
                    CUSTPO     WITH &lcCrMemHdr..CustPo ;
                    COMMPCNT   WITH &lcCrMemHdr..CommPcnt&lcCount ;
                    AMOUNT     WITH lnCommDue ;
                    BALANCE    WITH lnNewBal ;
                    cCurrCode  WITH &lcCrMemHdr..cCurrCode ;
                    nExRate    WITH &lcCrMemHdr..nExRate ;
                    nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ;
                    nForAmnt   WITH &lcCrMemHdr..CommAmt&lcCount                    
            *B604036,1 KAM [end]

            
            *-- Call global function to add audit fields info.
            =gfAdd_Info("REPCOMM")
            UNLOCK
            
            *-- Call Global Function to transmit the local data.
            =gfTraceKey("REPCOMM" , REPCODE+DTOS(DATE)+TRAN+TRANTYPE , "A")
            
            *B602641,1 Reham On 03/04/99   *** Begin ***
            =gfCloseFile('REPCOMM')
            *B602641,1 Reham On 03/04/99   *** End   ***
          ENDIF
        ENDIF
      ENDIF
    ENDFOR
  ENDIF
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *REPLACE nSteps WITH 5
  REPLACE nSteps WITH 8
  *B602719,1 Reham On 04/05/99   *** Begin ***
  UNLOCK
ENDIF

*C102676,1 Custom process for A.S.T. [Begin]
IF ASCAN(laEvntTrig,PADR('MEMOREP3',10))<>0
  =gfDoTriger('RMCRMEM',PADR('MEMOREP3',10))
ENDIF  
*C102676,1 Custom process for A.S.T. [End]

*B602719,1 Reham On 04/05/99   *** Begin ***
*IF &lcCrMemHdr..nSteps < 6
IF &lcCrMemHdr..nSteps < 9
*B602719,1 Reham On 04/05/99   *** End   ***
  *-- Update Return Authorization.
  *IF !EMPTY(&lcCrMemHdr..RaNo) .AND. &lcCrMemHdr..cIntr_Inv = "Y"
  IF !EMPTY(&lcCrMemHdr..RaNo)
    SELECT RALINE
    *-- Set order descending into the RALINE file to get the last line.
    SET ORDER TO RALINE DESCENDING
    *-- Get the last line in the R/A.
    lnRaCnt = IIF(SEEK(&lcCrMemHdr..RaNo , "RALINE") , VAL(RALINE.cRa_LinNo) + 1 , 1)
    SET ORDER TO RALINE ASCENDING
    lnTotDedct = 0    && Var. hold the total difference between open & received
    lnAmtDedct = 0    && Var. hold the total amount of difference between open & received
    lnTotRecvd = 0    && Var. hold total received qty.
    
    *B802907,1 Reham On 12/28/1999   *** Begin ***
    lcSaveFile = lcCrMemLin
    lcCrMemLin = lcOpenLine
    *B802907,1 Reham On 12/28/1999   *** End   ***
    SELECT (lcCrMemLin)
    SCAN
      *-- If this line related to the selected invoice & not related to 
      *-- the selected R/A, Add this line with zero qty. in RALINE file.
      IF &lcCrMemLin..lInvoice .AND. ;
         !SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
        SELECT RALINE
        APPEND BLANK
        *-- Lock the record to grantee the phiscal update.
        = RLOCK()
        REPLACE Rano      WITH &lcCrMemHdr..RaNo ;
                Account   WITH &lcCrMemHdr..Account ;
                Style     WITH &lcCrMemLin..Style ;
                cRa_LinNo WITH ALLTRIM(STR(lnRaCnt)) ;
                Dyelot    WITH &lcCrMemLin..Dyelot ;
                Reason    WITH &lcCrMemHdr..Reason ;
                Price     WITH &lcCrMemLin..Price  ;
                Tax_Rate  WITH &lcCrMemLin..Tax_Rate ;
                nPstRate  WITH &lcCrMemLin..nPstRate
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('RALINE')
        UNLOCK
        
        *-- Call Global Function to transmit the local data.
        =gfTraceKey("RALINE" , RANO+STYLE+CRA_LINNO , "A")
        
        *-- Increment the line no. in the raline file.
        lnRaCnt = lnRaCnt + 1
      ENDIF
      
      DO CASE
        *-- If received line.
        CASE &lcCrMemLin..cRet_TrnCd = "2"
          IF SEEK(&lcCrMemHdr..RaNo + &lcCrMemLin..Style , "RALINE")
            SELECT RALINE
            *-- Lock the record to grantee the phiscal update.
            = RLOCK()
            REPLACE nOpnQty1 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty1-&lcCrMemLin..Qty1 , 0)) ;
                    nOpnQty2 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty2-&lcCrMemLin..Qty2 , 0)) ;
                    nOpnQty3 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty3-&lcCrMemLin..Qty3 , 0)) ;
                    nOpnQty4 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty4-&lcCrMemLin..Qty4 , 0)) ;
                    nOpnQty5 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty5-&lcCrMemLin..Qty5 , 0)) ;
                    nOpnQty6 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty6-&lcCrMemLin..Qty6 , 0)) ;
                    nOpnQty7 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty7-&lcCrMemLin..Qty7 , 0)) ;
                    nOpnQty8 WITH IIF(llCancRest , 0 , MAX(&lcCrMemLin..nOpnQty8-&lcCrMemLin..Qty8 , 0)) ;
                    nTotOpnQty WITH nOpnQty1 + nOpnQty2 + nOpnQty3 + nOpnQty4 + ;
                                    nOpnQty5 + nOpnQty6 + nOpnQty7 + nOpnQty8
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
    
    *B802907,1 Reham On 12/28/1999   *** Begin ***
    lcCrMemLin = lcSaveFile
    *B802907,1 Reham On 12/28/1999   *** End   ***
    
    *-- Update the RA header file with the changed fields.
    SELECT RETAUTH
    SET ORDER TO TAG RETAUTHA
    IF SEEK(&lcCrMemHdr..Account + &lcCrMemHdr..RaNo)
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE INVOICE    WITH &lcCrMemHdr..Invoice ;
              TRAN       WITH &lcCrMemHdr..CrMemo ;
              ORDER      WITH &lcCrMemHdr..Order ;
              CUSTPO     WITH &lcCrMemHdr..CustPo ;
              RETDATE    WITH &lcCrMemHdr..CrDate ;
              RETURN     WITH RETURN + &lcCrMemHdr..Pieces ;
              RETURNAMT  WITH RETURNAMT + &lcCrMemHdr..Gross_Amt ;
              cCurrCode  WITH &lcCrMemHdr..cCurrCode ;
              nExRate    WITH &lcCrMemHdr..nExRate ;
              nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ;
              nreta_opn  WITH IIF(llCancRest , 0 , MAX(nreta_opn-lnTotDedct , 0)) ;
              nreta_rec  WITH nreta_rec + lnTotRecvd ;
              nreta_can  WITH nreta_can + lnTotCancl ;
              nRtOpnAmt  WITH IIF(llCancRest , 0 , MAX(nRtOpnAmt-lnAmtDedct , 0)) ;
              STATUS     WITH IIF(llCancRest .OR. nreta_opn = 0 , "C" , STATUS)
      UNLOCK
      
      *-- Call Global Function to transmit the local data.
      =gfTraceKey("RETAUTH" , RANO , "M")
    ENDIF
    SET ORDER TO TAG RETAUTH
  ENDIF
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *REPLACE nSteps WITH 6
  REPLACE nSteps WITH 9
  *B602719,1 Reham On 04/05/99   *** End   ***
  UNLOCK
ENDIF

*-- Post customer's A/R
llPost = IIF(llPostfInv .AND. !EMPTY(&lcCrMemHdr..cFacCode) , .F. , .T.)

IF llPost .AND. &lcCrMemHdr..TotCredit <> 0
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *IF &lcCrMemHdr..nSteps < 7
  IF &lcCrMemHdr..nSteps < 10
  *B602719,1 Reham On 04/05/99   *** End   ***
    *E301077,4 Reham On 12/27/98   *** Begin ***
    *E301077,4 Open the credit file.
    =gfOpenFile(gcDataDir+'CREDIT',gcDataDir+'CREDIT','SH')
    *E301077,4 Reham On 12/27/98   *** End   ***
    
    lnTotCredit = ABS(&lcCrMemHdr..TotCredit) * -1
    
    *-- Add record in the credit file for the total credit amount.
    SELECT CREDIT
    APPEND BLANK
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    REPLACE TRAN       WITH &lcCrMemHdr..CrMemo ;
            cFacCode   WITH &lcCrMemHdr..cFacCode ;
            ACCOUNT    WITH &lcCrMemHdr..Account ;
            TRANDATE   WITH &lcCrMemHdr..CrDate ;
            dPostDate  WITH &lcCrMemHdr..dPostDate ;
            TRANTYPE   WITH '0' ;
            CCREDITCOD WITH &lcCrMemHdr..Reason ;
            DESC       WITH 'RET.INV#'+&lcCrMemHdr..Invoice ;
            STORE      WITH &lcCrMemHdr..Store ;
            DSC_AMT    WITH ABS(lnTTrdeAmt) * -1 ;
            cCurrCode  WITH &lcCrMemHdr..cCurrCode ;
            nExRate    WITH &lcCrMemHdr..nExRate ;
            nCurrUnit  WITH &lcCrMemHdr..nCurrUnit ;
            AMOUNT     WITH lnTotCredit ;
            cArGlAcc   WITH &lcCrMemHdr..cArAcnt
            
    *-- Call global function to add audit fields info.
    =gfAdd_Info("CREDIT")
  
    IF !EMPTY(&lcCrMemHdr..RaNo) .AND. EMPTY(&lcCrMemHdr..Reference)
      REPLACE REFERENCE WITH 'R/A# '+&lcCrMemHdr..RaNo
    ELSE
      IF EMPTY(&lcCrMemHdr..RaNo) .AND. !EMPTY(&lcCrMemHdr..Reference)
        REPLACE REFERENCE WITH &lcCrMemHdr..Reference
      ELSE
        IF !EMPTY(&lcCrMemHdr..RaNo) .AND. !EMPTY(&lcCrMemHdr..Reference)
          REPLACE REFERENCE WITH 'RA'+&lcCrMemHdr..RaNo+'/'+&lcCrMemHdr..Reference
        ELSE
          REPLACE REFERENCE WITH ' '
        ENDIF
      ENDIF
    ENDIF
    UNLOCK
    
    *-- Call Global Function to transmit the local data.
    =gfTraceKey("CREDIT" , TRANTYPE+TRAN , "A")
    
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *B602719,1 Reham On 04/05/99   *** Begin ***
    *REPLACE nSteps WITH 7
    REPLACE nSteps WITH 10
    *B602719,1 Reham On 04/05/99   *** Begin ***
    UNLOCK
  ENDIF
  
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *IF &lcCrMemHdr..nSteps < 8
  IF &lcCrMemHdr..nSteps < 11
  *B602719,1 Reham On 04/05/99   *** End   ***
    *-- Compute new aged A/R applying credits to oldest balance first
    *-- Return new customer a/r balance.
    SELECT CUSTOMER
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *-- Calculated the equivalent amount.
    REPLACE OPENCR  WITH OPENCR + ROUND((ABS(&lcCrMemHdr..TotCredit) * -1) &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
            NETBAL  WITH TOTAGE + OPENCR
    UNLOCK
    
    *E301245,1 function to update nHgWtrMark Field with NETBAL field [Begin.]
    =lfHgWUpdat() 
    *E301245,1 function to update nHgWtrMark Field with NETBAL field [End.]
    
    *-- Call Global Function to transmit the local data.
    =gfTraceKey("CUSTOMER" , "M"+Account+Store , "M")
    
    SELECT (lcCrMemHdr)
    *-- Lock the record to grantee the phiscal update.
    = RLOCK()
    *B602719,1 Reham On 04/05/99   *** Begin ***
    *REPLACE nSteps WITH 8
    REPLACE nSteps WITH 11
    *B602719,1 Reham On 04/05/99   *** End   ***
    UNLOCK
  ENDIF
ENDIF

*B602719,1 Reham On 04/05/99   *** Begin ***
*IF &lcCrMemHdr..nSteps < 9
IF &lcCrMemHdr..nSteps < 12
*B602719,1 Reham On 04/05/99   *** End   ***
  IF !EMPTY(lcTrYear) .AND. BETWEEN(VAL(lcTrPeriod) , 1 , 13)
    *E301077,4 Reham On 12/27/98   *** Begin ***
    *E301077,4 Open the customer history file.
    =gfOpenFile(gcDataDir+'ARCUSHST',gcDataDir+'ACTHST','SH')
    *E301077,4 Reham On 12/27/98   *** End   ***
    
    lcTrPeriod = PADL(ALLTRIM(lcTrPeriod) , 2 , "0" )
    *-- Update the customer history file.
    SELECT ARCUSHST
    IF SEEK(&lcCrMemHdr..Account + lcTrYear , "ARCUSHST")
      *-- Lock the record to grantee the phiscal update.
      = RLOCK()
      REPLACE nRetAmt            WITH nRetAmt + ROUND(&lcCrMemHdr..Gross_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
              nRetQty            WITH nRetQty + &lcCrMemHdr..Pieces ;
              nDisAmt            WITH nDisAmt - ROUND(&lcCrMemHdr..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
              nRetAmt&lcTrPeriod WITH nRetAmt&lcTrPeriod + ROUND(&lcCrMemHdr..Gross_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2) ;
              nRetQty&lcTrPeriod WITH nRetQty&lcTrPeriod + &lcCrMemHdr..Pieces ;
              nDisAmt&lcTrPeriod WITH nDisAmt&lcTrPeriod - ROUND(&lcCrMemHdr..Disc_Amt &lcExRSin &lcCrMemHdr..nExRate &lcUntSin &lcCrMemHdr..nCurrUnit,2)
      UNLOCK
      
      *-- Call Global Function to transmit the local data.
      =gfTraceKey("ARCUSHST" , &lcCrMemHdr..Account + lcTrYear , "M")
    ENDIF
  ENDIF
  SELECT (lcCrMemHdr)
  *-- Lock the record to grantee the phiscal update.
  = RLOCK()
  *B602719,1 Reham On 04/05/99   *** Begin ***
  *REPLACE nSteps WITH 9
  REPLACE nSteps WITH 12
  *B602719,1 Reham On 04/05/99   *** End   ***
  UNLOCK
ENDIF

*!*************************************************************
*! Name      : lfGetTrdDs
*! Developer : Reham Al-Allamy
*! Date      : 06/16/1998
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
*
FUNCTION lfGetTrdDs

*E301490,1 AHM 11/27/2000 add new parameter to get the return value through it
*E301490,1                in case of calling from outside program
*E301490,1 (Start)
*PARAMETERS lcCurTerm , lcCurInv
PARAMETERS lcCurTerm , lcCurInv , lnInvTrdDs
*E301490,1 (End)

PRIVATE lcCurTerm , lcCurInv

lnInvTrdDs = 0

*-- If there is invoice #.
IF !EMPTY(lcCurInv)
  *-- If the terms used in the invoice changed.
  IF !(lcCurTerm == lcInvTerms)
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
    *E301077,4 Reham On 12/27/98   *** Begin ***
    *E301077,4 Open the invoice header file.
    =gfOpenFile(gcDataDir+'INVHDR',gcDataDir+'INVHDR','SH')
    *E301077,4 Reham On 12/27/98   *** End   ***
    *-- If the terms code used in the invoice does not change, get the 
    *-- trade discount saved in the invoice header file.
    lnInvTrdDs = IIF(SEEK(lcCurInv , "INVHDR") , InvHdr.Trde_Disc , 0)
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

RETURN lnInvTrdDs

*!*************************************************************
*! Name      : lfCrtUnComp
*! Developer : Reham Al-Allamy
*! Date      : 12/21/1997
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
*
FUNCTION lfCrtUnComp
*B602719,1 Reham On 04/05/99   *** Begin ***
*B602719,1 Add new parameter for GLDIST file.
*PARAMETERS llHeader , llDetails
*PRIVATE llHeader , llDetails
*B802907,1 Reham On 12/28/1999   *** Begin ***
*PARAMETERS llHeader , llDetails , llGLDist

*E301490,1 (Start) Add new parameters Called from EDI
*PARAMETERS llHeader , llDetails , llGLDist , llOpenLine
PARAMETERS llHeader , llDetails , llGLDist , llOpenLine , llFromEDI
*E301490,1 (End) Add new parameters Called from EDI

PRIVATE llHeader , llDetails , llGLDist , llOpenLine
*B802907,1 Reham On 12/28/1999   *** Begin ***
*B602719,1 Reham On 04/05/99   *** Begin ***

*-- If parameter send to create the temp. header file, create it from the 
*-- master Return header file.
IF llHeader
  SELECT RETHDR
  =AFIELDS(laCrMemHdr)
  lnCrMemHdr = ALEN(laCrMemHdr , 1)
  *-- Add field to know at which step the saving has done.
  DIMENSION laCrMemHdr [lnCrMemHdr+1,4]
  laCrMemHdr[lnCrMemHdr + 1 , 1] = 'NSTEPS'
  laCrMemHdr[lnCrMemHdr + 1 , 2] = 'N'
  laCrMemHdr[lnCrMemHdr + 1 , 3] = 6
  laCrMemHdr[lnCrMemHdr + 1 , 4] = 0
  *-- Call global function to create the temp. header file.
  =gfCrtTmp(lcCrMemHdr , @laCrMemHdr , [ACCOUNT] , lcCrMemHdr)
ENDIF

*B802907,1 Reham On 12/28/1999   *** Begin ***
*-- If parameter send to create the temp. lines file, create it from the 
*-- master Return lines file.
*IF llDetails
*B802907,1 Reham On 12/28/1999   *** Begin ***

*-- Temp. name for the lines file.
SELECT RETLINE
=AFIELDS(laCrMemLin)
lnCrMemLin = ALEN(laCrMemLin , 1)
*B602932,1 Reham On 05/26/1999   *** Begin ***
*B602932,1 Increase the array dimension to add new logical field to the temp. file.
*-- Add 8 fields for open qty. to the lines file.
*DIMENSION laCrMemLin [lnCrMemLin+11,4]

*E301490,1 Modify array diminsion
*DIMENSION laCrMemLin [lnCrMemLin+12,4]
IF llFromEdi
  DIMENSION laCrMemLin [lnCrMemLin+12,16]
ELSE
 DIMENSION laCrMemLin [lnCrMemLin+12,4]  
ENDIF
*E301490,1 (End)

*B602932,1 Reham On 05/26/1999   *** End   ***
FOR lnCount = 1 TO 8
  lcCount = ALLTRIM(STR(lnCount))
  laCrMemLin[lnCrMemLin + lnCount , 1] = 'NOPNQTY&lcCount'
  laCrMemLin[lnCrMemLin + lnCount , 2] = 'N'
  laCrMemLin[lnCrMemLin + lnCount , 3] = 6
  laCrMemLin[lnCrMemLin + lnCount , 4] = 0
  *E301490,1 Modify array diminsion
  IF llFromEdi
    laCrMemLin[lnCrMemLin + lnCount , 5] = .F.
    laCrMemLin[lnCrMemLin + lnCount , 6] = .F.
    laCrMemLin[lnCrMemLin + lnCount , 7] = ''
    laCrMemLin[lnCrMemLin + lnCount , 8] = ''
    laCrMemLin[lnCrMemLin + lnCount , 9] = ''
    laCrMemLin[lnCrMemLin + lnCount ,10] = ''
    laCrMemLin[lnCrMemLin + lnCount ,11] = ''
    laCrMemLin[lnCrMemLin + lnCount ,12] = ''
    laCrMemLin[lnCrMemLin + lnCount ,13] = ''
    laCrMemLin[lnCrMemLin + lnCount ,14] = ''
    laCrMemLin[lnCrMemLin + lnCount ,15] = ''
    laCrMemLin[lnCrMemLin + lnCount ,16] = ''
  ENDIF
  *E301490,1 (End)
ENDFOR
*-- Add field for total open qty. to the lines file.
laCrMemLin[lnCrMemLin + 9 , 1] = 'NTOTOPNQTY'
laCrMemLin[lnCrMemLin + 9 , 2] = 'N'
laCrMemLin[lnCrMemLin + 9 , 3] = 6
laCrMemLin[lnCrMemLin + 9 , 4] = 0
*E301490,1 Modify array diminsion
IF llFromEdi
  laCrMemLin[lnCrMemLin + 9 , 5] = .F.
  laCrMemLin[lnCrMemLin + 9 , 6] = .F.
  laCrMemLin[lnCrMemLin + 9 , 7] = ''
  laCrMemLin[lnCrMemLin + 9 , 8] = ''
  laCrMemLin[lnCrMemLin + 9 , 9] = ''
  laCrMemLin[lnCrMemLin + 9 ,10] = ''
  laCrMemLin[lnCrMemLin + 9 ,11] = ''
  laCrMemLin[lnCrMemLin + 9 ,12] = ''
  laCrMemLin[lnCrMemLin + 9 ,13] = ''
  laCrMemLin[lnCrMemLin + 9 ,14] = ''
  laCrMemLin[lnCrMemLin + 9 ,15] = ''
  laCrMemLin[lnCrMemLin + 9 ,16] = ''
ENDIF
*E301490,1 (End)

*-- Add logical field to know if we need to add the valid lines of the 
*-- selected invoice in the RA line file.
laCrMemLin[lnCrMemLin + 10 , 1] = 'LINVOICE'
laCrMemLin[lnCrMemLin + 10 , 2] = 'L'
laCrMemLin[lnCrMemLin + 10 , 3] = 1
laCrMemLin[lnCrMemLin + 10 , 4] = 0
*E301490,1 Modify array diminsion
IF llFromEdi
  laCrMemLin[lnCrMemLin + 10 , 5] = .F.
  laCrMemLin[lnCrMemLin + 10 , 6] = .F.
  laCrMemLin[lnCrMemLin + 10 , 7] = ''
  laCrMemLin[lnCrMemLin + 10 , 8] = ''
  laCrMemLin[lnCrMemLin + 10 , 9] = ''
  laCrMemLin[lnCrMemLin + 10 ,10] = ''
  laCrMemLin[lnCrMemLin + 10 ,11] = ''
  laCrMemLin[lnCrMemLin + 10 ,12] = ''
  laCrMemLin[lnCrMemLin + 10 ,13] = ''
  laCrMemLin[lnCrMemLin + 10 ,14] = ''
  laCrMemLin[lnCrMemLin + 10 ,15] = ''
  laCrMemLin[lnCrMemLin + 10 ,16] = ''
ENDIF
*E301490,1 (End)

*-- Add field to know at which step the saving has done.
laCrMemLin[lnCrMemLin + 11 , 1] = 'NSTEPS'
laCrMemLin[lnCrMemLin + 11 , 2] = 'N'
laCrMemLin[lnCrMemLin + 11 , 3] = 6
laCrMemLin[lnCrMemLin + 11 , 4] = 0
*E301490,1 Modify array diminsion
IF llFromEdi
  laCrMemLin[lnCrMemLin + 11 , 5] = .F.
  laCrMemLin[lnCrMemLin + 11 , 6] = .F.
  laCrMemLin[lnCrMemLin + 11 , 7] = ''
  laCrMemLin[lnCrMemLin + 11 , 8] = ''
  laCrMemLin[lnCrMemLin + 11 , 9] = ''
  laCrMemLin[lnCrMemLin + 11 ,10] = ''
  laCrMemLin[lnCrMemLin + 11 ,11] = ''
  laCrMemLin[lnCrMemLin + 11 ,12] = ''
  laCrMemLin[lnCrMemLin + 11 ,13] = ''
  laCrMemLin[lnCrMemLin + 11 ,14] = ''
  laCrMemLin[lnCrMemLin + 11 ,15] = ''
  laCrMemLin[lnCrMemLin + 11 ,16] = ''
ENDIF
*E301490,1 (End)

*B602932,1 Reham On 05/26/1999   *** Begin ***
*B602932,1 Add logical field to know if this style was shipped to this customer before or not.
laCrMemLin[lnCrMemLin + 12 , 1] = 'LSHIPPED'
laCrMemLin[lnCrMemLin + 12 , 2] = 'L'
laCrMemLin[lnCrMemLin + 12 , 3] = 0
laCrMemLin[lnCrMemLin + 12 , 4] = 0
*E301490,1 Modify array diminsion
IF llFromEdi
  laCrMemLin[lnCrMemLin + 12 , 5] = .F.
  laCrMemLin[lnCrMemLin + 12 , 6] = .F.
  laCrMemLin[lnCrMemLin + 12 , 7] = ''
  laCrMemLin[lnCrMemLin + 12 , 8] = ''
  laCrMemLin[lnCrMemLin + 12 , 9] = ''
  laCrMemLin[lnCrMemLin + 12 ,10] = ''
  laCrMemLin[lnCrMemLin + 12 ,11] = ''
  laCrMemLin[lnCrMemLin + 12 ,12] = ''
  laCrMemLin[lnCrMemLin + 12 ,13] = ''
  laCrMemLin[lnCrMemLin + 12 ,14] = ''
  laCrMemLin[lnCrMemLin + 12 ,15] = ''
  laCrMemLin[lnCrMemLin + 12 ,16] = ''
ENDIF
*E301490,1 (End)

*B602932,1 Reham On 05/26/1999   *** End   ***

*B802907,1 Reham On 12/28/1999   *** Begin ***
*-- If parameter send to create the temp. lines file, create it from the 
*-- master Return lines file.
IF llDetails
*B802907,1 Reham On 12/28/1999   *** Begin ***
  *-- Call global function to create the temp. lines file.

  *B039660,1 NNA 06/06/2006 (Begin) convert next trigger to be done within Binmain.Prg instead of Davmain.prg 
  *C037816,1 MHM 04/06/2004 Add Custom Fields[Start]
  IF ASCAN(laEvntTrig,PADR('ADDRMFLD',10))<>0
    =gfDoTriger('RMCRMEM',PADR('ADDRMFLD',10))
  ENDIF
  *C037816,1 MHM [End]
  *B039660,1 NNA (End)
  
  *B603741,1 WAB - create the temp file with 2 index tag 
  *B603741,1 WAB - StART
  *=gfCrtTmp(lcCrMemLin , @laCrMemLin , [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD] , lcCrMemLin)
  DECLARE laIndexTag[2,2]
  laIndexTag[1,1] = [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD]
  laIndexTag[1,2] = lcCrMmStyl 
  *B603939,1 WAB (START)- Change the index to val(CRET_LINNO) to sort correctly
  *laIndexTag[2,1] = [ACCOUNT+CRET_LINNO+STYLE]
  laIndexTag[2,1] = [ACCOUNT+STR(VAL(CRET_LINNO),4)+STYLE]
  *B603939,1 WAB (END)
  laIndexTag[2,2] = lcCrMmLine 
  =gfCrtTmp(lcCrMemLin , @laCrMemLin , @laIndexTag )  
  *B603741,1 WAB - END

ENDIF

*B802907,1 Reham On 12/26/1999   *** Begin ***
IF llOpenLine
  =gfCrtTmp(lcOpenLine , @laCrMemLin , [ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD] , lcOpenLine)
ENDIF
*B802907,1 Reham On 12/26/1999   *** End   ***

*B602719,1 Reham On 04/05/99   *** Begin ***
*B602719,1 Create the GLDIST file only if linked to GL.
IF llLink_GL .AND. llGLDist
  SELECT GLDIST
  =AFIELDS(laGLDstTmp)
  *-- Call global function to create the temp. header file.
  =gfCrtTmp(lcGLDstTmp , @laGLDstTmp , [GLaccount] , lcGLDstTmp)
ENDIF
*B602719,1 Reham On 04/05/99   *** End   ***
*!**************************************************************************
*! Name      : lfHgWUpdat
*! Developer : Sameh (SSE)
*! Date      : 06/09/1999
*! Purpose   : Update nHgWtrMark field (Customer) with NETBAL, if NETBAL is greater
*!*************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************************
*! Passed Parameters  : None
*!*************************************************************************
*! Returns            : None
*!*************************************************************************
*! Example   : =lfHgWUpdat()
*!*************************************************************************
*E301245,1

FUNCTION lfHgWUpdat
=RLOCK()
REPLACE nHgWtrMark WITH IIF(NETBAL>nHgWtrMark,NETBAL,nHgWtrMark)
UNLOCK

*-- End of lfHgWUpdat.

*!*************************************************************
*! Name        : lfGetHST
*! Developer   : Mohamed Shokry (MHM)
*! Date        : 11/18/2007
*! Purpose     : To get the HST from Codes file
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Refer to    : 
*!*************************************************************
*! Example     : 
*!*************************************************************
FUNCTION lfGetHST
lcAlias = ALIAS()

SELECT CODES
lcOldOrd = ORDER()
SET ORDER TO Codes
llRpHSTTax = .F.
IF SEEK("N"+ LEFT(Customer.cAddress4,6)+"YSTATE")
  SCAN REST WHILE cdefcode+ccode_no+crltfield+cfld_name = "N"+ LEFT(Customer.cAddress4,6)+"YSTATE"
    IF crltd_nam = "LHSTTAX   "
       llRpHSTTax = IIF(ALLTRIM(crltd_vlu) = "T",.T.,.F.)
       EXIT
    ENDIF
  ENDSCAN  
ENDIF

SELECT CODES
SET ORDER TO &lcOldOrd 

SELECT (lcAlias)

*:*************************************************************
