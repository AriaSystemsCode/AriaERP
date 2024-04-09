*!**************************************************************************
*! Name      : ERIMAIN.PRG
*! Developer : Hassan Ibrahim Ali [HIA]
*! Date      : 02/13/2007
*! Purpose   : Custom main program for ERIC JAVITS, INC              
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       C120205,1
*!**************************************************************************
*! Modifications
*!***************************************************************************
*!C200759,1 HIA 02/13/2007 Blank the color cost when we add a new color to a Style 
*!C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045]
*!C201898,2 Sara.o 12/06/2016 Add function to validate tax amount entered in Sales order screen UDF[T20161019.0045]
*:***************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue
*:***************************************************************************
*:* Name        : lfClrCost
*:* Developer   : Hassan Ibrahim Ali [HIA]
*:* Date        : 02/13/2007
*:* Purpose     : Blank the Mcost,Nicost,Total Cost,Avg_Cost when we add a new
*:*             : Color for a Style
*:***************************************************************************
*:* Called from : ICSTYLE.PRG
*:***************************************************************************
*:C200459,1
FUNCTION lfClrCost

PRIVATE lcI
STORE '' TO lcI

SELECT STYLE
FOR I = 1 TO 5
  lcI = STR(I,1)
  REPLACE NMCOST&lcI WITH 0 , NICOST&lcI WITH 0
ENDFOR
REPLACE TOTCOST WITH 0 , AVE_COST WITH 0 , GROS_PRICE WITH 0
RETURN

*--END OF FUNCTION lfClrCost
*C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][Start]
*:***************************************************************************
*:* Name        : lfGetTaxAmt
*:* Developer   : Sara Osama
*:* Date        : 11/23/2016
*:* Purpose     : 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
FUNCTION lfGetTaxAmt


*!*	IF Ordhdr.TAXAMOUNT > 0
*!*	  loFormSet.AriaForm1.AriapageFrame1.Page4.cntlnvoicesummary.cntTaxes.txtTaxAmount = Ordhdr.TAXAMOUNT
*!*	ENDIF 
IF !USED('Ordhdr_Tx')
  =gfOpenTable('ORDHDR','ORDHDR','SH','Ordhdr_Tx')
ENDIF
lcTaxRule = ALLTRIM(gfGetMemVar('M_TAX_METH',oAriaApplication.ActiveCompanyID))
loSummaryObj= loformset.ariaForm1.ariapageframe1.page4.cntInvoicesummary
IF gfSeek('O'+EVALUATE(loSummaryObj.InvoiceHeaderFile+'.Order'),'Ordhdr_Tx','ORDHDR')
  IF Ordhdr_Tx.FRGHTAMT > 0  OR  Ordhdr_Tx.TAXAMOUNT > 0
    m.Freight= Ordhdr_Tx.FRGHTAMT
    IF Ordhdr_Tx.TAXAMOUNT > 0
      m.Tax_Amt = Ordhdr_Tx.TAXAMOUNT 
      lnNewTaxRate = ROUND((m.Tax_Amt *100)/((IIF(lcTaxRule='M',loSummaryObj.TaxDueAmount,loSummaryObj.TaxDueAmount+m.Freight+m.Insur+m.Cod)-ABS(loSummaryObj.TaxDueAmount*EVALUATE(loSummaryObj.InvoiceHeaderFile+'.DiscPcnt')/100) )),3)
      REPLACE Tax_Rate WITH lnNewTaxRate 
    ELSE
      IF Ordhdr_Tx.FRGHTAMT > 0 AND EVALUATE(loSummaryObj.InvoiceHeaderFile+'.Tax_Rate') > 0
        m.Tax_Amt =  ROUND(EVALUATE(loSummaryObj.InvoiceHeaderFile+'.Tax_Rate') *(IIF(lcTaxRule='M',loSummaryObj.TaxDueAmount,loSummaryObj.TaxDueAmount+;
        m.Freight+m.Insur+m.Cod)-ABS(loSummaryObj.TaxDueAmount*EVALUATE(loSummaryObj.InvoiceHeaderFile+'.DiscPcnt')/100) )/100,2)
      ENDIF
    ENDIF  
  ENDIF
  IF !EMPTY(Ordhdr_Tx.CCARTRACK)
    *REPLACE CCARTRCKNO WITH Ordhdr_Tx.CCARTRACK
    m.CCARTRCKNO = Ordhdr_Tx.CCARTRACK
    *loSummaryObj.cntTotals.txtUpsTrack.Value = Ordhdr_Tx.CCARTRACK
  ENDIF
ENDIF
*C201898,1 Sara.o 11/20/2016 Custom Import SO program does not work if there is empty lines in the CSV file[T20161019.0045][End]

*!C201898,2 Sara.o 12/06/2016 Add function to validate tax amount entered in Sales order screen UDF[T20161019.0045][Start]
*:***************************************************************************
*:* Name        : lfTaxCheck
*:* Developer   : Sara Osama
*:* Date        : 12/06/2016
*:* Purpose     : Validate UDF Tax amount against customer tax rate
*:***************************************************************************
FUNCTION lfTaxCheck
PARAMETERS lcDummy
lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.
lnTaxValue = loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6]
IF lnTaxValue > 0
  lnCurrDataSess = SET("Datasession")
  lnDataSessCnt  = 1
  llFound = .F.
  DO WHILE (!USED(lcBaseFile))
    TRY 
      SET DATASESSION TO lnDataSessCnt  
    CATCH
      EXIT 
    ENDTRY
    IF USED(lcBaseFile)
      llFound = .T.
      EXIT 
    ENDIF
    lnDataSessCnt  = lnDataSessCnt + 1
  ENDDO
  lcAccount = ''
  lcStore = '' 
  IF llFound 
    lcAccount = &lcBaseFile..Account
    lcStore = &lcBaseFile..Store
  ENDIF
  SET DATASESSION TO lnCurrDataSess 
  IF !EMPTY(lcAccount)
    IF !USED('Customer')
      =gfOpenTable('Customer','Customer')
    ENDIF  
    =gfSeek(IIF(!EMPTY(lcStore),"S","M")+lcAccount+IIF(!EMPTY(lcStore ),lcStore ,""),'CUSTOMER','CUSTOMER')
    IF Customer.ntaxrate > 0 AND lnTaxValue > 0
      lnChoice  =gfModalGen("INM00000B44009","Dialog","","","Customer "+lcAccount+" has tax rate "+ALLTRIM(STR(Customer.ntaxrate,6,3))+;
  	                                                       " and the order has tax amount "+ALLTRIM(STR(lnTaxValue ,13,2))+;
  	                                                       ", would you like to use order tax amount?")
      DO CASE
        CASE lnChoice = 1
         *Yes
        CASE lnChoice = 2
         *No 
         loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = 0
      ENDCASE
    ENDIF
  ENDIF
ENDIF
*!C201898,2 Sara.o 12/06/2016 Add function to validate tax amount entered in Sales order screen UDF[T20161019.0045][End]