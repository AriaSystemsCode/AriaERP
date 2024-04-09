*!***************************************************************************************************************************************
*! Name      : CHAMAIN.PRG
*! Developer : MOUSTAFA ABOUSHADY (MAA)
*! Date      : 11/15/2016
*! Purpose   : CHA26 Main Program (A4: C201892,A27:C201891){T20161003.0034}
*!***************************************************************************************************************************************
*! Parameters: loFormSet -> FormSet object
*!             lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!***************************************************************************************************************************************
*! Returns   : Logical value.     
*!***************************************************************************************************************************************
*! Modifcations :
*!***************************************************************************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars  


lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**********************************************************************************
*! Name      : lfUpdOrdDisc
*: Developer : MOUSTAFA ABOUSHADY (MAA)
*: Date      : 11/15/2016
*! Purpose   : function to reset the order discount if customer is using discount per line
*!**********************************************************************************
FUNCTION lfUpdOrdDisc
lcOldSel = SELECT(0)
lcOrdAccount = EVALUATE(loFormSet.OFORMENVIRONMENT.lcOrdHdr+'.Account')
SELECT Customer
lcOldkey= EVALUATE(KEY())
=SEEK('M'+lcOrdAccount ,'Customer','Customer')
IF UPPER(Customer.Usr_dfnd2) = 'Y'
   Replace Disc WITH 0 IN (loFormSet.OFORMENVIRONMENT.lcOrdHdr)
ENDIF 
SELECT Customer
=SEEK(lcOldkey,'Customer')
SELECT(lcOldSel)
loFormSet.ariaform1.Ariapageframe1.page3.spnDiscount.Refresh()
ENDFUNC 

*!**********************************************************************************
*! Name      : lfUpdLinDisc
*: Developer : MOUSTAFA ABOUSHADY (MAA)
*: Date      : 11/15/2016
*! Purpose   : function to update the order lines discount if customer is using discount per line
*!**********************************************************************************
FUNCTION lfUpdLinDisc

lcOldSel = SELECT(0)
lcOrdAccount = EVALUATE(loFormSet.OFORMENVIRONMENT.lcOrdHdr+'.Account')
SELECT Customer
lcOldkey= EVALUATE(KEY())
=SEEK('M'+lcOrdAccount ,'Customer','Customer')
lnAccDisc = Customer.Disc
IF UPPER(Customer.Usr_dfnd2) = 'Y'
  REPLACE Disc_Pcnt WITH lnAccDisc ,;
          Price WITH Gros_price *( 1 - (lnAccDisc/100)) IN (loFormSet.oFormEnvironment.lcOrdLine)
  loFormSet.AriaForm1.AriaPageframe1.Page2.Ariaeditregion1.SpnDiscount.Refresh()
  loFormSet.AriaForm1.AriaPageframe1.Page2.Ariaeditregion1.txtNetPrice.Refresh()  
ENDIF
SELECT Customer
=SEEK(lcOldkey,'Customer')
SELECT(lcOldSel)
ENDFUNC 
