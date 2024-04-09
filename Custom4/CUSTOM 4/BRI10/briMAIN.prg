*!***************************************************************************************************************************************
*! Name      : MEMMAIN.PRG
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/05/2009
*! Purpose   : BRI10 Main Program (A4: C201093,A27:C201092){T20080711.0012}
*!***************************************************************************************************************************************
*! Parameters: loFormSet -> FormSet object
*!       lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!***************************************************************************************************************************************
*! Returns   : Logical value.     
*!***************************************************************************************************************************************
*! Modifcations :
*B609574,1 TMI 04/24/2011 Add a custom index to INVHDR for BRI10 [T20110407.0043]
*!***************************************************************************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars  


lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!**********************************************************************************
*! Name      : lfADDPRFBUT
*: Developer : Mariam Mazhar[MMT]
*: Date      : 01/05/2009
*! Purpose   : function to Change the property of Has Prof. in formset of Sku Screen
*!**********************************************************************************
FUNCTION lfADDPRFBUT
loFormSet.llHaveProfile = .T.
loFormSet.lcprofilekey = "ALLTRIM(thisformset.AriaForm1.kbAccountCode.KeyTextbox.value)+PADR(thisformset.AriaForm1.kbSkuId.KeyTextbox.value,8)"
loFormSet.lcProfiletype = 'ST'
loFormSet.otoolbar.buttonrefresh 


*!**********************************************************************************
*! Name      : lfBRIBROW
*: Developer : Tarek Mohammed Ibrahim
*: Date      : *B609574,1 TMI 04/24/2011 
*! Purpose   : function to build set the order to a custom index created specifically for BRI10 as there is a customer in CUSTOMER file has 59000 invoices in INVHDR file
*!**********************************************************************************
FUNCTION lfBRIBROW
LOCAL lcSvOrd , llConsol,lcOldKey , lnSlct
lnSlct = SELECT()

PRIVATE lcAcc,lcStor
lcAcc  = PADR(ALLTRIM(loFormSet.Ariaform1.kbAccount.keyTextBox.Value),5)
lcStor = PADR(ALLTRIM(loFormSet.Ariaform1.kbStore.keyTextBox.Value),8)

SELECT CUSTOMER
lcOldKey = EVAL(KEY())
=SEEK('M'+lcAcc,'CUSTOMER')
llConsol = CUSTOMER.CONSOL = 'Y'
=SEEK(lcOldKey,'CUSTOMER')

SELECT (lnSlct)

lcSvOrd = ORDER('INVHDR')
lcKey = "lcAcc FOR Invhdr.status<>'V' "
IF !EMPTY(lcStor)
  IF llConsol
    lcKey = lcKey + " AND (STORE=lcStor OR SEEK(INVHDR.INVOICE+lcStor,'CONSINVH'))"
  ELSE
    SET ORDER TO ACCSTORE IN INVHDR
    lcKey = "lcAcc+lcStor FOR Invhdr.status<>'V' "
  ENDIF
ENDIF

*-- Call the global browse.
=gfBrows(lcKey,"invoice","laInvoice")

SET ORDER TO &lcSvOrd IN INVHDR
*- end if FUNCTION lfBRIBROW

