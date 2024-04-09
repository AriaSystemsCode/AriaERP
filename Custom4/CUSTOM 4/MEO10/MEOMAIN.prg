*!**************************************************************************
*! Name      : MEOMAIN.PRG
*! Developer : MMT (Mariam Mazhar)
*! Date      : 10/17/2010
*! Purpose   : MEO10 Custom Process Program.
*!  C201274,C201275  ==> for Aria4  attachments
*!  C201273  ==> for Aria27 attachments
*! Ticket id T20100804.0014
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
* Modifications
*!**************************************************************************

PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfCPYSHPACC
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 10/17/2010
*:* Purpose     : copy shipper account from main record to store record
*:***************************************************************************
FUNCTION lfCPYSHPACC
IF loFormSet.activemode = 'A' AND !EMPTY(loFormSet.ariaForm1.keyStore.keytextbox.Value)
  IF !USED('Customer_ship')
    =gfOpenTable('CUSTOMER','CUSTOMER','SH','Customer_ship')
  ENDIF
  =gfSeek('M'+loFormSet.ariaForm1.keyAccount.keytextbox.Value,'Customer_ship')
  REPLACE customer.cshpacntno WITH Customer_ship.cshpacntno
ENDIF 

*:**************************************************************************
*:* Name        : lfCUSTSHPACC
*:* Developer   : MMT - Mariam Mazhar
*:* Date        : 10/17/2010
*:* Purpose     : copy shipper account from customer record to order header record
*:***************************************************************************
FUNCTION lfCUSTSHPACC
IF loFormSet.activemode = 'A'
  IF !USED('Customer_ship')
    =gfOpenTable('CUSTOMER','CUSTOMER','SH','Customer_ship')
  ENDIF
  lcOrdHdrFile = loFormSet.OFORMENVIRONMENT.lcOrdHdr
  IF !EMPTY(loFormSet.ariaform1.keyStore.keytextbox.Value)
    =gfSeek('S'+PADR(loFormSet.ariaform1.keyAccount.keytextbox.Value,5)+;
    			loFormSet.ariaform1.keyStore.keytextbox.Value,'Customer_ship')
  ELSE
    =gfSeek('M'+PADR(loFormSet.ariaform1.keyAccount.keytextbox.Value,5),'Customer_ship')    
  ENDIF
  REPLACE cshpacntno WITH Customer_ship.cshpacntno IN (lcOrdHdrFile)
ENDIF 
