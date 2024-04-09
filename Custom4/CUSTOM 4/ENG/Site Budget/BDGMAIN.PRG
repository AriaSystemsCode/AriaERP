*!**************************************************************************
*! Name      : BDGMAIN.PRG
*! Developer : SAB (Saber A.Razek)
*! Date      : 05/13/2012
*! Purpose   : Site Budget Customization
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.       
*!**************************************************************************
* Modifications
* B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[Start]
*!**************************************************************************
PARAMETER loFormSet,lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!*************************************************************
*! Name      : lfSbSavCrMem
*! Developer : Saber A.Razek
*! Date      : 05/13/2012
*! Purpose   : Update Site Budget Info after saving credit memo
*!*************************************************************
PROCEDURE lfSbSavCrMem
IF Customer.nStBdgVal > 0
  oBusObj.CUSTOMER.REPLACE('nStBdgBln WITH Customer.nStBdgBln + &lcCrMemHdr..Amount')
ENDIF

ENDPROC

*!*************************************************************
*! Name      : SbDelCrMem
*! Developer : Saber A.Razek
*! Date      : 05/13/2012
*! Purpose   : Update Site Budget Info after deleting credit memo
*!*************************************************************
PROCEDURE lfSbDelCrMem
* B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[Start]
*IF Customer.nStBdgVal > 0
LOCAL lcAlias, lcOrder, lcKey
lcAlias = SELECT()
SELECT Customer
lcOrder = SET("Order")
lcKey   = EVALUATE(KEY())
IF loFormSet.AriaForm1.rmcrmbus.Customer.Seek(IIF(EMPTY(&lcCrMemHdr..Account), 'M','S')+&lcCrMemHdr..Account+&lcCrMemHdr..Store) .AND. Customer.nStBdgVal > 0
* B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[End]
  *This.CUSTOMER.REPLACE('nStBdgBln WITH Customer.nStBdgBln - &lcCrMemHdr..Amount')
  =loFormSet.AriaForm1.rmcrmbus.Customer.Replace('nStBdgBln WITH Customer.nStBdgBln - &lcCrMemHdr..Amount')
ENDIF

* B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[Start]
SELECT Customer
SET ORDER TO &lcOrder.
=SEEK(lcKey)
SELECT (lcAlias)
* B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[End]        
ENDPROC

*!*************************************************************
*! Name      : SbUpdtOrd
*! Developer : Saber A.Razek
*! Date      : 05/13/2012
*! Purpose   : Update Site Budget Info after Saving Sales Order
*!*************************************************************
PROCEDURE lfSbUpdtOrd
IF &lcOrdHDr..cOrdType='O' .AND. &lcOrdHDr..STATUS <>'B'
  LOCAL lcAlias, lcOrder, lcKey
  lcAlias = SELECT()
  IF !USED('Customer')
    =gfOpenTable('Customer','CUSTOMER','SH')      && TYPE+ACCOUNT+STORE
  ENDIF
  SELECT Customer
  lcOrder = SET("Order")
  lcKey   = EVALUATE(KEY())
  * B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[Start]
  *IF SEEK('M'+&lcOrdHDr..Account) .AND. Customer.nStBdgVal > 0
  IF SEEK(IIF(EMPTY(&lcOrdHDr..Store),'M','S')+&lcOrdHDr..Account+&lcOrdHDr..Store) .AND. Customer.nStBdgVal > 0
  * B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[End]
    IF &lcOrdHDr..Entered > Customer.dStBdgEnDt
      *-Reset Site Budget Info
      LOCAL lnDiff
      lnDiff = (dStBdgEnDt - dStBdgStDt) + 1
      REPLACE dStBdgStDt WITH dStBdgStDt + lnDiff, ;
              dStBdgEnDt WITH dStBdgEnDt + lnDiff, ;
              nStBdgBln  WITH nStBdgVal
    ENDIF
    
    LOCAL lnNewBalance
    lnNewBalance = Customer.nStBdgBln + (IIF(SEEK(&lcOrdHDr..CORDTYPE+&lcOrdHDr..ORDER, 'OrdHdr', 'ORDHDR'), OrdHdr.OpenAmt, 0) - &lcOrdHDr..OpenAmt)       
    REPLACE nStBdgBln WITH MAX(MIN(lnNewBalance, nStBdgVal), 0)        
  ENDIF
  SELECT Customer
  SET ORDER TO &lcOrder.
  =SEEK(lcKey)
  SELECT (lcAlias)
ENDIF

ENDPROC

*!*************************************************************
*! Name      : SbDelOrd
*! Developer : Saber A.Razek
*! Date      : 05/13/2012
*! Purpose   : Update Site Budget Info after deleteing sales order
*!*************************************************************
PROCEDURE lfSbDelOrd
IF OrdHdr.cOrdType='O' .AND. OrdHdr.STATUS <>'B'
  LOCAL lcAlias, lcOrder, lcKey
  lcAlias = SELECT()
  IF !USED('Customer')
    =gfOpenTable('Customer','CUSTOMER','SH')      && TYPE+ACCOUNT+STORE
  ENDIF
  SELECT Customer
  lcOrder = SET("Order")
  lcKey   = EVALUATE(KEY())
  * B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[Start]
  *IF SEEK('M'+OrdHdr.Account) .AND. Customer.nStBdgVal > 0
  IF SEEK(IIF(EMPTY(OrdHdr.Store),'M','S')+OrdHdr.Account+OrdHdr.Store) .AND. Customer.nStBdgVal > 0
  * B609951,1 SAB 05/30/2012 Change Site Budget info on Store Level[End]
    LOCAL lnNewBalance
    lnNewBalance = Customer.nStBdgBln + OrdHdr.OpenAmt
    REPLACE nStBdgBln WITH MAX(MIN(lnNewBalance, nStBdgVal), 0)
  ENDIF
  SELECT Customer
  SET ORDER TO &lcOrder.
  =SEEK(lcKey)
  SELECT (lcAlias)
ENDIF
      
ENDPROC