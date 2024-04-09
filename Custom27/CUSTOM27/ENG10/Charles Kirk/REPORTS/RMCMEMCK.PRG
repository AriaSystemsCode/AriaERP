*:***************************************************************************
*: Program file       : RMCMEMCK.PRG
*: Program desc.      : CUSTOMIZED CREDIT MEMO FORM FOR ENGLAND.
*: Date               : 02/05/2002
*: System             : Aria Advantage Series.
*: Module             : RETURN MERSHANDISE (RM)
*: Developer          : RANIA ABDEL RAZIK (RAE)
*: Tracking Job Number: C200274
*:***************************************************************************
*: Calls : 						
*:    Procedures      : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes              : ....
*:***************************************************************************
*: Example : DO RMCMEMCK
*:***************************************************************************

lcTax_Rate = gfGetMemVar('M_TAX_RATE',gcAct_Comp)
lcVatNo = gfGetMemVar('M_TAX_REFE',gcAct_Comp)

lnTaxRat = 0
DECLARE laTaxRat[1,2]
laTaxRat[1,1] = 'NTAXRATE'
laTaxRat[1,2] = 'lnTaxRat'

DECLARE laCompAdd[7,1]
laCompAdd[7] = "Fax No.    :" + TRANSFORM(lcCompFax,lcPhonPict)
=lfAdrShift('laCompAdd')        && Shift Company address if there is empty line.

*-- Remove '#' sign from all the form
FOR lnCount = 1 TO ALEN(laCompAdd)
  laCompAdd[lnCount,1] = STRTRAN(laCompAdd[lnCount,1],'#','No.')  
ENDFOR

*-- Create Temp File 

CREATE TABLE (gcWorkDir+lcTaxable)  (CRMEMO C(6)  , Taxable N(12,2) , NonTax N(12,2) , ;
                                     Price N(9,2) , TrdeAmt N(7,2))
INDEX ON CRMEMO TAG (lcTaxable) OF (gcWorkDir + lcTaxable + '.CDX')

SELECT RETHDR
SET RELATION TO RETHDR.crmemo INTO RETLINE ADDITIVE
SET RELATION TO RETHDR.crmemo INTO (lcTaxable) ADDITIVE

SCAN FOR &lcRpExp
  lcCrmem = RETHDR.CRMEMO
  lcStore = RETHDR.STORE
  SELECT RETLINE
  SCAN REST WHILE CRMEMO = lcCrmem
    WAIT WINDOW 'Selecting Records For The Report ...' + lcCrmem NOWAIT
    =gfRltFld(STYLE.CTAXCODE , @laTaxRat , 'CTAXCODE')

    SELECT (lcTaxable)
    lnTaxable = 0

    IF SEEK(lcCrmem)
      REPLACE &lcTaxable..TrdeAmt WITH &lcTaxable..TrdeAmt + RETLINE.TRDE_AMT
      FOR lnLop = 1 TO SCALE.CNT
        lcTax = "RETLINE.QTY" + ALLTRIM(STR(lnLop))
        IF BETWEEN(lnLop,STYLE.NTAXBREAK,8) AND IIF(CUSTOMER.TYPE = "M" , !CUSTOMER.lvatexem , lfCustExmt()) AND lnTaxRat > 0
          IF EVAL(lcTax) > 0
            REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + (EVAL(lcTax) * RETLINE.PRICE) ,;
                    &lcTaxable..PRICE   WITH &lcTaxable..PRICE + (EVAL(lcTax) * RETLINE.GROS_PRICE) 
          ENDIF
        ELSE
          REPLACE &lcTaxable..NonTax WITH &lcTaxable..NonTax + (EVAL(lcTax) * RETLINE.PRICE) 
        ENDIF
      ENDFOR
    ELSE 
      APPEND BLANK
      REPLACE &lcTaxable..CRMEMO  WITH RETHDR.CRMEMO ,;
              &lcTaxable..TrdeAmt WITH RETLINE.TRDE_AMT
      FOR lnLop = 1 TO SCALE.CNT
        lcTax = "RETLINE.QTY" + ALLTRIM(STR(lnLop))
        IF BETWEEN(lnLop,STYLE.NTAXBREAK,8) AND IIF(CUSTOMER.TYPE = "M" , !CUSTOMER.lvatexem , lfCustExmt()) AND lnTaxRat > 0
          IF EVAL(lcTax) > 0
            REPLACE &lcTaxable..Taxable WITH &lcTaxable..Taxable + (EVAL(lcTax) * RETLINE.PRICE) ,;
                    &lcTaxable..PRICE   WITH &lcTaxable..PRICE + (EVAL(lcTax) * RETLINE.GROS_PRICE) 
          ENDIF
        ELSE
          REPLACE &lcTaxable..NonTax WITH &lcTaxable..NonTax + (EVAL(lcTax) * RETLINE.PRICE) 
        ENDIF
      ENDFOR
    ENDIF
  ENDSCAN
ENDSCAN


*!*************************************************************
*! Name      : lfCustExmt
*! Developer : RANIA ABDEL RAZIK (RAE)
*! Date      : 02/05/2002
*! Purpose   : Function to check if the customer is exempted or not.
*!*************************************************************
*! Called from : RMCMEMCK.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCustExmt()
*!*************************************************************

FUNCTION lfCustExmt

PRIVATE llReturn , lcAliasX1

M.STORE = lcStore
llReturn = .F.
lcAliasX1 = SELECT(0)
SELECT CUSTOMER
lcKeyx1 = EVAL(KEY())

IF SEEK(IIF(M.STORE = SPACE(8),'M'+M.ACCOUNT,'S'+M.ACCOUNT+M.STORE))
  llReturn = !CUSTOMER.lvatexem
ENDIF

SELECT CUSTOMER
=SEEK(lcKeyx1)

SELECT(lcAliasX1)

RETURN llReturn