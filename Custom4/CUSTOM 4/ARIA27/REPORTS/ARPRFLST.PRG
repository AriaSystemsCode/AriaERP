*:***************************************************************************
*: Program file  : ARPRFLST.PRG
*: Program desc. : Profile Listing Report.
*: 				   The program is a conversion from 2.6 ..CONT920.PRG
*! Date          : 07/06/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: New Program 000151
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPRFLST
*:***************************************************************************
*B605440,1 NAD 01/29/2001   FIX BUGS IN THE REALEASE 
*B038431,1 ASH 08/20/2004 while choosing the account code and press ESC an error message appeared.
*:***************************************************************************

*--Get the full picture of the date.
lcCentury = SET('CENTURY')
SET CENTURY ON

*-- Collectting the data.
*B605440,1 NAD       FIX BUGS IN THE REALEASE 
*lcorder = IIF(lcSortBy = 'A' , 'Account,Cpro_code' , 'Cpro_code,Account')
lcorder = IIF(lcRpSortBy = 'A' , 'Account,Cpro_code' , 'Cpro_code,Account')
*B605440,1 NAD      FIX BUGS IN THE REALEASE 
*-- Get the value of the account variables.
lnDataPos = ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT') 
IF lnDataPos > 0
  lnDataPos  = ASUBSCRIPT(laOgFxFlt,lnDataPos,1)
  lcFaccount = SUBSTR(laOgFxFlt[lnDataPos,6],1,5)
  lcTaccount = SUBSTR(laOgFxFlt[lnDataPos,6],7,5)
ENDIF

SELECT DISTINCT Account, Btname ,profile.CPRO_CODE, CDISCREP ,profile.dpro_date ,profile.cpro_value ;
FROM   Customer , Profile, Codes ;
WHERE  Customer.Type = 'M' ;
     AND IIF(EMPTY(lcFAccount) AND EMPTY(lcTAccount) , Customer.account = SUBSTR(ProFile.cCont_Id,1,5),;
         IIF(EMPTY(lcFAccount) AND !EMPTY(lcTAccount), Customer.Account <=lcTAccount AND Customer.account = SUBSTR(ProFile.cCont_Id,1,5) , BETWEEN(Customer.Account,lcFAccount,lcTAccount) AND Customer.account = SUBSTR(ProFile.cCont_Id,1,5) ));
     AND Profile.cconttype+Profile.ccont_id+Profile.store+Profile.cpro_code+Profile.cpro_value=;
         'C'+Customer.Account ;
     AND CODES.cdefcode+CODES.cfld_name+CODES.ccode_no+CODES.cdiscrep+CODES.crltd_nam = ;
         'N'+'CPRO_CODE '+ Profile.cpro_code;
INTO TABLE (GCWORKDIR+lcTempFile);
ORDER BY &lcorder

IF _TALLY = 0
  *---Text : 'No Record Selected for the report..!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN
ENDIF

DO gfDispRe WITH EVALUATE('lcRpForm')
SET CENTURY &lcCentury

*!*************************************************************
*! Name      : lfvAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Validation function for the Account field
*!*************************************************************
*! Called from : Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvAccount

PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF

*!**************************************************************************
*! Name      : lfsAccount
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 07/04/2000
*! Purpose   : Directs Customer File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsAccount()
*!**************************************************************************
*! Note      : SR symbol is [S,Set--R,Reset]
*!**************************************************************************
FUNCTION lfsAccount
PARAMETERS lcParm

IF lcParm = 'S'  && Set code
  SELECT Customer
  GO TOP
ENDIF    
*-- End of lfsAccount.

*B038431,1 ASH 08/20/2004 
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Haytham El_Sheltawi
*! Date      : 01/11/1998
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
