*:***************************************************************************
*: Program file  : SOSSCTS
*: Program desc. : Send Shipment Confirmation To Seta
*: Module        : Sales Order (SO)
*: Developer     : HIA(HMA)
*: Tracking Job Number: C201469
*: Date : 04/02/2012
*:***************************************************************************
*: Calls :
*:    Programs   : ....
*:    Screens    : ....
*:    Global Functions  : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO SOSSCTS
*:***************************************************************************
*: Modification:
*:**************************************************************************
*----------------------- Report Code Begin -----------------------------
FUNCTION lfvOutPth

LOCAL lcExt, lcDirect
lcExt    = UPPER(ALLTRIM(SUBSTR(lcRpOutNam,AT('.',lcRpOutNam))))
lcDirect = SUBSTR(lcRpOutNam,1,AT('\',lcRpOutNam,OCCURS('\',lcRpOutNam)))

IF '?' $ lcRpOutNam OR (!EMPTY(lcRpOutNam) AND ((lcExt <> '.CSV') .OR. !DIRECTORY(lcDirect)))
  lcRpOutNam = GETFILE('CSV', '','Save')
ENDIF

IF !EMPTY(lcRpOutNam)
  lcRpOutNam=  FORCEEXT(lcRpOutNam,'CSV')
ENDIF

IF !EMPTY(lcRpOutNam)
  SAVE TO oariaApplication.DataDir+'SOSSCTS'+'.MEM'  ALL LIKE lcRp* 
ENDIF

ENDFUNC


*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/11/2011
*! Purpose   : When Function of the Option grid
*!*************************************************************
FUNCTION lfwRepWhen
IF EMPTY(lcRpOutNam)
  IF FILE(oariaApplication.DataDir+'SOSSCTS'+'.MEM')
    RESTORE FROM oariaApplication.DataDir+'SOSSCTS'+'.MEM' ADDITIVE
  ENDIF
ENDIF

*!*************************************************************

FUNCTION lfOrdExp
IF EMPTY(lcRpOutNam)
  =gfModalGen('TRM00000B00000','ALERT','','','Please enter the outgoing Shipment file name.')
  RETURN .F.
ENDIF
IF EMPTY(lcRpAcct)
  =gfModalGen('TRM00000B00000','ALERT','','','Please enter the Account name.')
  RETURN .F.
ENDIF

ENDFUNC


*!*************************************************************
*! Name      : lfvAccount
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/08/2011
*! Purpose   : Validate Account
*!*************************************************************
FUNCTION lfvAccount

PRIVATE lcObjVal
PRIVATE lnAlsNo,lcCustOrd,lcObjName
lnAlsNo = SELECT(0)
IF !USED('CUSTOMER')
  =gfOpenTable('Customer','Customer')
ENDIF
SELECT CUSTOMER
lcCustOrd = ORDER()
gfSetOrder('CUSTOMER')
*IF The user want to Browse or if the Account he entered is not in the file
IF '?' $ lcRpAcct .OR. (!EMPTY(lcRpAcct) .AND. !gfSEEK('M' +lcRpAcct, 'CUSTOMER'))
  llObjRet = CusBrowM(@lcRpAcct, '' , 'M')
  lcRpAcct = IIF(llObjRet , lcRpAcct, '')
  *WAIT WINDOW lcRpAcct +"D"
ENDIF

SELECT CUSTOMER
gfSetOrder(lcCustOrd)
SELECT(lnAlsNo)

IF !EMPTY(lcRpAcct)
  SAVE TO oariaApplication.DataDir+'SOSSCTS'+'.MEM'  ALL LIKE lcRp* 
ENDIF

ENDFUNC
*!*************************************************************
