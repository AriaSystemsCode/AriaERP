*:***************************************************************************
*: Program file  : RmrAucSH.PRG
*: Program desc. : Return Authorization,custom
*: System        : Aria 4 XP
*: Module        : Return Merchandise (RM)
*: Developer     : Hassan Ibrahin Ali
*: Issue #       : C201484
*:***************************************************************************
*: Calls :
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO RmrAut
*:***************************************************************************
*: Modifications:
*:***********************************************************************************************
PRIVATE lnPrevAl


*!*************************************************************
*! Name        : lfHeader
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band]
*!*************************************************************
*! Calls       : FUNCTION => gfRltFld()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeader()
*!*************************************************************

FUNCTION lfHdAlt
  LPARAMETERS ldum
  PRIVATE lnPrevAl
  = gfRltFld(RetAuth.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
  lcReason = gfCodDes(RetAuth.reason,'REASON')

  * fill Sold To address array
  lnPrevAl = SELECT(0)
  SELECT(lnPrevAl)
  *B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [Begin]
  IF !EMPTY(RetAuth.STORE)
    =SEEK('S'+RetAuth.ACCOUNT+RetAuth.STORE,'CUSTOMER')
  ELSE
    =SEEK('M'+RetAuth.ACCOUNT,'CUSTOMER')
  ENDIF

  *B608098,1 HIA stop the relation to the customer file based on the main account [T20070507.0003] [End]


  *E303128,1 HIA 05/13/2012 Add new option to Sold To Address from Alternate order Address [T20070507.0003] [Begin]
  &&  SET ORDER TO ORDHDR   && CORDTYPE+ORDER
  
  IF !USED('ORDHDR')
    =gfOpenTable('ORDHDR','ORDHDR','SH')
  ENDIF
  
  IF lcRpAdTy = "O" AND !EMPTY(ALLTRIM(RetAuth.ORDER)) AND SEEK("O"+RetAuth.ORDER,"ORDHDR","ORDHDR") AND ORDHDR.alt_shpto
    && As per MMT, we will fill the array manually, the call lfAdrShift function, below the IF statment.
    laSoldTo[1] = ALLTRIM(ORDHDR.STName)
    laSoldTo[2] = ALLTRIM(ORDHDR.caddress1 )
    laSoldTo[3] = ALLTRIM(ORDHDR.caddress2 )
    laSoldTo[4] = ALLTRIM(ORDHDR.caddress3 )
    laSoldTo[5] = ALLTRIM(ORDHDR.caddress4 )
    laSoldTo[6] = ALLTRIM(ORDHDR.caddress5 )

    laSoldTo[7]  = IIF(!EMPTY(ALLTRIM(ORDHDR.Phone)),TRANSFORM(ORDHDR.Phone, '@R ' + lcPhonPict), '')

  ELSE

    *E303128,1 HIA 05/13/2012 Add new option to Sold To Address from Alternate order Address [T20070507.0003] [End]
    laSoldTo[1] = CUSTOMER.STName
    *: B609017,1 HES 09/10/2009 Fix bug of wrong sold to address on RA at DCC [Start]
    *!*	laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , IIF(Customer.Type='S','2',''))
    *!*	laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , IIF(Customer.Type='S','2',''))
    *!*	laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , IIF(Customer.Type='S','2',''))
    *!*	laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , IIF(Customer.Type='S','2',''))
    *!*	laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , IIF(Customer.Type='S','2',''))
    laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '')
    laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '')
    laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '')
    laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '')
    laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '')
    *: B609017,1 HES 09/10/2009 Fix bug of wrong sold to address on RA at DCC [End]
    laSoldTo[7]  = IIF(!EMPTY(ALLTRIM(CUSTOMER.Phone1)),TRANSFORM(CUSTOMER.Phone1, '@R ' + lcPhonPict),'')
    *E303128,1 HIA 05/13/2012 Add new option to Sold To Address from Alternate order Address [T20070507.0003] [Begin]
  ENDIF
  *E303128,1 HIA 05/13/2012 Add new option to Sold To Address from Alternate order Address [T20070507.0003] [End]

  laWareAdd[1]= gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
  laWareAdd[2]= gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
  laWareAdd[3]= gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
  laWareAdd[4]= gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
  laWareAdd[5]= gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
  laWareAdd[6]= IIF(!EMPTY(ALLTRIM(WareHous.cphone)),TRANSFORM(WareHous.cphone , '@R ' + lcPhonPict),'')
  = lfAdrShift('laWareAdd')
  = lfAdrShift('laSoldTo')    && Shift Sold To address if there is empty line.

  IF llTocca
    lcVendCD = CUSTOMER.CCUSVEND
    lcDept = IIF(!EMPTY(RetAuth.INVOICE) AND gfSEEK(RetAuth.INVOICE,'INVHDR'),INVHDR.DEPT,'')

  ENDIF
  RETURN .T.

  *!*************************************************************
