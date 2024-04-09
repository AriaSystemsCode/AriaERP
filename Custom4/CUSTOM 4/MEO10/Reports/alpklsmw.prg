*!**************************************************************************
*! Name      : alpklsmw.prg
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/03/2008
*! Purpose   : Custom Packing List Form For MEO10
*!  C201081  ==> for Aria4  attachments
*!  C201080  ==> for Aria27 attachments
*! Ticket id T20080916.0012
*!**************************************************************************
*!**************************************************************************
*! Name      : lfHdVar
*! Developer : Mariam Mazhar - (MMT)
*! Date      : 12/03/2008
*! Purpose   : 1 - Hold Address In Shipto Array
*!           : 2 - Get the division long name.
*!**************************************************************************
*! Called from : Report Group Header band.
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : ....
*!**************************************************************************
FUNCTION lfHdVar

IF !USED('Customer_MW')
  =gfOpenTable('Customer','Customer','SH','Customer_MW')
ENDIF 
=gfSeek('M'+&lcCustomer..Account,'Customer_MW')

= gfRltFld(&lcOrdHdr..CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

DIMENSION laShipTo[5,1]
laShipTo = ''

DIMENSION laSoldTo[6,1]

laSoldTo = ''

lcSolTName = &lcCustomer..BTName
laSoldTo[1] = gfGetAdr(lcCustomer , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr(lcCustomer , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr(lcCustomer , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr(lcCustomer , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr(lcCustomer , '' , '' , '' , 5 , '2')
laSoldTo[6] = gfGetAdr(lcCustomer , '' , '' , '' , 6 , '2')
=lfAdrShift('laSoldTo')

PRIVATE lcDistCntr , lcAlasCust
lcAlasCust = SELECT(0)
SELECT(lcCustomer)

IF &lcOrdHdr..Alt_ShpTo
  DIMENSION laShipTo[6,1]
  laShipTo = ''
  laShipTo[1] = &lcOrdHdr..STName
  laShipTo[2] = &lcOrdHdr..cAddress1
  laShipTo[3] = &lcOrdHdr..cAddress2
  laShipTo[4] = &lcOrdHdr..cAddress3
  laShipTo[5] = &lcOrdHdr..cAddress4
  laShipTo[6] = &lcOrdHdr..cAddress5
  =lfAdrShift('laShipTo')
ELSE
  lnCUSRec = 0
  IF !EMPTY(&lcCustomer..Store) AND !EMPTY(&lcCustomer..Dist_ctr) AND !EVALUATE(lcOrdHdr+'.lStrDirct')
    lnCUSRec = IIF(!EOF(lcCustomer),RECNO(lcCustomer),0)
    =SEEK('S'+&lcCustomer..Account+&lcCustomer..Dist_ctr)
  ENDIF

  =gfGetAdr(lcCustomer , '' , '' , '' , @laShipTo)
  =lfAdrShift('laShipTo')

  DIMENSION laShipTo[6,1]
  =AINS(laShipTo,1)

  laShipTo[1,1] = &lcCustomer..StName
  =lfAdrShift('laShipTo')

  IF BETWEEN(lnCusRec , 1 , RECCOUNT(lcCUSTOMER))
    GOTO lnCusRec IN &lcCUSTOMER
  ENDIF
ENDIF
SELECT(lcAlasCust)


PRIVATE lnSlct
lnSlct = SELECT(0)
IF llPrntComp AND lcRpAddres = 'W'   && If no need to print the warehous data
  lcFAXNum = ''
  DIMENSION laCompAdd[6,1]
  laCompAdd = ''
  STORE '' TO lcCompPhon
  SELECT(lcWareHous)
  SEEK &lcPackTmp..CWARECODE
  DIMENSION laCompAdd[5,1]      && redimension the array to be used by the glabal function gfGetAdr().
  =gfGetAdr(lcWareHous , '' , '' , '' , @laCompAdd)

  && Get the warehous name ane its phone in the variables   lcCompName , lcCompPhon
  *lcCompName = &lcWareHous..CDESC  && WAREHOUS Name.
  lcCompPhon = &lcWareHous..CPHONE && WAREHOUS Phone.

  DIMENSION laCompAdd[6,1]
  laCompAdd[6,1] = 'Phone# : '+TRANSFORM(lcCompPhon , '@R '+lcPhonPict)
  DIMENSION laCompAdd[7,1]
  laCompAdd[7,1] = 'FAX      : '+TRANSFORM(&lcWareHous..cfax, '@R '+lcPhonPict)
  =lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
ENDIF

IF llPrntComp AND !(lcRpAddres = 'W')
  IF USED(lcCompInfo)
    DIMENSION laCompAdd[7,1]
    laCompAdd = ''
    lcCompName      = &lcCompInfo..cCom_Name
    lcCompPhon      = &lcCompInfo..cCom_Phon              && Variable to hold the Company Phone
    lcPhonPict      = gfPhoneTem()          && Variable to hold the Company Phone Format
    laCompAdd[1]    = gfGetAdr(lcCompInfo , '' , '' , '' , 1)
    laCompAdd[2]    = gfGetAdr(lcCompInfo , '' , '' , '' , 2)
    laCompAdd[3]    = gfGetAdr(lcCompInfo , '' , '' , '' , 3)
    laCompAdd[4]    = gfGetAdr(lcCompInfo , '' , '' , '' , 4)
    laCompAdd[5]    = gfGetAdr(lcCompInfo, '' , '' , '' , 5)
    laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon ,'@R '+lcPhonPict)
    laCompAdd[7,1] = 'FAX      : '+TRANSFORM(&lcCompInfo..cCom_Fax , '@R '+lcPhonPict)
    =lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF  
ENDIF 

SELECT (lnSlct)

STORE "" TO lcScale , lcPackNo
RETURN ""
*-- END OF lfHeadVar