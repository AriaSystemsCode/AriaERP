*:***************************************************************************
*: Program file  : ALPKTKSE.PRG
*: Program desc. : CUSTOMIZED SO ALLOCATION PICKING TICKET Form FOR SEE06
*: Date          : 03/14/2005
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C126052
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKSE
*:***************************************************************************
*: Modifications :
***************************************************************************

*--Section remove the country code from the company address.

IF !EMPTY(gcContCode)
  FOR LnCont = 1 TO ALEN(laCompAdd,1)
    IF gcContCode $ laCompAdd[LnCont]
      laCompAdd[LnCont,1] = SPACE(0)
	ENDIF
  ENDFOR
ENDIF

=lfAdrShift('laCompAdd')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/14/2005
*! Purpose   : Function to delete the phone number and country code.
*!*************************************************************
*! Called from : ALPKTKSE.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSolSpAdr()
*!*************************************************************
FUNCTION lfSolSpAdr
PARAMETER lcReturn
PRIVATE lcDistCntr , lcAlias , lcKey

lcAlias = SELECT(0)
lcKey = EVAL(KEY())
llEndGroup = .F.
=gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')

SELECT CUSTOMER
SEEK IIF(EMPTY(PIKTKT.STORE),'M','S')+ PIKTKT.Account + PIKTKT.STORE 
lcShipVia = gfCodDes(IIF(CUSTOMER.nBrkWeight <> 0 .AND.;
                         &lcTmpOrdH..nWeight > CUSTOMER.nBrkWeight ,;
                         CUSTOMER.cAltShpvia ,IIF(ORDHDR.ShipVia ='*',CUSTOMER.ShipVia,ORDHDR.ShipVia)), 'SHIPVIA')

lcSeason = gfCodDes(ORDHDR.Season , 'SEASON')
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST')
lcTerms = gfCodDes(ORDHDR.cTermCode , 'CTERMCODE')

SELECT CUSTOMER
SEEK IIF(EMPTY(PIKTKT.Store) , 'M' , 'S') + PIKTKT.Account + PIKTKT.Store

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2'))+ ' Phone# '+ TRANSFORM(Customer.Phone1 , lcPhonPict)

IF !EMPTY(gcContCode)
  FOR LnContSld = 1 TO ALEN(laSoldTo,1)
    IF gcContCode $ laSoldTo[LnContSld]
      laSoldTo[LnContSld,1] = SPACE(0)
	ENDIF
	IF 'Phone#' $ laSoldTo[LnContSld]
      laSoldTo[LnContSld,1] = SPACE(0)
	ENDIF
  ENDFOR
ENDIF

=lfAdrShift('laSoldTo')

IF ORDHDR.Alt_ShpTo
  
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
  
ELSE

  SELECT CUSTOMER
  lcDistCntr = CUSTOMER.Dist_Ctr
  
  *--If there is a distribution center
  IF !EMPTY(lcDistCntr)
    SEEK 'S' + PIKTKT.Account + lcDistCntr
  ENDIF
  
  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = ALLTRIM(gfGetAdr('CUSTOMER' , '' , '' , '' , 5)) + ' Phone#' + TRANSFORM(Customer.Phone1 , lcPhonPict)
  
ENDIF

IF !EMPTY(gcContCode)
  FOR LnContSip = 1 TO ALEN(laShipTo,1)
    IF gcContCode $ laShipTo[LnContSip]
      laShipTo[LnContSip,1] = SPACE(0)
	ENDIF
	IF 'Phone#' $ laShipTo[LnContSip]
      laShipTo[LnContSip,1] = SPACE(0)
	ENDIF
  ENDFOR
ENDIF

=lfAdrShift('laShipTo')

SELECT (lcTmpOrdL)
SELECT(lcAlias)
=SEEK(lcKey)
RETURN ""

*--End of lfSolSpAdr.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST
*! Date      : 03/14/2005
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ALPKTKSE.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAdrShift()
*!*************************************************************

FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF
ENDFOR

*--End of lfAdrShift.