*************************************************************************
*: Program file  : ALPKTKSE.PRG
*: Program desc. : CUSTOMIZED SO ALLOCATION PICKING TICKET Form FOR SEE06
*: Date          : 03/16/2005
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
    IF ALLTRIM(gcContCode) $ laCompAdd[LnCont]
      laCompAdd[LnCont,1] = SPACE(0)
	ENDIF
  ENDFOR
ENDIF

=lfAdrShift('laCompAdd')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 03/16/2005
*! Purpose   : Function to delete the phone number and country code.
*!*************************************************************
*! Called from : ARPINVSE.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfSolSpAdr()
*!*************************************************************
FUNCTION lfSolSpAdr
PARAMETER lcReturn
PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec , lcAlias , lcKey
*--mhm2005
*lcAlias = SELECT(0)
*lcKey = EVAL(KEY())
*--mhm2005

lnInvHdRec = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
lnInvLnRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnPakLnRec = IIF(EOF('SPCK_LIN') , 0 , RECNO('SPCK_LIN'))

*-- (LCTMPDBT) , ARINSTMD
IF USED(lcTmpDbt)
  lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
  lnARINSTMD = IIF(EOF('ARINSTMD') , 0 , RECNO('ARINSTMD'))
ELSE
  lnTmpDbt   = 0
  lnARINSTMD = 0
ENDIF

lnLineRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
lnHrRc    = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
COUNT TO lnLines WHILE INVLINE.INVOICE = INVHDR.INVOICE
IF lnInvLnRec > 0
  GOTO (lnLineRec) IN INVLINE
ENDIF
IF lnHrRc > 0
  GOTO (lnHrRc) IN INVHDR
ENDIF

*-- HDM B802298,1 [Start] Fill laFactor with factor address
IF !EMPTY(INVHDR.CFACCODE)
  =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
=gfRltFld(INVHDR.cDivision , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

IF !EMPTY(gcContCode)
  FOR LnContSld = 1 TO ALEN(laSoldTo,1)
    IF (ALLTRIM(gcContCode) $ laSoldTo[LnContSld]) OR (ALLTRIM(CUSTOMER.CCONT_CODE) $ laSoldTo[LnContSld])
      laSoldTo[LnContSld,1] = SPACE(0)
	ENDIF
	IF 'Phone#' $ laSoldTo[LnContSld]
      laSoldTo[LnContSld,1] = SPACE(0)
	ENDIF
  ENDFOR
ENDIF

=lfAdrShift('laSoldTo')

*--IF ORDHDR.Alt_ShpTo is .T.
SELECT INVHDR
IF BETWEEN(RECNO(), 1, RECCOUNT())
  GOTO RECNO()
ENDIF
SELECT CUSTOMER

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    *-- DC Code
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF
  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  
  laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0 
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF

IF !EMPTY(gcContCode)
  FOR LnContSip = 1 TO ALEN(laShipTo,1)
    IF (ALLTRIM(gcContCode) $ laShipTo[LnContSip]) OR (ALLTRIM(CUSTOMER.CCONT_CODE) $ laShipTo[LnContSip])
      laShipTo[LnContSip,1] = SPACE(0)
	ENDIF
	IF 'Phone#' $ laShipTo[LnContSip]
      laShipTo[LnContSip,1] = SPACE(0)
	ENDIF
  ENDFOR
ENDIF

=lfAdrShift('laShipTo')

SELECT INVHDR

*-- (LCTMPDBT) , ARINSTMD
IF lnTmpDbt <> 0
  GOTO lnTmpDbt IN (lcTmpDbt)
ENDIF
IF lnARINSTMD <> 0
  GO lnARINSTMD IN ARINSTMD
ENDIF

*-- Restore the old record pointer in INVLINE
IF lnInvLnRec = 0
  GO BOTTOM IN INVLINE
  IF !EOF('INVLINE')
    SKIP IN INVLINE
  ENDIF
ELSE
  GO lnInvLnRec IN INVLINE
ENDIF

*-- Restore the old record pointer in SPCK_LIN
IF lnPakLnRec = 0
  GO BOTTOM IN SPCK_LIN
  IF !EOF('SPCK_LIN')
    SKIP IN SPCK_LIN
  ENDIF
ELSE
  GO lnPakLnRec IN SPCK_LIN
ENDIF

*--mhm2005
*SELECT(lcAlias)
*=SEEK(lcKey)
*--mhm2005
RETURN ""

*--End of lfSolSpAdr.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : BASSEM RAFAAT ERNEST
*! Date      : 03/16/2005
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVSE.PRG
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