  *!*************************************************************
  *! Name      : lfShpSpAdr
  *! Developer : Mariam Mazhar(MMT)
  *! Date      : 09/26/2021
  *! Purpose   : Function to Get the Sold to Address & Ship to Address
  *!             & the Description of the Ship Via , Terms
  *!*************************************************************
  *! Called from : ARPINVA.FRX
  *!*************************************************************
  *! Calls       : gfRltFld() , gfCodDes() , gfGetAdr() , lfAdrShift()
  *!*************************************************************
  *! Passed Parameters : None
  *!*************************************************************
  *! Return      : ''
  *!*************************************************************
  *C202431 - Custom Invoice form for STC10
FUNCTION lfShpSpAdr

IF !USED('CUSTOMER_ST')
  =gfOpenTable('CUSTOMER','CUSTOMER','SH','CUSTOMER_ST')
ENDIF
=gfSeek('M'+INVHDR.ACCOUNT,'CUSTOMER_ST','CUSTOMER')

  PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
  lnInvHdRec = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))
  lnInvLnRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
  lnPakLnRec = IIF(EOF('SPCK_LIN') , 0 , RECNO('SPCK_LIN'))

  IF USED(lcTmpDbt)
    lnTmpDbt = IIF(EOF(lcTmpDbt) , 0 , RECNO(lcTmpDbt))
    lnARINSTMD = IIF(EOF('ARINSTMD') , 0 , RECNO('ARINSTMD'))
  ELSE
    lnTmpDbt   = 0
    lnARINSTMD = 0
  ENDIF
  lnLineRec = IIF(EOF('INVLINE') , 0 , RECNO('INVLINE'))
  lnHrRc    = IIF(EOF('INVHDR') , 0 , RECNO('INVHDR'))

  COUNT TO lnLines WHILE INVLINE.Invoice = INVHDR.Invoice
  IF lnInvLnRec > 0
    GO (lnLineRec) IN INVLINE
  ENDIF
  IF lnHrRc > 0
    GO (lnHrRc) IN INVHDR
  ENDIF

  *B608441,1 WAM 02/18/2008 Reset factor name and address for each invoice printed.
  *! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [Start]
  *DECLARE laFactor[5,1]
  DECLARE laFactor[6,1]
  *! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [End]
  STORE '' TO laFactor,lcFacName
  *B608441,1 WAM 02/18/2008 (End)

  *B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [BEGIN]
  STORE '' TO lcFacTel
  *B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [END]


  *-- Fill laFactor with factor address
  IF !EMPTY(INVHDR.CFACCODE)
    =SEEK(INVHDR.CFACCODE,'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    *B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [BEGIN]
    lcFacTel    = ALLTRIM(SYCFACT.cphoneno)
    *B609872,1 HIA ADD FACTOR TEL TO INVOICE FORM [END]
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    *! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [Start]
    laFactor[6] = TRANSFORM(lcFacTel, '@R ' + lcPhonPict)
    *! B609872,2 MMT 04/02/2012 ADD FACTOR TEL TO INVOICE FORM [End]
    =lfAdrShift('laFactor')
  ENDIF

  llEndGroup = .F.
  =gfRltFld(CUSTOMER_ST.cDivision , @laDivLName , 'CDIVISION')
  lcShipVia = gfCodDes(INVHDR.ShipVia , 'SHIPVIA')
  lcTerms = gfCodDes(INVHDR.cTermCode , 'CTERMCODE')

  lcSolTName = CUSTOMER.BTName

  laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
  laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
  laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
  laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
  laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

  =lfAdrShift('laSoldTo')

  *IF ORDHDR.Alt_ShpTo is .T.
  SELECT INVHDR
  IF BETWEEN(RECNO(), 1, RECCOUNT())
    GOTO RECNO()
  ENDIF
  SELECT CUSTOMER

  IF Ordhdr.Alt_ShpTo
    lcShpTName  = Ordhdr.STName
    laShipTo[1] = Ordhdr.cAddress1
    laShipTo[2] = Ordhdr.cAddress2
    laShipTo[3] = Ordhdr.cAddress3
    laShipTo[4] = Ordhdr.cAddress4
    laShipTo[5] = Ordhdr.cAddress5
  ELSE    && Else

    lnCUSRec = 0
    *N000592,1 HBG 02/27/2007 Print Store Address or DC Address depnding on the Flag of Dircet To Store in ORDHDR [Begin]
    *IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr)
    IF !EMPTY(CUSTOMER.STORE) AND !EMPTY(CUSTOMER.dist_ctr) AND !Ordhdr.lStrDirct
      *N000592,1 HBG [End]
      lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
      =SEEK('S'+CUSTOMER.Account+CUSTOMER.dist_ctr)
      lcDCCode    = CUSTOMER.STORE
    ELSE
      lcDCCode = ''
    ENDIF

    lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
      IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STName , CUSTOMER.DBA))

    laShipTo[1] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
    laShipTo[2] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
    laShipTo[3] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
    laShipTo[4] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
    laShipTo[5] = IIF(INVHDR.STORE = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

    IF lnCUSRec <> 0
      GOTO lnCUSRec IN CUSTOMER
    ENDIF
  ENDIF    && End of IF

  =lfAdrShift('laShipTo')

  SELECT INVHDR

  IF lnTmpDbt <> 0
    GO lnTmpDbt IN (lcTmpDbt)
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
  RETURN ''