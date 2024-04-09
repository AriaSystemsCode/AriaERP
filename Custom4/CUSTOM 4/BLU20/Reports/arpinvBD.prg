*:***************************************************************************
*: Program file  : ARPINVIW.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR BLUE DUCK
*: Date          : 08/26/2007
*: System        : Aria4xp.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Mariam Mazhar (MMT)
*: Track no      : C200841 - T20070725.0020 
*:***************************************************************************
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006]
*:***************************************************************************
lcRpPrSt = IIF(lcRpPrSt ='N',SPACE(1),lcRpPrSt)

SELECT INVHDR
LOCATE 
LOCATE FOR &lcRpExp
IF !FOUND()
  WAIT WINDOW "No Records Selected"
  llNoRec = .T.
  SET DEVICE TO SCREEN
  RETURN
ENDIF



SELECT Invhdr 
DIMENSION laStructArr[1,18]

lnLength =AFIELDS(laStructArr)
DIMENSION laStructArr[lnLength +1,18]

laStructArr[lnLength +1,1]= 'cPType'
laStructArr[lnLength +1,2]= 'C'
laStructArr[lnLength +1,3]=  1
laStructArr[lnLength +1,4]= 0


FOR lncnt=7 TO 16
    STORE SPACE(0) TO laStructArr[lnLength +1,lnCnt]
ENDFOR  
STORE 0 TO laStructArr[lnLength +1,17],laStructArr[lnLength +1,18]

=gfCrtTmp(lcInvhFile ,@laStructArr,'INVOICE+cPType',lcInvhFile,.T.)

SELECT INVHDR
lcrelation = SET("Relation" )
lcSkip = SET("Skip")
SET RELATION TO
SET SKIP TO 
SCAN FOR  &lcRpExp
  SCATTER MEMO MEMVAR 
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
*!*	  IF !EMPTY(lcPrintPart)
*!*	    IF 'A' $ lcPrintPart
  IF !EMPTY(lcRpPrnt)
    IF 'A' $ lcRpPrnt
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]    
      m.CPTYPE = 'A'
      SELECT (lcInvhFile) 
      APPEND BLANK 
      GATHER MEMO MEMVAR
    ENDIF 
    *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
    *IF 'B' $ lcPrintPart
    IF 'B' $ lcRpPrnt
    *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]
      m.CPTYPE = 'B'
      SELECT (lcInvhFile) 
      APPEND BLANK 
      GATHER MEMO MEMVAR
    ENDIF 
    *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
    *IF 'C' $ lcPrintPart
    IF 'C' $ lcRpPrnt
    *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]
      m.CPTYPE = 'C'
      SELECT (lcInvhFile) 
      APPEND BLANK 
      GATHER MEMO MEMVAR
    ENDIF 
    *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
    *IF 'D' $ lcPrintPart
    IF 'D' $ lcRpPrnt
    *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]
      m.CPTYPE = 'D'
      SELECT (lcInvhFile) 
      APPEND BLANK 
      GATHER MEMO MEMVAR
    ENDIF 

  ELSE
    m.CPTYPE = 'X'
    SELECT (lcInvhFile) 
    APPEND BLANK 
    GATHER MEMO MEMVAR
  ENDIF   
ENDSCAN

lcrelation = STRTRAN(UPPER(lcrelation),'INVHDR.',"")

SELECT (lcInvhFile) 
SET RELATION TO &lcrelation
SET SKIP TO &lcSkip

SET RELATION TO invoice INTO invhdr ADDITIVE
*MMT
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
llarpinv  = .F.
*MMT
RETURN 

*!*************************************************************
*! Name      : lfCrtMover
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/26/2007
*! Purpose   : Function to create mover for allowance ,
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfCrtMover()
*!*************************************************************
FUNCTION lfCrtMover
PARAMETER lcParam


= lfogmover(@laRpSParts,@laRpTParts,'Parts',.T.,'')
IF EMPTY(laRpTParts[1])
  =gfModalGen('INM00000B00000','','','','You have to select at least one copy to be printed.')    
  DIMENSION laRpTParts[3]
  laRpTParts[1] = 'INVOICE'
  laRpTParts[2] = 'PACKING LIST'
  laRpTParts[3] = 'OFFICE COPY'
ENDIF 
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
*lcPrintPart = ' '
lcRpPrnt = ' '
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]
*-- Loop to make Status expression.
*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTParts[1])
  FOR lnI = 1 TO ALEN(laRpTParts,1)
  *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
*!*	    lcPrintPart = lcPrintPart+ IIF(laRpTParts[lnI] = 'INVOICE','A',;
*!*	      IIF(laRpTParts[lnI] = 'PACKING LIST','B',;
*!*	      IIF(laRpTParts[lnI] = 'OFFICE COPY','C',IIF(laRpTParts[lnI] = 'SALES COPY','D',''))))
    lcRpPrnt = lcRpPrnt + IIF(laRpTParts[lnI] = 'INVOICE','A',;
      IIF(laRpTParts[lnI] = 'PACKING LIST','B',;
      IIF(laRpTParts[lnI] = 'OFFICE COPY','C',IIF(laRpTParts[lnI] = 'SALES COPY','D',''))))
  *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]
  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
*lcPrintPart = IIF(EMPTY(lcPrintPart),lcPrintPart ,ALLTRIM(lcPrintPart))
lcRpPrnt = IIF(EMPTY(lcRpPrnt),lcRpPrnt,ALLTRIM(lcRpPrnt))
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]


*!*************************************************************
*! Name      : lfFillAll
*! Developer : Mariam Mazhar (MMT)
*! Date      : 08/26/2007
*! Purpose   : Function to fill Allowance
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************

FUNCTION lfFillAll
PARAMETER lcParam
DIMENSION laRpSParts[1,1],laRpTParts[1,1]

STORE '' TO laRpSParts,laRpTParts

DIMENSION laRpSParts[4]


laRpSParts[1] = 'INVOICE'
laRpSParts[2] = 'PACKING LIST'
laRpSParts[3] = 'OFFICE COPY'
laRpSParts[4] = 'SALES COPY'

DIMENSION laRpTParts[3]
laRpTParts[1] = 'INVOICE'
laRpTParts[2] = 'PACKING LIST'
laRpTParts[3] = 'OFFICE COPY'

*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
*lcPrintPart = ' '
lcRpPrnt = ' '
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]
*-- Loop to make Status expression.
*-- Now we didn`t canceld or deselect
IF !EMPTY(laRpTParts[1])
  FOR lnI = 1 TO ALEN(laRpTParts,1)
  *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
*!*	    lcPrintPart = lcPrintPart+ IIF(laRpTParts[lnI] = 'INVOICE','A',;
*!*	      IIF(laRpTParts[lnI] = 'PACKING LIST','B',;
*!*	      IIF(laRpTParts[lnI] = 'OFFICE COPY','C',IIF(laRpTParts[lnI] = 'SALES COPY','D',''))))
   lcRpPrnt = lcRpPrnt + IIF(laRpTParts[lnI] = 'INVOICE','A',;
      IIF(laRpTParts[lnI] = 'PACKING LIST','B',;
      IIF(laRpTParts[lnI] = 'OFFICE COPY','C',IIF(laRpTParts[lnI] = 'SALES COPY','D',''))))
  *B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]
  ENDFOR  && end Loop to make Status expression.

ENDIF && End of ' IF !EMPTY(laRpTarget[1]) '
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][Start]
*lcPrintPart = IIF(EMPTY(lcPrintPart),lcPrintPart ,ALLTRIM(lcPrintPart))
lcRpPrnt= IIF(EMPTY(lcRpPrnt),lcRpPrnt,ALLTRIM(lcRpPrnt))
*B609563,1 MMT 04\10\2011 Fix bug of wrong invoice labels[T20110328.0006][End]
*!*************************************************************
*! Name      : RefreshStatus
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 26/08/2007
*! Purpose   : Return the selected status in the ReadBox
*!*************************************************************
*! Passed Parameters  : 
*!*************************************************************
*! Returns            : 
*!***************************************************************************
*! Modification:
*!***************************************************************************
FUNCTION RefreshStatus
  LOCAL lcStatusStr, lnTarget
  lcStatusStr = ""
  IF !EMPTY(laRpTParts)
    FOR lnTarget = 1 TO ALEN(laRpTParts,1)
      lcStatusStr = lcStatusStr + ", " + laRpTParts[lnTarget]
    ENDFOR 
    lcStatusStr = SUBSTR(lcStatusStr,3)
  ENDIF   
  RETURN lcStatusStr
ENDFUNC 

*!*************************************************************
*! Name      : lfSSpAdr
*! Developer : MARIAM MAZHAR (MMT) 
*! Date      : 26/08/2007
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
*
FUNCTION lfSSpAdr
PARAMETERS lcnothing

PRIVATE lnInvHdRec , lnInvLnRec , lnPakLnRec ,lnLineRec
lnInvHdRec = IIF(EOF(lcInvhFile) , 0 , RECNO(lcInvhFile))
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
lnHrRc    = IIF(EOF(lcInvhFile) , 0 , RECNO(lcInvhFile))

COUNT TO lnLines WHILE INVLINE.INVOICE = EVALUATE(lcInvhFile+'.INVOICE')
IF lnInvLnRec > 0
  GO (lnLineRec) IN INVLINE
ENDIF
IF lnHrRc > 0
  GO (lnHrRc) IN (lcInvhFile)
ENDIF

*-- Fill laFactor with factor address
IF !EMPTY(EVALUATE(lcInvhFile+'.CFACCODE'))
  =SEEK(EVALUATE(lcInvhFile+'.CFACCODE'),'SYCFACT')
    lcFacName   = SYCFACT.cfaccomp
    laFactor[1] = gfGetAdr('SYCFACT' , '' , '' , '' , 1)
    laFactor[2] = gfGetAdr('SYCFACT' , '' , '' , '' , 2)
    laFactor[3] = gfGetAdr('SYCFACT' , '' , '' , '' , 3)
    laFactor[4] = gfGetAdr('SYCFACT' , '' , '' , '' , 4)
    laFactor[5] = gfGetAdr('SYCFACT' , '' , '' , '' , 5)
    =lfAdrShift('laFactor')
ENDIF

llEndGroup = .F.
=gfRltFld(EVALUATE(lcInvhFile+'.cDivision') , @laDivLName , 'CDIVISION')
lcShipVia = gfCodDes(EVALUATE(lcInvhFile+'.ShipVia') , 'SHIPVIA')
lcTerms = gfCodDes(EVALUATE(lcInvhFile+'.cTermCode') , 'CTERMCODE')

lcSolTName = CUSTOMER.BTName

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

*IF ORDHDR.Alt_ShpTo is .T.
SELECT (lcInvhFile)
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
ELSE    && Else

  lnCUSRec = 0
  IF !EMPTY(CUSTOMER.Store) AND !EMPTY(CUSTOMER.Dist_ctr) AND !ORDHDR.lStrDirct
    lnCUSRec = IIF(!EOF('CUSTOMER'),RECNO('CUSTOMER'),0)
    =SEEK('S'+CUSTOMER.Account+CUSTOMER.Dist_ctr)
    lcDCCode    = CUSTOMER.STORE
  ELSE
    lcDCCode = ''
  ENDIF

  lcShpTName  = IIF(EVALUATE(lcInvhFile+'.STORE') = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))

  laShipTo[1] = IIF(EVALUATE(lcInvhFile+'.STORE')= "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 1))
  laShipTo[2] = IIF(EVALUATE(lcInvhFile+'.STORE')= "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 2))
  laShipTo[3] = IIF(EVALUATE(lcInvhFile+'.STORE')= "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 3))
  laShipTo[4] = IIF(EVALUATE(lcInvhFile+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 4))
  laShipTo[5] = IIF(EVALUATE(lcInvhFile+'.STORE') = "********" , "" , gfGetAdr('CUSTOMER' , '' , '' , '' , 5))

  IF lnCUSRec <> 0
    GOTO lnCUSRec IN CUSTOMER
  ENDIF
ENDIF    && End of IF

=lfAdrShift('laShipTo')

  SELECT (lcInvhFile)

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



