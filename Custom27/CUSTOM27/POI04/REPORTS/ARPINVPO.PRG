*:*************************************************************
*: Program file  : Arpinvpo.Prg
*: Program desc. : Invoice Form (For Point Image)
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Account Receivable - [AR]
*: Developer     : Abdou Elgendy -  [ABD]
*: Date          : 12/16/2003
*: Tracking Job Number : C#120057,1
*:*************************************************************
*: Calls         : FUNCTIONS  : lfEvalSegs , lfCustInfo
*:               -----------------------------------------------
*:               : PROCEDURE  : ..
*:*************************************************************
*: Passed Parameters  : None.
*:*************************************************************
*: C#120057,1
*:*************************************************************
*: Modifications  :
*:*************************************************************
*:
*-- Start Declaration Variable.
STORE 0 TO lnMajorLen, lnColorLen,lnNonMajst


*-- Evaluate NonMajor Type and variables.
= lfEvalSegs ()
*-- End Declaration Variable.

RETURN

*-- End OF Code.
*:*************************************************************
*: Name        : lfEvalSegs
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/16/2003
*: Purpose     : Evaluate NonMajor Type and variables.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfEvalSegs()
*:*************************************************************
*:
FUNCTION lfEvalSegs

STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF

*-- End OF lfEvalSegs.
*:*************************************************************
*: Name        : lfCustInfo
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/02/2003
*: Purpose   : To get the customer addresses.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   :  =lfCustInfo()
*:*************************************************************
*:
FUNCTION lfCustInfo
PARAMETER lcReturn

PRIVATE lcAlias,lnCount

lcReturn = .T.
lcAlias = SELECT(0)
lcSolTName = CUSTOMER.BTName
llEndGroup = .F.
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

=lfAdrShift('laSoldTo')

IF ORDHDR.Alt_ShpTo
  lcShpTName  = ORDHDR.STName
  laShipTo[1] = ORDHDR.cAddress1
  laShipTo[2] = ORDHDR.cAddress2
  laShipTo[3] = ORDHDR.cAddress3
  laShipTo[4] = ORDHDR.cAddress4
  laShipTo[5] = ORDHDR.cAddress5
ELSE    && Else
  lcShpTName  = IIF(INVHDR.STORE = "********" , "At Store Level " ,;
                IIF( EMPTY(CUSTOMER.DBA) , CUSTOMER.STNAME , CUSTOMER.DBA))
  laShipTo[1] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] =  gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
ENDIF

=lfAdrShift('laShipTo')

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


SELECT (lcAlias)

RETURN ''
*- End OF lfCustInfo.
*:*************************************************************
*: Name        : lfPrntScl
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 12/02/2003
*: Purpose     : Function to print the scale or not if repeted.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   :  lfPrntScl()
*:*************************************************************
*:
FUNCTION lfPrntScl
PARAMETER lcReturn
lcReturn = .T.

IF lcOldScale = Invline.Scale
  llPrintScl = .F.
ELSE
  llPrintScl = .T.
  lcOldScale = Invline.Scale
ENDIF


RETURN ''

*-- End OF lfPrntScl
*:***********************************************************************