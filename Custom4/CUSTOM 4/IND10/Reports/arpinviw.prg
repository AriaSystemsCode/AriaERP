*:***************************************************************************
*: Program file  : ARPINVIW.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR INDUSTRIE WEAR, INC.
*: Date          : 11/15/2006
*: System        : Aria4xp.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : AYMAN MAHMOUD AHMED (AYM)
*: Track no      : C200702 - T20061009.0004
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVIW
*:***************************************************************************
PRIVATE lcAlias,lcOrder
STORE 0 TO lnDim,lnCount
lnDim=ALEN(laRpTarget,1)
DIMENSION laCopy[3,2]
DIMENSION laCopyN[3,1]
STORE '' TO laCopy
STORE 0 TO laCopyN
lcAlias=ALIAS()
SELECT CODES
lcOrder=ORDER()
SET ORDER TO CODES
FOR lnCount=1 TO ALEN(laRpTarget,1)
  SEEK 'N'+laRpTarget(1,lnCount)
   laCopy(lnCount,1)=CODES.ccode_no
   laCopy(lnCount,2)=CODES.cdiscrep 
   SKIP
   laCopyN(lnCount,1)=VAL(CODES.crltd_vlu)
ENDFOR
SET ORDER TO (lcOrder)
SELECT (lcAlias)
*-- End of lfGetCodes.



*-- End of lfFillAll.
*!*************************************************************
*! Name      : lfCrtMover
*! Developer : Mostafa Rawash (MMR)
*! Date      : 08/21/2005
*! Purpose   : Function to create mover for allowance ,
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfCrtMover()
*!*************************************************************
FUNCTION lfCrtMover
PARAMETER lcParam
= lfogmover(@laRpSource,@laRpTarget,'Allowance ',.T.,'')
lnCount=0
FOR lnInd = 1 TO ALEN(laRpTarget)
    lnCount=lnCount+1
    IF lnCount>3
      =gfModalGen('INM00000B00000','','','','You have selected more than 3 Allowance Codes,your selections will be removed.')    
      DIMENSION laRpSource[1,1],laRpTarget[1,1]
      STORE '' TO laRpSource,laRpTarget
      SELECT CODES
      SELECT DISTINCT cCode_NO FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CALLOWANCE ' AND crltField = 'Y';
                                               INTO ARRAY laRpSource
      EXIT
    ENDIF 
ENDFOR
CLEARREAD()
lcParam = .T.
RETURN lcParam

*-- End of lfCrtMover.
*!*************************************************************
*! Name      : lfFillAll
*! Developer : Mostafa Rawash (MMR)
*! Date      : 08/21/2005
*! Purpose   : Function to fill Allowance
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillAll()
*!*************************************************************
FUNCTION lfFillAll
PARAMETER lcParam
DIMENSION laRpSource[1,1],laRpTarget[1,1]

STORE '' TO laRpSource,laRpTarget

*--The ALLOWANCE.
SELECT CODES
SELECT DISTINCT cCode_NO FROM CODES WHERE cDefCode+cFld_Name = 'N'+'CALLOWANCE ' AND crltField = 'Y';
                                        INTO ARRAY laRpSource

