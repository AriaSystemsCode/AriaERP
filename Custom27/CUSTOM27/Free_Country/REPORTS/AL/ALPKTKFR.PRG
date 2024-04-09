*:***************************************************************************
*: Program file  : ALPKTKFR.PRG
*: Program desc. : CUSTOMIZED P/T FOR FREE COUNTRY
*: Date          : 06/02/2005
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mostafa Mahmoud Rawash (MMR)
*: Tracking Job Number: C126673
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKFR
*:***************************************************************************
*:B039684 MMR 09/07/2005  Printing SKU Per Style only.
*:***************************************************************************
DIME laSku1[8]
STORE SPACE(0) TO laSku1 , lcStrToPr1,lcDunsNo

lcDunsNo  = gfGetMemVar('XDUNS') 
*!*************************************************************
*! Name        : lflnc
*! Developer   : Mostafa Mahmoud Rawash (MMR)
*! Date        : 09/07/2005
*! Purpose     : To get the SKU.
*!*************************************************************
*! Called from : ALPKTKFR.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lflnc()
*!*************************************************************
*B039684 MMR 09/07/2005  Printing SKU Per Style only.
FUNCTION lflnc
PARAMETER lcDumdy
PRIVATE lcAlias
STORE SPACE(0) TO lcStrToPr1,laSku1 
lcAlias = SELECT (0)
Select SPCK_LIN
SET ORDER TO Spcklins
IF !SEEK('S'+ORDLINE.Account+ORDLINE.Style,'SPCK_LIN')
  llPrtSku = .F.
  IF !EMPTY(lcAlias)
    SELECT (lcAlias)
  ENDIF  
  RETURN .F.
ENDIF
SELECT Spck_HDR
SET ORDER to Sku_style
=SEEK('S'+ORDLINE.Account+ORDLINE.Style,'Spck_HDR')
IF !EMPTY(SKU)
  lcStrToPr1 = SKU  
ENDIF
SELECT Spck_Lin
IF !EMPTY(PACK_ID)
  lnI = 1
  SCAN WHILE Type+Account+Style = 'S'+ORDLINE.Account+ORDLINE.Style .AND. lnI < 9
   FOR lnX = 1 TO 8
     Z=STR(lnX,1)
     IF QTY&Z > 0
       IF ORDLINE.PIK&Z<>0
         laSku1[lnX]= "Size"+ Z + " " + Pack_Id
         EXIT
       ELSE
         laSku1[lnX]=''
         EXIT
       ENDIF  
     ENDIF
   ENDFOR
   lnI = lnI + 1
  ENDSCAN
  lnI = 1
ENDIF
  IF !EMPTY(lcAlias)
    SELECT (lcAlias)
  ENDIF  
IF EMPTY(lcStrToPr1)
  STORE SPACE(0) TO laSku1
  llPrtSku = .F.
ELSE
  llPrtSku = .T.
ENDIF

RETURN ''
*-- End of lflnc.