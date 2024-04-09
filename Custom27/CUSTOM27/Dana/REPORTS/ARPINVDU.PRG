*:***************************************************************************
*: Program file  : ARPINVDU.PRG
*: Program desc. : CUSTOMIZED INVOICE FOR DANA.
*! Date          : 05/22/2000
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C101789
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARPINVDU
*:***************************************************************************
*:Modifications :
*B803708,1 BWA 10/02/2000 Fix the bug of not printing the notepad.[Fix in the frx]
*:***************************************************************************

DIME laSku1[8]
STORE SPACE(0) TO laSku1
STORE SPACE(0) TO lcStrToPr1

*!*************************************************************
*! Name        : lflnc
*! Developer   : Bassem Rafaat (BWA)
*! Date        : 05/22/2000
*! Purpose     : To get the SKU.
*!*************************************************************
*! Called from : ARPINVDU.FRX
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lflnc()
*!*************************************************************

FUNCTION lflnc
PARAMETERS lcDumdy

STORE SPACE(0) TO laSku1
PRIVATE lnPrevAl
STORE SPACE(0) TO lcStrToPr1

IF ! SEEK('S'+InvLine.Account+InvLine.Style,'Spck_Lin')
  llPrtSku = .F.
  RETURN .F.
ENDIF
lnPrevAl = SELECT (0)

SELECT Spck_Lin
IF !EMPTY(PACK_ID)
  lnI = 1
  lcSkuTmpl=IIF(!EMPTY(Customer.SkuTmpl),Customer.SkuTmpl,'DEF')
  
  IF SEEK('S'+lcSkuTmpl,'SkuTmpl')
    lnDime1 = SkuTmpl.Len1+SkuTmpl.Len2+SkuTmpl.Len3
    lnDime2 = SkuTmpl.Len4
  ELSE
    lnDime1 = 8  &&Default
    lnDime2 = 8  &&Default
  ENDIF
  
*  lcStrToPr1 = 'SKU # ' + SUBSTR(Pack_Id,1,lnDime1) + ' '  
  lcStrToPr1 = SUBSTR(Pack_Id,1,lnDime1) + ' '  
  
   SCAN WHILE Type+Account+Style = 'S'+InvLine.Account+InvLine.Style .AND. lnI < 9
    FOR lnX=1 TO 8
      Z=STR(lnX,1)
      IF QTY&Z > 0
        laSku1[lnX]=Z + " " + lcStrToPr1 + SUBSTR(Pack_Id,lnDime1+1,lnDime2)
        EXIT
      ENDIF
    ENDFOR
    lnI = lnI + 1
  ENDSCAN
  lnI = 1
ENDIF

SELECT (lnPrevAl)
IF EMPTY(lcStrToPr1)
  STORE SPACE(0) TO laSku1
  llPrtSku = .F.
ELSE
  llPrtSku = .T.
ENDIF


RETURN ''