*:**************************************************************************
*: Program file  : FIXINVDT.PRG
*: Program desc. : Fix Program
*: System        : Aria 2.7
*: Developer     : Amin Khodary (AKA)
*: Date          : 09/27/20001
*: Reference     : B-604975
*:**************************************************************************
PRIVATE lcPath , lcCurPath , lcInvNo , lcVendCode

SET STEP ON
SET DEBU ON 
CLOSE DATAB
lcPath = SET("DEFAULT")
SET DEFA TO R:\ARIA27\DBFS\02

USE ApDist   IN 0 ORDER TAG InvVend
USE ApvInvDt IN 0 ORDER TAG OrgvInv DESCENDING    &&Vendor+Invoice+Line#
USE ApVendor IN 0 ORDER TAG VenCode
USE PosHdr   IN 0 ORDER TAG Poshdrv 
USE POFHDR   IN 0 ORDER TAG Pofhdrv 
USE ApInvHdr IN 0 ORDER TAG Vendinv 

SELECT ApVendor

SELECT ApDist
*-- Scan loop around ApDist file [Begin]
=SEEK(XInvNo  +XVENDCODE)
SCAN REST  WHILE CINVNO + CVENDCODE = XInvNo  +XVENDCODE FOR cApdActID = "D" AND EMPTY(cApdStat) AND cApdTrTyp = "I" AND nApdAmnt > 0  
  lcVendCode = cVendCode
  lcInvNo    = cInvNo
  
  IF cVendCode <>  XVendCode OR cInvNo <> XInvNo  
    LOOP
  ENDIF
  
  IF SEEK(cVendCode,'ApVendor') AND !EMPTY(ApVendor.cVenInvTyp) AND !EMPTY(ApVendor.cVenSupTyp)
    * If this vendor does not have the supplies types, so no need to update the invoice detail file.
    IF !SEEK(cVendCode + cInvNo,'ApvInvDt') 
      SCAN REST WHILE cInvNo + cVendCode = lcInvNo + lcVendCode
        IF cApdActID = "D" AND EMPTY(cApdStat) AND cApdTrTyp = "I" AND nApdAmnt > 0
          WAIT WINDOW "Copying " + ALLTRIM(cInvNo) + " for vendor : " + ALLTRIM(cVendCode) NOWAIT
   
          STORE 0 TO m.nApPrice , m.nApAprPric , m.nApAprAmnt , m.nApAmnt
          STORE 1 TO m.nApTotQty , m.nAptAprQty
          STORE 'G' TO m.cImTyp
          STORE 'General expenses' TO m.cApviDes 
          STORE '' TO m.cApInvNo 

          SCATTER MEMVAR MEMO
          STORE m.nApdAmnt TO m.nApPrice , m.nApAprPric , m.nApAprAmnt , m.nApAmnt
          m.cApInvNo = m.cInvNo
          m.cApvilNo = PADL(ALLTRIM(STR(nApdlinno)) ,4, " ")
          INSERT INTO ApvInvDt FROM MEMVAR
          
          IF SEEK(lcVendCode+lcInvNo ,'ApInvHdr') AND ApInvHdr.nLastViLno < VAL(ApvInvDt.capvilno) 
            Replace ApInvHdr.nLastViLno WITH VAL(ApvInvDt.capvilno) 
          ENDIF
          
        ENDIF  
      ENDSCAN
      IF !BOF()
        SKIP -1
      ENDIF
    ENDIF  
  ENDIF  
ENDSCAN
WAIT CLEAR
*-- Scan loop around ApDist file [End]

IF USED('ApInvHdr')
  USE IN ApInvHdr
ENDIF

IF USED('PosHdr')
  USE IN PosHdr
ENDIF

IF USED('POFHDR')
  USE IN POFHDR   
ENDIF


IF USED('ApDist')
  USE IN ApDist
ENDIF

IF USED('ApvInvDt')
  USE IN ApvInvDt
ENDIF

IF USED('ApVendor')
 USE IN ApVendor
ENDIF
*=gfModalGen("TRM000000B00000","DIALOG",'','','Finished adding Payable invoice lines.')  
WAIT WINDOW  'Finished adding Payable invoice lines.'   nowait


SET DEFAULT TO &lcPath