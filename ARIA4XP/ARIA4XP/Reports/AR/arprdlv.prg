*:***************************************************************************
*: Program file    : ARPRDLV.PRG
*: Program desc.   : Proof Of Delivery Report                     
*: Module          : Accounts receivable (AR)
*: Developer       : Tarek Mohamed Ibrahim 
*: Tracking Number : E302929.exe [T20101207.0006] 
*:                 : E302930.122  a27 menu tracking entry
*: Date            : 07/08/2011 
*:***************************************************************************

IF loOGScroll.llOGFltCh
  *- Create temp files
  =lfCrTemp()
  
  *- Collect Data
  =lfCollData()
  
ENDIF 

SELECT &lcWorkFile
DO gfDispRe WITH 'ARPRDLV'

RETURN 
*!*************************************************************
*! Name      : lfSROrder
*! Developer : Tarek Mohammed Ibrahim
*! Date      : 07/08/2011
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Note      : S symbol is [S,Set],R is Reset.
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

DO CASE
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfInvSet
*! Developer : Tarek Mohammed Ibrahim
*! Date      : 07/08/2011
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Note      : S symbol is [S,Set],R is Reset.
*!*************************************************************
FUNCTION lfInvSet
PARAMETERS lcParm

DO CASE
ENDCASE
*-- end of lfsrAcc.


*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Tarek Mohammed Ibrahim
*! Date      : 07/08/2011
*! Purpose   : Rise change account flag, in range browse screen.
*!*************************************************************
*! Note      : S symbol is [S,Set],R is Reset.
*!*************************************************************
FUNCTION lfsrAcc
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    llChAcc = .T.
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*-- end of lfsrAcc.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Tarek Mohammed Ibrahim
*! Date      : 07/08/2011
*! Purpose   : Option Grid When function
*!*************************************************************
FUNCTION lfwRepWhen

************************************************************************************************
*
*   FUNCTION lfCrTemp
*
************************************************************************************************
FUNCTION lfCrTemp

IF USED(lcWorkFile)
  SELECT &lcWorkFile
  ZAP
ELSE
  DIMENSION laStru[6,4]
  
  lnI = 0
  
  lnI = lnI + 1
  laStru[lnI,1] = 'INVOICE'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 6
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  laStru[lnI,1] = 'INVDATE'
  laStru[lnI,2] = 'D'
  laStru[lnI,3] = 8
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  laStru[lnI,1] = 'ORDER'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 6
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  laStru[lnI,1] = 'CUSTPO'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 15
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  laStru[lnI,1] = 'CUPSDESC'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0
  
  lnI = lnI + 1
  laStru[lnI,1] = 'CCARTRCKNO'
  laStru[lnI,2] = 'C'
  laStru[lnI,3] = 30
  laStru[lnI,4] = 0
  
  CREATE TABLE (oAriaApplication.WorkDir+lcWorkFile) FROM ARRAY laStru
  INDEX ON INVOICE TAG INVOICE
ENDIF


*!*************************************************************
*! Name      : lfCollData
*! Developer : Tarek Mohammed Ibrahim
*! Date      : 07/08/2011
*! Purpose   : Collect Data
*!*************************************************************
FUNCTION lfCollData

*- also to check the CUPS type, we have only the SHIPVIA value
*- a need to check if the CUPS related field of the SHIPVIA of the selected order is in the selected list of CUPS in the filter


IF 'INVHDR.ORDER' $ lcRpExp
  *- in case if the invoice is consolidated then this would requires check the consinvh file 
  lcRpExp = STRTRAN(lcRpExp,'INVHDR.ORDER','IIF(!EMPTY(INVHDR.ORDER),INVHDR.ORDER,CONSINVH.ORDER)')
  SET ORDER TO CONSINVH IN CONSINVH   && INVOICE+STORE+ORDER+PIKTKT
  SELECT INVHDR
  SET RELATION TO INVOICE INTO CONSINVH
  SET SKIP TO CONSINVH
ENDIF

lnUPSPos = AT( 'INLIST(CUPS' , lcRpExp )
IF lnUPSPos > 0  
  lcUpsCrit = SUBSTR(lcRpExp,lnUPSPos+11)
  lcRpExp = SUBSTR(lcRpExp,1,lnUPSPos-1)+'.T.'
ENDIF


SELECT INVHDR
LOCATE 
SCAN FOR &lcRpExp
  SCATTER MEMVAR MEMO
  lcUPS = lfGetUPS(INVHDR.SHIPVIA)
  IF lnUPSPos > 0    
    IF !lcUPS $ lcUpsCrit
      LOOP
    ENDIF    
  ENDIF
  m.cUPSDESC = gfCodDes(lcUPS,'CUPS ')
  m.ORDER = IIF(!EMPTY(INVHDR.ORDER),INVHDR.ORDER,CONSINVH.ORDER)
  IF !SEEK(m.INVOICE,lcWorkFile)  
    INSERT INTO &lcWorkFile FROM MEMVAR
  ELSE
    IF m.ORDER <> &lcWorkFile..ORDER
      SELECT &lcWorkFile
      REPLACE ORDER WITH ''
    ENDIF
  ENDIF
ENDSCAN
*- End of FUNCTION lfCollData



*!*************************************************************
*! Name      : lfGetUPS
*! Developer : Tarek Mohammed Ibrahim
*! Date      : 07/08/2011
*! Purpose   : Get Carrier type
*!*************************************************************
FUNCTION lfGetUPS
PARAMETERS lcShipVia
LOCAL lnSlct
lnSlct = SELECT(0)

IF !USED(lcShpViaCurs)
  CREATE CURSOR &lcShpViaCurs (SHIPVIA C(6),CUPS C(6))
  INDEX ON SHIPVIA TAG SHIPVIA
ENDIF
IF !SEEK(lcShipVia,lcShpViaCurs)
  lcCUPS = ''
  DECLARE laShpUPS[1,2]
  laShpUPS[1,1] = 'CUPS'
  laShpUPS[1,2] = 'lcCUPS'
  =gfRltFld(INVHDR.SHIPVIA,@laShpUPS,'SHIPVIA')
  lcRet = PADR(lcCUPS,6)
  SELECT &lcShpViaCurs
  APPEND BLANK
  REPLACE SHIPVIA WITH lcShipVia ;
          CUPS    WITH lcCUPS
ELSE
  lcRet = &lcShpViaCurs..CUPS
ENDIF
SELECT (lnSlct)
RETURN lcRet