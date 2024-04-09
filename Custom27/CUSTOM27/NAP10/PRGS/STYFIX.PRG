*-- Fix program done For Nap to recompute the stock value, previous stock and previous stock value.
*-- Done by Abdou Elgendi on 12/04/2001 under supervision of Ashraf Sherif
*-- This program should be copied to the data directory(aria27\dbfs\01) and ran from it.

SELECT 0
USE styinvjl.dbf SHARED order 3
SELECT STYINVJL
lcStyleWar = ''
STORE .T. TO llFirst
STORE 0 TO  lnSTotStk , lnPrvsVal 
SCAN 
  WAIT WINDOW STYLE NOWAIT
  IF lcStylewar # Style+CwareCode
    STORE 0 TO  lnSTotStk , lnPrvsVal
    lcStyleWar = Style+CwareCode
  ENDIF
 
  DO CASE
  CASE ctrtype = '2' 
    IF llFirst
      REPLACE nStkVal  WITH ntotstk * ncost   ,;
              nPrvsQty WITH ntotstk * -1      ,;
              nPrvsval WITH nPrvsQty * nCost
      STORE 0 TO  lnSTotStk , lnPrvsVal
      llFirst = .F.
    ELSE
      REPLACE nStkVal  WITH ntotstk * ncost ,;
              nPrvsQty WITH lnSTotStk       ,;
              nPrvsval WITH lnPrvsVal
           
      lnSTotStk  = lnSTotStk + nTotStk
      lnPrvsVal  = lnPrvsVal + nStkVal
      llFirst = .T.
    ENDIF
  OTHERWISE
    REPLACE nStkVal  WITH ntotstk * ncost  ,;
            nPrvsQty WITH lnSTotStk        ,;
            nPrvsval WITH lnPrvsVal
           
     lnSTotStk  = lnSTotStk + nTotStk
     lnPrvsVal  = lnPrvsVal + nStkVal
     llFirst = .T.
  ENDCASE
ENDSCAN
WAIT CLEAR
USE