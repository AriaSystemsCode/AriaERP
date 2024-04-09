*:***************************************************************************
*: Program file  : ARPINVRU
*: Program desc. : Custom Pick Ticket Form For JO JOE (JOJ10)
*: For Report    : ....
*: System        : Aria 4XP
*: Module        : AL
*: Developer     : Mariam Mazhar  (MMT)
*: Date          : 06/30/2009
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:MOdifcations :
*:***************************************************************************
*: This Report Program is due to C201181(Aria4),C201180(Aria27)... [T20090416.0007]
*----------------------- Report Code Begin -----------------------------
STORE '' TO lnToTPktk
*!*************************************************************
*! Name      : lfGetLastLine
*! Developer : Mariam Mazhar  (MMT)
*! Date      : 06/30/2009
*! Purpose   : Function to Get TOTal Style Qty in Piktkt
*!*************************************************************
FUNCTION lfGetLastLine
PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.
llEndGroup = .F.
SELECT (lcTmpOrdL)
lcPitkt = PIKTKT
lcStyle = SUBSTR(Style,1,lnLenthM1)
lnThRec = RECNO()    && Save Current record #.
lnToTPktk= 0
LOCATE 
SCAN FOR Piktkt = lcPitkt  AND SUBSTR(Style,1,lnLenthM1) = lcStyle 
  lnToTPktk= lnToTPktk+ TotPik
ENDSCAN 

IF BETWEEN(lnThRec,1,RECCOUNT(lcTmpOrdL))
  GO lnThRec IN (lcTmpOrdL)    && Restore Record #
ELSE
  GO TOP IN (lcTmpOrdL) && Restore Record #
ENDIF
SELECT (lcThAlias)            && Restore Alias.
RETURN ''